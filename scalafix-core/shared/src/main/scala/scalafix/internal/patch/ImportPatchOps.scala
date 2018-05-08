package scalafix
package internal.patch

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.meta._
import scalafix.internal.util.SymbolOps
import scalafix.patch.Patch
import scalafix.patch.TokenPatch
import scalafix.patch.TreePatch
import scalafix.patch.TreePatch.ImportPatch
import scalafix.rule.RuleCtx
import scalafix.syntax._
import scalafix.util.Newline
import scalafix.v1.SemanticDoc
import scalafix.v1.Sym

object ImportPatchOps {
  object symbols {
    val Scala: Sym = Sym("_root_.scala.")
    val Predef: Sym = Sym("_root_.scala.Predef.")
    val Java: Sym = Sym("_root_.java.lang.")
    val Immutable: Sym = Sym("_root_.scala.collection.immutable.")
  }

  def isPredef(symbol: Sym): Boolean = {
    import symbols._
    val owner = symbol.owner
    // TODO this is a hack!
    owner == Scala ||
    owner == Predef ||
    owner == Java
  }

  private def fallbackToken(ctx: SemanticDoc): Token = {
    def loop(tree: Tree): Token = tree match {
      case Source((stat: Pkg) :: _) => loop(stat)
      case Source(_) => tree.tokens.head
      case Pkg(_, stat :: _) => loop(stat)
      case els => ctx.tokenList.prev(ctx.tokenList.prev(els.tokens.head))
    }
    loop(ctx.tree)
  }
  private def extractImports(stats: Seq[Stat]): Seq[Import] = {
    stats
      .takeWhile(_.is[Import])
      .collect { case i: Import => i }
  }

  @tailrec private final def getLastTopLevelPkg(potPkg: Stat): Stat =
    potPkg match {
      case Pkg(_, head +: Nil) => getLastTopLevelPkg(head)
      case Pkg(_, head +: _) => head
      case _ => potPkg
    }

  @tailrec private final def getGlobalImports(ast: Tree): Seq[Import] =
    ast match {
      case Pkg(_, Seq(pkg: Pkg)) => getGlobalImports(pkg)
      case Source(Seq(pkg: Pkg)) => getGlobalImports(pkg)
      case Pkg(_, stats) => extractImports(stats)
      case Source(stats) => extractImports(stats)
      case _ => Nil
    }

  // NOTE(olafur): This method is the simplest/dummest thing I can think of
  private[scalafix] def superNaiveImportPatchToTokenPatchConverter(
      ctx: RuleCtx,
      importPatches: Seq[ImportPatch])(
      implicit index: SemanticdbIndex): Iterable[Patch] = {
    ???
  }

  private[scalafix] def superNaiveImportPatchToTokenPatchConverter(
      doc: SemanticDoc,
      importPatches: Seq[ImportPatch]): Iterable[Patch] = {
    implicit val semanticDoc = doc
    val allImports = doc.tree.collect { case i: Import => i }
    val allImporters = allImports.flatMap(_.importers)
    lazy val allImportersSyntax = allImporters.map(_.syntax)
    val allImportees = allImporters.flatMap(_.importees)
    val allNamedImports = allImportees.collect {
      case Importee.Name(n) if !n.sym.isNone =>
        n.sym
      // TODO(olafur) handle rename.
    }
    val allImporteeSymbols =
      allImportees.map(importee => importee.sym.normalized -> importee)
    val globalImports = getGlobalImports(doc.tree)
    val editToken: Token = {
      if (globalImports.isEmpty) fallbackToken(doc)
      else globalImports.last.tokens.last
    }
    val isRemovedImportee = mutable.LinkedHashSet.empty[Importee]
    importPatches.foreach {
      case TreePatch.RemoveGlobalImport(sym) =>
        allImporteeSymbols
          .withFilter(_._1 == sym.normalized)
          .foreach { case (_, x) => isRemovedImportee += x }
      case x: TreePatch.RemoveImportee => isRemovedImportee += x.importee
      case _ =>
    }
    val importersToAdd = {
      val isAlreadyImported = mutable.Set.empty[Sym]
      for { // register global imports
        import_ <- globalImports
        importer <- import_.importers
        importee <- importer.importees
        symbol = importee.sym
        if !symbol.isNone
        underlying <- SymbolOps.underlyingSymbols(Symbol(symbol.value))
      } {
        isAlreadyImported += underlying.sym
      }
      importPatches.flatMap {
        case TreePatch.AddGlobalSymbol(symbol)
            if !allNamedImports.contains(symbol) &&
              !isAlreadyImported(symbol) &&
              !isPredef(symbol) =>
          isAlreadyImported += symbol
          SymbolOps.toImporter(symbol.msymbol).toList
        case TreePatch.AddGlobalImport(importer)
            // best effort deduplication for syntactic addGlobalImport(Importer)
            if !allImportersSyntax.contains(importer.syntax) =>
          importer :: Nil
        case _ => Nil
      }
    }
    val grouped: Seq[Importer] =
      if (doc.doc.config.groupImportsByPrefix)
        importersToAdd
          .groupBy(_.ref.syntax)
          .map {
            case (_, is) =>
              Importer(
                is.head.ref,
                is.flatMap(_.importees)
                  .sortBy({
                    case Importee.Name(n) => n.value
                    case Importee.Rename(n, _) => n.value
                    case Importee.Unimport(n) => n.value
                    case Importee.Wildcard() => '\uFFFF'.toString
                  })
                  .toList
              )
          }
          .toList
      else importersToAdd
    val extraPatches =
      grouped
        .sortBy(_.ref.syntax)
        .map(is => doc.addRight(editToken, s"\nimport ${is.syntax}"))
    val isRemovedImporter =
      allImporters.toIterator
        .filter(_.importees.forall(isRemovedImportee))
        .toSet
    def removeSpaces(tokens: scala.Seq[Token]): Patch =
      tokens
        .takeWhile {
          case Token.Space() => true
          case _ => false
        }
        .map(Patch.removeToken)
        .asPatch
    val curlyBraceRemoves = allImporters.map { importer =>
      val keptImportees = importer.importees.filterNot(isRemovedImportee)
      val hasRemovedImportee = importer.importees.exists(isRemovedImportee)
      keptImportees match {
        case (Importee.Wildcard() | Importee.Name(_)) +: Nil
            if hasRemovedImportee =>
          importer.tokens.collectFirst {
            case open @ Token.LeftBrace() =>
              doc.matchingParens
                .close(open)
                .map { close =>
                  Patch.removeToken(open) +
                    removeSpaces(doc.tokenList.trailing(open)) +
                    Patch.removeToken(close) +
                    removeSpaces(doc.tokenList.leading(close))
                }
                .asPatch
          }.asPatch
        case _ => Patch.empty
      }
    }
    // NOTE: keeps track of which comma is removed by which tree to prevent the
    // same comma being removed twice.
    val isRemovedComma = mutable.Map.empty[Token.Comma, Tree]
    val isRemovedImport =
      allImports.filter(_.importers.forall(isRemovedImporter))
    def remove(toRemove: Tree) = {
      val tokens = doc.toks(toRemove)
      def removeFirstComma(lst: Iterable[Token]): Iterable[Patch] = {
        lst
          .takeWhile {
            case lf @ Token.LF() if doc.tokenList.prev(lf).is[Token.Comma] =>
              true
            case Token.Space() => true
            case comma @ Token.Comma() =>
              if (!isRemovedComma.contains(comma)) {
                isRemovedComma(comma) = toRemove
              }
              true
            case _ => false
          }
          .map(Patch.removeToken)
      }
      val leadingComma =
        removeFirstComma(doc.tokenList.leading(tokens.head))
      val hadLeadingComma = leadingComma.exists {
        case TokenPatch.Add(comma: Token.Comma, _, _, keepTok @ false) =>
          isRemovedComma.get(comma).contains(toRemove)
        case _ => false
      }
      val trailingComma =
        if (hadLeadingComma) List(Patch.empty)
        else removeFirstComma(doc.tokenList.trailing(tokens.last))
      Patch.removeTokens(tokens) ++ trailingComma ++ leadingComma
    }

    val leadingNewlines = isRemovedImport.map { i =>
      var newline = false
      doc.tokenList
        .leading(doc.toks(i).head)
        .takeWhile(x =>
          !newline && {
            x.is[Token.Space] || {
              val isNewline = x.is[Newline]
              if (isNewline) newline = true
              isNewline
            }
        })
        .map(tok => Patch.removeToken(tok))
        .asPatch
    }

    leadingNewlines ++
      curlyBraceRemoves ++
      extraPatches ++
      (isRemovedImportee ++
        isRemovedImporter ++
        isRemovedImport).map(remove)
  }

}
