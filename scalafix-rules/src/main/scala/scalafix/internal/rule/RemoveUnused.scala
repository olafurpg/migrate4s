package scalafix.internal.rule

import metaconfig.Configured
import scalafix.v1._

import scala.collection.mutable
import scala.meta._
import scala.meta.transversers.SimpleTraverser

class RemoveUnused(config: RemoveUnusedConfig)
    extends SemanticRule(
      RuleName("RemoveUnused")
        .withDeprecatedName(
          "RemoveUnusedImports",
          "Use RemoveUnused instead",
          "0.6.0")
        .withDeprecatedName(
          "RemoveUnusedTerms",
          "Use RemoveUnused instead",
          "0.6.0")
    ) {
  // default constructor for reflective classloading
  def this() = this(RemoveUnusedConfig.default)

  override def description: String =
    "Removes unused imports and terms that reported by the compiler under -Ywarn-unused"
  override def isRewrite: Boolean = true

  override def withConfiguration(config: Configuration): Configured[Rule] =
    if (!config.scalacOptions.exists(_.startsWith("-Ywarn-unused"))) {
      Configured.error(
        """|The Scala compiler option "-Ywarn-unused" is required to use RemoveUnused.
           |To fix this problem, update your build to use at least one Scala compiler
           |option that starts with "-Ywarn-unused"""".stripMargin
      )
    } else {
      config.conf
        .getOrElse("RemoveUnused")(this.config)
        .map(new RemoveUnused(_))
    }

  override def fix(implicit doc: SemanticDocument): Patch = {
    val unusedTermsPosition = mutable.Set.empty[Position]
    val unusedImportsPosition = mutable.Set.empty[Position]

    doc.diagnostics.foreach { diagnostic =>
      if (config.imports && diagnostic.message == "Unused import") {
        unusedImportsPosition += diagnostic.position
      } else if (config.privates &&
        diagnostic.message.startsWith("private") &&
        diagnostic.message.endsWith("is never used")) {
        unusedTermsPosition += diagnostic.position
      } else if (config.locals &&
        diagnostic.message.startsWith("local") &&
        diagnostic.message.endsWith("is never used")) {
        unusedTermsPosition += diagnostic.position
      }
    }

    if (unusedImportsPosition.isEmpty && unusedTermsPosition.isEmpty) {
      // Optimization: don't traverse if there are no diagnostics to act on.
      Patch.empty
    } else {
      fix(doc.tree, unusedTermsPosition.toSet, unusedImportsPosition.toSet)
    }
  }

  private def fix(
      tree: Tree,
      unusedTermsPosition: Set[Position],
      unusedImportsPosition: Set[Position])(
      implicit doc: SemanticDocument): Patch = {
    val patches = mutable.ListBuffer.empty[Patch]
    var wildcardImportsInScope = Set.empty[Symbol]
    var unusedRenameImports = Map.empty[Importee.Rename, Symbol]
    def isUnusedImport(i: Importee) = unusedImportsPosition(importPosition(i))

    object traverser extends SimpleTraverser {
      override def apply(current: Tree): Unit = {
        current match {
          case Importer(ref, importees) =>
            importees.foreach {
              case i: Importee.Wildcard if !isUnusedImport(i) =>
                wildcardImportsInScope += ref.symbol
              case i: Importee.Rename if isUnusedImport(i) =>
                unusedRenameImports += (i -> ref.symbol)
              case i: Importee if isUnusedImport(i) =>
                patches += Patch.removeImportee(i).atomic
              case _ =>
            }
            super.apply(current)

          case i: Defn if unusedTermsPosition.contains(i.pos) =>
            patches += defnTokensToRemove(i)
            super.apply(current)

          case i @ Defn.Val(_, List(pat), _, _)
              if unusedTermsPosition.exists(_.start == pat.pos.start) =>
            patches += defnTokensToRemove(i)
            super.apply(current)

          case i @ Defn.Var(_, List(pat), _, _)
              if unusedTermsPosition.exists(_.start == pat.pos.start) =>
            patches += defnTokensToRemove(i)
            super.apply(current)

          case EnclosingScope() =>
            val prevWildcards = wildcardImportsInScope
            val prevUnusedRenames = unusedRenameImports
            super.apply(current)
            val (potentiallyShadowing, notShadowing) =
              unusedRenameImports.partition {
                case (_, refSymbol) =>
                  wildcardImportsInScope.contains(refSymbol)
              }
            patches ++= potentiallyShadowing.keys.map {
              // Unimport the identifier instead of removing the importee since
              // unused renamed may still impact compilation by shadowing an identifier.
              // See https://github.com/scalacenter/scalafix/issues/614
              case Importee.Rename(_, to) => Patch.replaceTree(to, "_").atomic
            }
            val (unusedRenamesInCurrentScope, remaining) =
              notShadowing.partition {
                case (i, _) => !prevUnusedRenames.contains(i)
              }
            patches ++= unusedRenamesInCurrentScope.keys
              .map(Patch.removeImportee(_).atomic)
            unusedRenameImports = remaining
            wildcardImportsInScope = prevWildcards

          case _ => super.apply(current)
        }
      }
    }
    traverser(tree)
    patches.asPatch
  }

  private def importPosition(importee: Importee): Position = importee match {
    case Importee.Rename(from, _) => from.pos
    case _ => importee.pos
  }

  // Given defn "val x = 2", returns "val x = ".
  private def binderTokens(defn: Defn, body: Term): Tokens = {
    val startDef = defn.tokens.start
    val startBody = body.tokens.start
    defn.tokens.take(startBody - startDef)
  }

  private def defnTokensToRemove(defn: Defn): Patch = {
    val maybeTokens = defn match {
      case i @ Defn.Val(_, _, _, Lit(_)) => Some(i.tokens)
      case i @ Defn.Val(_, _, _, rhs) => Some(binderTokens(i, rhs))
      case i @ Defn.Var(_, _, _, Some(Lit(_))) => Some(i.tokens)
      case i @ Defn.Var(_, _, _, rhs) => rhs.map(binderTokens(i, _))
      case i: Defn.Def => Some(i.tokens)
      case _ => None
    }
    maybeTokens.map(Patch.removeTokens).asPatch.atomic
  }

  private case object EnclosingScope {
    def unapply(tree: Tree): Boolean = tree match {
      case _: Source | _: Pkg | _: Template | _: Term.Block |
          _: Ctor.Secondary =>
        true
      case _ => false
    }
  }
}
