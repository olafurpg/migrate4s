package scalafix
package patch

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.tokens.Token
import scalafix.syntax._
import scalafix.patch.TokenPatch.Add
import scalafix.patch.TokenPatch.Remove
import scalafix.patch.TreePatch.ImportPatch
import scalafix.diff.DiffUtils
import scalafix.internal.patch.ImportPatchOps
import scalafix.internal.patch.ReplaceSymbolOps
import scalafix.internal.util.Failure
import scalafix.internal.util.TokenOps
import scalafix.lint.LintMessage
import scalafix.patch.TreePatch.ReplaceSymbol
import org.scalameta.logger
import scalafix.internal.config.ScalafixMetaconfigReaders
import scalafix.internal.util.SymbolOps.Root
import scalafix.patch.TreePatch.AddGlobalImport
import scalafix.patch.TreePatch.RemoveGlobalImport
import scalafix.v1.SemanticDoc
import scalafix.v1.Sym

/** A data structure that can produce a .patch file.
  *
  * The best way to build a Patch is with a RuleCtx inside a Rule.
  * For example, `Rule.syntactic(ctx => ctx.addLeft(ctx.tree.tokens.head): Patch)`
  *
  * Patches can be composed with Patch.+ and Patch.++. A Seq[Patch] can be combined
  * into a single patch with `Seq[Patch](...).asPatch` with `import scalafix._`.
  *
  * Patches are split into low-level token patches and high-level tree patches.
  * A token patch works on scala.meta.Token and provides surgical precision over
  * how details like formatting are managed by the rule.
  *
  * NOTE: Patch current only works for a single file, but it may be possible
  * to add support in the future for combining patches for different files
  * with Patch + Patch.
  */
sealed abstract class Patch {
  // NOTE: potential bottle-neck, this might be very slow for large
  // patches. We might want to group related patches and enforce some ordering.
  def +(other: Patch): Patch =
    if (this eq other) this
    else if (isEmpty) other
    else if (other.isEmpty) this
    else Concat(this, other)
  def +(other: Option[Patch]): Patch =
    this.+(other.getOrElse(Patch.empty))
  def ++(other: Iterable[Patch]): Patch = other.foldLeft(this)(_ + _)
  def isEmpty: Boolean = this == EmptyPatch
  def nonEmpty: Boolean = !isEmpty

  /** Skip this entire patch if a part of it is disabled with // scalafix:off */
  def atomic: Patch = AtomicPatch(this)
}

//////////////////////////////
// Low-level patches
//////////////////////////////
trait LowLevelPatch

abstract class TokenPatch(val tok: Token, val newTok: String)
    extends Patch
    with LowLevelPatch {
  override def toString: String =
    if (newTok.isEmpty)
      s"TokenPatch.Remove(${logger.revealWhitespace(tok.structure)})"
    else
      s"TokenPatch.${this.getClass.getSimpleName}(${logger.revealWhitespace(
        tok.syntax)}, ${tok.structure}, $newTok)"
}
private[scalafix] object TokenPatch {
  case class Remove(override val tok: Token) extends TokenPatch(tok, "")
  case class Add(
      override val tok: Token,
      addLeft: String,
      addRight: String,
      keepTok: Boolean = true)
      extends TokenPatch(
        tok,
        s"""$addLeft${if (keepTok) tok else ""}$addRight""")

}

//////////////////////////////
// High-level patches
//////////////////////////////
abstract class TreePatch extends Patch
private[scalafix] object TreePatch {
  abstract class ImportPatch extends TreePatch
  case class RemoveGlobalImport(symbol: Sym) extends ImportPatch
  case class RemoveImportee(importee: Importee) extends ImportPatch
  case class AddGlobalImport(importer: Importer) extends ImportPatch
  case class AddGlobalSymbol(symbol: Sym) extends ImportPatch
  case class ReplaceSymbol(from: Sym, to: Sym) extends TreePatch {
    def msymbols: (Symbol, Symbol) = (from.msymbol, to.msymbol)
  }
}

// implementation detail
private[scalafix] case class AtomicPatch(underlying: Patch) extends Patch
private[scalafix] case class LintPatch(message: LintMessage) extends Patch
private[scalafix] case class Concat(a: Patch, b: Patch) extends Patch
private[scalafix] case object EmptyPatch extends Patch with LowLevelPatch

object Patch extends PatchOps {
  def removeImportee(importee: Importee): Patch =
    TreePatch.RemoveImportee(importee)
  def addGlobalImport(importer: Importer): Patch =
    AddGlobalImport(importer)
  def replaceToken(token: Token, toReplace: String): Patch =
    Add(token, "", toReplace, keepTok = false)
  def removeTokens(tokens: Tokens): Patch =
    doRemoveTokens(tokens)
  def removeTokens(tokens: Iterable[Token]): Patch =
    doRemoveTokens(tokens)
  private def doRemoveTokens(tokens: Iterable[Token]): Patch =
    tokens.foldLeft(Patch.empty)(_ + TokenPatch.Remove(_))
  def removeToken(token: Token): Patch =
    Add(token, "", "", keepTok = false)
  def replaceTree(from: Tree, to: String): Patch = {
    val tokens = from.tokens
    removeTokens(tokens) + tokens.headOption.map(x => addRight(x, to))
  }
  def addRight(tok: Token, toAdd: String): Patch =
    Add(tok, "", toAdd)
  def addRight(tree: Tree, toAdd: String): Patch =
    tree.tokens.lastOption.fold(Patch.empty)(addRight(_, toAdd))
  def addLeft(tok: Token, toAdd: String): Patch =
    Add(tok, toAdd, "")
  def addLeft(tree: Tree, toAdd: String): Patch =
    tree.tokens.headOption.fold(Patch.empty)(addLeft(_, toAdd))

  // Semantic patch ops.
  def removeGlobalImport(symbol: Symbol)(implicit doc: SemanticDoc): Patch =
    RemoveGlobalImport(symbol.sym)
  def addGlobalImport(symbol: Symbol)(implicit doc: SemanticDoc): Patch =
    TreePatch.AddGlobalSymbol(symbol.sym)
  def replaceSymbol(fromSymbol: Symbol.Global, toSymbol: Symbol.Global)(
      implicit doc: SemanticDoc): Patch =
    TreePatch.ReplaceSymbol(fromSymbol.sym, toSymbol.sym)
  def replaceSymbols(toReplace: (String, String)*)(
      implicit doc: SemanticDoc): Patch =
    toReplace.foldLeft(Patch.empty) {
      case (a, (from, to)) =>
        val (fromSymbol, toSymbol) =
          ScalafixMetaconfigReaders.parseReplaceSymbol(from, to).get
        a + ReplaceSymbol(fromSymbol, toSymbol)
    }
  def replaceSymbols(toReplace: scala.collection.Seq[(String, String)])(
      implicit noop: DummyImplicit,
      doc: SemanticDoc): Patch =
    replaceSymbols(toReplace: _*)
  def renameSymbol(fromSymbol: Symbol.Global, toName: String)(
      implicit doc: SemanticDoc): Patch =
    TreePatch.ReplaceSymbol(fromSymbol.sym, Root(Signature.Term(toName)).sym)
  def lint(msg: LintMessage): Patch =
    LintPatch(msg)

  /** Combine a sequence of patches into a single patch */
  def fromIterable(seq: Iterable[Patch]): Patch =
    seq.foldLeft(empty)(_ + _)

  /** A patch that does no diff/rule */
  val empty: Patch = EmptyPatch

  def merge(a: TokenPatch, b: TokenPatch): TokenPatch = (a, b) match {
    case (add1: Add, add2: Add) =>
      Add(
        add1.tok,
        add1.addLeft + add2.addLeft,
        add1.addRight + add2.addRight,
        add1.keepTok && add2.keepTok)
    case (_: Remove, add: Add) => add.copy(keepTok = false)
    case (add: Add, _: Remove) => add.copy(keepTok = false)
    case (rem: Remove, rem2: Remove) => rem
    case _ => throw Failure.TokenPatchMergeError(a, b)
  }

  // Patch.apply and Patch.lintMessages package private. Feel free to use them
  // for your application, but please ask on the Gitter channel to see if we
  // can expose a better api for your use case.
  private[scalafix] def apply(
      patchesByName: Map[scalafix.rule.RuleName, scalafix.Patch],
      ctx: RuleCtx,
      index: Option[SemanticdbIndex]
  ): (String, List[LintMessage]) = {
    val idx = index.getOrElse(SemanticdbIndex.empty)
    val (patch, lints) = ctx.filter(patchesByName, idx)
    val patches = treePatchApply(patch)(ctx, idx)
    (tokenPatchApply(ctx, patches), lints)
  }

  private[scalafix] def apply(
      patchesByName: Map[scalafix.rule.RuleName, scalafix.Patch],
      doc: SemanticDoc
  ): (String, List[LintMessage]) = {
    val (patch, lints) = doc.doc.escapeHatch.filter(patchesByName, doc)
    val patches = treePatchApply(patch, doc)
    (tokenPatchApply(doc, patches), lints)
  }

  def treePatchApply(patch: Patch, doc: SemanticDoc): Iterable[TokenPatch] = {
    val base = underlying(patch)
    val moveSymbol = underlying(
      ReplaceSymbolOps.naiveMoveSymbolPatch(base.collect {
        case m: ReplaceSymbol => m
      })(doc))
    val patches = base.filterNot(_.isInstanceOf[ReplaceSymbol]) ++ moveSymbol
    val tokenPatches = patches.collect { case e: TokenPatch => e }
    val importPatches = patches.collect { case e: ImportPatch => e }
    val importTokenPatches = {
      val result = ImportPatchOps.superNaiveImportPatchToTokenPatchConverter(
        doc,
        importPatches
      )

      Patch
        .underlying(result.asPatch)
        .collect {
          case x: TokenPatch => x
          case els =>
            throw Failure.InvariantFailedException(
              s"Expected TokenPatch, got $els")
        }
    }
    importTokenPatches ++ tokenPatches
  }

  def treePatchApply(patch: Patch)(
      implicit ctx: RuleCtx,
      index: SemanticdbIndex): Iterable[TokenPatch] = {
    ???
  }

  private def tokenPatchApply(
      doc: SemanticDoc,
      patches: Iterable[TokenPatch]): String = {
    val patchMap = patches
      .groupBy(x => TokenOps.hash(x.tok))
      .mapValues(_.reduce(merge).newTok)
    doc.tokens.toIterator
      .map(tok => patchMap.getOrElse(TokenOps.hash(tok), tok.syntax))
      .mkString
  }

  private def tokenPatchApply(
      ctx: RuleCtx,
      patches: Iterable[TokenPatch]): String = {
    ???

  }

  private def underlying(patch: Patch): Seq[Patch] = {
    val builder = Seq.newBuilder[Patch]
    foreach(patch) {
      case _: LintPatch =>
        ()
      case els =>
        builder += els
    }
    builder.result()
  }

  private[scalafix] def isOnlyLintMessages(patch: Patch): Boolean = {
    // TODO(olafur): foreach should really return Stream[Patch] for early termination.
    var onlyLint = true
    var hasLintMessage = false
    foreach(patch) {
      case _: LintPatch => hasLintMessage = true
      case _ => onlyLint = false
    }
    patch.isEmpty || hasLintMessage && onlyLint
  }

  private def foreach(patch: Patch)(f: Patch => Unit): Unit = {
    def loop(patch: Patch): Unit = patch match {
      case Concat(a, b) =>
        loop(a)
        loop(b)
      case EmptyPatch =>
        ()
      case AtomicPatch(underlying) =>
        loop(underlying)
      case els =>
        f(els)
    }
    loop(patch)
  }

  def unifiedDiff(original: Input, revised: Input): String = {
    DiffUtils.unifiedDiff(
      original.label,
      revised.label,
      new String(original.chars).lines.toList,
      new String(revised.chars).lines.toList,
      3)
  }
}
