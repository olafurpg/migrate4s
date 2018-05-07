package scalafix.internal.rule

import org.langmeta.semanticdb.Signature
import scala.meta.Importee
import scala.meta.Importer
import scala.meta.Symbol
import scala.meta.Token
import scala.meta.Tree
import scala.meta.tokens.Tokens
import scalafix.LintMessage
import scalafix.Patch
import scalafix.internal.config.ScalafixMetaconfigReaders
import scalafix.internal.util.SymbolOps.Root
import scalafix.patch.LintPatch
import scalafix.patch.PatchOps
import scalafix.patch.TokenPatch
import scalafix.patch.TokenPatch.Add
import scalafix.patch.TreePatch
import scalafix.patch.TreePatch.AddGlobalImport
import scalafix.patch.TreePatch.RemoveGlobalImport
import scalafix.util.SemanticdbIndex

class DeprecatedPatchOps extends PatchOps {
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
  def removeGlobalImport(symbol: Symbol)(
      implicit index: SemanticdbIndex): Patch =
    RemoveGlobalImport(symbol)
  def addGlobalImport(symbol: Symbol)(implicit index: SemanticdbIndex): Patch =
    TreePatch.AddGlobalSymbol(symbol)
  def replaceSymbol(fromSymbol: Symbol.Global, toSymbol: Symbol.Global)(
      implicit index: SemanticdbIndex): Patch =
    TreePatch.ReplaceSymbol(fromSymbol, toSymbol)
  def replaceSymbols(toReplace: (String, String)*)(
      implicit index: SemanticdbIndex): Patch =
    toReplace.foldLeft(Patch.empty) {
      case (a, (from, to)) =>
        val (fromSymbol, toSymbol) =
          ScalafixMetaconfigReaders.parseReplaceSymbol(from, to).get
        a + replaceSymbol(fromSymbol, toSymbol)
    }
  def replaceSymbols(toReplace: Seq[(String, String)])(
      implicit noop: DummyImplicit,
      index: SemanticdbIndex): Patch =
    replaceSymbols(toReplace: _*)
  def renameSymbol(fromSymbol: Symbol.Global, toName: String)(
      implicit index: SemanticdbIndex): Patch =
    TreePatch.ReplaceSymbol(fromSymbol, Root(Signature.Term(toName)))
  override def lint(msg: LintMessage): Patch =
    LintPatch(msg)
}
