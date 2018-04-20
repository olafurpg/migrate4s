package scalafix.v1

import java.util
import scala.meta.Input
import scala.meta.Tokens
import scala.meta.Tree
import scala.meta.contrib.AssociatedComments
import scala.meta.internal.semanticdb3
import scalafix.internal.util.SymbolTable
import scalafix.util.MatchingParens
import scalafix.util.TokenList
import scala.meta.internal.{semanticdb3 => s}
import scalafix.internal.util.TreeOps

final class SemanticDoc private[scalafix] (
    val tree: Tree,
    val tokens: Tokens,
    val input: Input,
    val matching: MatchingParens,
    val tokenList: TokenList,
    val comments: AssociatedComments,
    // privates
    private[scalafix] val doc: s.TextDocument,
    private[scalafix] val table: SymbolTable
) {
  private[this] val locals = doc.symbols.iterator.collect {
    case info if info.symbol.startsWith("local") =>
      info.symbol -> info
  }.toMap
  // NOTE(olafur): it should be possible to avoid this using TextDocument during parsing to
  // attach symbols and types to trees as they are constructed.
  private[this] val occurrences: util.HashMap[semanticdb3.Range, String] = {
    val result = new util.HashMap[s.Range, String]()
    doc.occurrences.foreach { o =>
      if (o.range.isDefined) {
        // TODO: handle multi symbols
        result.put(o.range.get, o.symbol)
      }
    }
    result
  }

  def symbol(tree: Tree): Sym = {
    val pos = TreeOps.symbolPos(tree)
    val result = occurrences.get(
      s.Range(
        startLine = pos.startLine,
        startCharacter = pos.startColumn,
        endLine = pos.endLine,
        endCharacter = pos.endColumn
      )
    )
    if (result == null) Sym.None
    else Sym(result)
  }

  def info(sym: Sym): Sym.Info = {
    if (sym.isLocal) {
      new Sym.Info(locals.getOrElse(sym.value, s.SymbolInformation()))
    } else {
      new Sym.Info(table.info(sym.value).getOrElse(s.SymbolInformation()))
    }
  }
}
