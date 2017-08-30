package scalafix

import scala.language.experimental.macros

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.semanticdb.Symbol
import scala.compat.Platform.EOL
import scala.meta.internal.scalafix.ScalafixScalametaHacks
import scalafix.internal.util.SymbolOps
import scalafix.util.InputOps
import scalafix.util.SymbolMatcher
import scalafix.util.TreeOps

package object syntax {
  implicit class XtensionRefSymbolOpt(tree: Tree)(implicit sctx: SemanticCtx) {
    @deprecated("Renamed to symbol", "0.5.0")
    def symbolOpt: Option[Symbol] = symbol
    def symbol: Option[Symbol] = sctx.symbol(tree.pos)
    def denotation: Option[Denotation] = sctx.denotation(tree)
  }
  implicit class XtensionParsedOpt[T](parsed: Parsed[T]) {
    def toOption: Option[T] = parsed match {
      case parsers.Parsed.Success(tree) => Some(tree)
      case _ => None
    }
  }
  implicit class XtensionSymbolSemanticCtx(symbol: Symbol)(
      implicit sctx: SemanticCtx) {
    @deprecated("Renamed to denotation", "0.5.0")
    def denotOpt: Option[Denotation] = denotation
    def denotation: Option[Denotation] = sctx.denotation(symbol)
  }
  implicit class XtensionSymbol(symbol: Symbol) {
    def normalized: Symbol = SymbolOps.normalize(symbol)
  }
  implicit class XtensionAttributes(attributes: Attributes) {
    def dialect: Dialect = ScalafixScalametaHacks.dialect(attributes.language)
  }
  implicit class XtensionTreeScalafix(tree: Tree) {
    def matches(matcher: SymbolMatcher): Boolean =
      matcher.matches(tree)
    def parents: Stream[Tree] = TreeOps.parents(tree)
    def input: Input = TreeOps.input(tree)
  }
  implicit class XtensionInputScalafix(input: Input) {
    def label: String = InputOps.label(input)
  }
}
