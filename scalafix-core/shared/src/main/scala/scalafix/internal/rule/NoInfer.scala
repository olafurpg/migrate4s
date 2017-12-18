package scalafix.internal.rule

import scala.meta._
import metaconfig.{Conf, Configured}
import scalafix.lint.LintCategory
import scalafix.lint.LintMessage
import scalafix.rule.{Rule, RuleCtx}
import scalafix.rule.SemanticRule
import scalafix.util.SemanticdbIndex
import scalafix.util.SymbolMatcher
import scalafix.internal.config.NoInferConfig
import org.scalameta.logger

final case class NoInfer(index: SemanticdbIndex, config: NoInferConfig)
    extends SemanticRule(index, "NoInfer")
    with Product {

  private lazy val error: LintCategory =
    LintCategory.error(
      """The Scala compiler sometimes infers a too generic type such as Any.
        |If this is intended behavior, then the type should be explicitly type
        |annotated in the source.""".stripMargin
    )

  override def description: String =
    "Linter for types that the Scala compiler cannot infer."

  private lazy val noInferSymbol: SymbolMatcher =
    if (config.symbols.isEmpty)
      SymbolMatcher.normalized(NoInfer.badSymbols: _*)
    else SymbolMatcher.normalized(config.symbols: _*)

  override def init(config: Conf): Configured[Rule] =
    config
      .getOrElse("noInfer", "NoInfer")(NoInferConfig.default)
      .map(NoInfer(index, _))

  override def check(ctx: RuleCtx): Seq[LintMessage] =
    ctx.index.synthetics.flatMap {
      case synthetic @ Synthetic(pos, text, names) =>
        val enclosing = names
          .dropWhile(
            _.symbol == Symbol.Global(Symbol.None, Signature.Term("_star_")))
          .headOption
        logger.elem(enclosing)
        names.collect {
          case ResolvedName(_, noInferSymbol(Symbol.Global(_, signature)), _) =>
            val categoryId = signature.name.toLowerCase()
            error
              .copy(id = categoryId)
              .at(s"Inferred ${signature.name} in $text", pos)
        }
    }
}

case object NoInfer {
  lazy val badSymbols: List[Symbol] = List(
    Symbol("_root_.java.io.Serializable."),
    Symbol("_root_.scala.Any."),
    Symbol("_root_.scala.AnyVal."),
    Symbol("_root_.scala.Product.")
  )

  def badSymbolNames: List[String] = badSymbols.collect {
    case Symbol.Global(_, signature) => signature.name
  }
}
