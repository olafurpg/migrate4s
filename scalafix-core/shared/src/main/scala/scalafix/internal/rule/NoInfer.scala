package scalafix.internal.rule

import scala.meta._
import metaconfig.{Conf, Configured}
import scalafix._
import scalafix.internal.config.NoInferConfig
import scalafix.util.SymbolMatcher
import org.scalameta.logger

final case class NoInfer(index: SemanticdbIndex, config: NoInferConfig)
    extends SemanticRule(index, "NoInfer")
    with Product {

  private lazy val error: LintCategory =
    LintCategory.error(
      """The Scala compiler sometimes infers a too generic type such as Any.
        |If this is intended behavior, then the type should be explicitly type
        |annotated in the source. If you believe it's fine to infer this symbol
        |""".stripMargin
    )

  override def description: String =
    "Linter for types that the Scala compiler cannot infer."

  private lazy val noInferSymbol: SymbolMatcher =
    if (config.symbols.isEmpty)
      SymbolMatcher.normalized(NoInfer.badSymbols: _*)
    else SymbolMatcher.normalized(config.symbols: _*)

  private lazy val excludedSymbol: SymbolMatcher =
    SymbolMatcher.normalized(config.excludeEnclosing: _*)

  private val Star = Symbol("_star_.")

  override def init(config: Conf): Configured[Rule] =
    config
      .getOrElse("noInfer", "NoInfer")(NoInferConfig.default)
      .map(NoInfer(index, _))

  override def check(ctx: RuleCtx): Seq[LintMessage] = {
    val ctxIndex = ctx.index

    def isExcluded(synthetic: Synthetic): Boolean =
      config.excludeEnclosing.nonEmpty && {
        enclosingSymbol(synthetic).exists { enclosing =>
          logger.elem(enclosing)
          excludedSymbol.matches(enclosing)
        }
      }

    def enclosingSymbol(synthetic: Synthetic): Option[Symbol] = {
      for {
        term <- synthetic.input.parse[Term].toOption
        symbol <- index.symbol(term)
      } yield {
        if (symbol != Symbol.Global(Symbol.None, Signature.Term("_star_"))) {
          symbol
        } else {
          val Start = synthetic.position.start
          val replacement = ctxIndex.database.names.collectFirst {
            case ResolvedName(Position.Range(_, _, Start), sym, _) =>
              sym
          }
          replacement.getOrElse {
            Star
          }
        }
      }
    }

    for {
      synthetic @ Synthetic(pos, text, names) <- ctxIndex.synthetics
      if !isExcluded(synthetic)
      ResolvedName(_, noInferSymbol(Symbol.Global(_, signature)), _) <- names
    } yield {
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
