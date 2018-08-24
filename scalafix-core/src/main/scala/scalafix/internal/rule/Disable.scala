package scalafix.internal.rule

import metaconfig.{Conf, Configured}
import scala.meta._
import scala.meta.transversers.Traverser
import scalafix.internal.config.{DisableConfig, DisabledSymbol}
import scalafix.internal.v0.InputSynthetic
import scalafix.internal.util.SymbolOps
import scalafix.internal.v0.LegacyCodePrinter
import scalafix.v0
import scalafix.v1._

object Disable {

  /**
    * A tree traverser to collect values with a custom context.
    * At every tree node, either builds a new Context or returns a new Value to accumulate.
    * To collect all accumulated values, use result(Tree).
    */
  class ContextTraverser[Value, Context](initContext: Context)(
      fn: PartialFunction[(Tree, Context), Either[Value, Context]])
      extends Traverser {
    private var context: Context = initContext
    private val buf = scala.collection.mutable.ListBuffer[Value]()

    private val liftedFn = fn.lift

    override def apply(tree: Tree): Unit = {
      liftedFn((tree, context)) match {
        case Some(Left(res)) =>
          buf += res
        case Some(Right(newContext)) =>
          val oldContext = context
          context = newContext
          super.apply(tree)
          context = oldContext
        case None =>
          super.apply(tree)
      }
    }

    def result(tree: Tree): List[Value] = {
      context = initContext
      buf.clear()
      apply(tree)
      buf.toList
    }
  }

  final class DisableSymbolMatcher(symbols: List[DisabledSymbol]) {
    def findMatch(symbol: Symbol): Option[DisabledSymbol] = {
      if (!symbol.isGlobal) None
      else {
        val normalized = SymbolOps.normalize(symbol)
        symbols.find(_.matches(normalized))
      }
    }

    def unapply(tree: Tree)(
        implicit doc: SemanticDoc): Option[(Tree, DisabledSymbol)] =
      findMatch(tree.symbol).map(ds => (tree, ds))

    def unapply(symbol: Symbol): Option[(Symbol, DisabledSymbol)] =
      findMatch(symbol).map(ds => (symbol, ds))
  }
}

final case class Disable(config: DisableConfig = DisableConfig.default)
    extends SemanticRule("Disable") {

  import Disable._

  override def description: String =
    "Linter that reports an error on a configurable set of symbols."

  override def withConfig(config: Conf): Configured[Rule] =
    config
      .getOrElse("disable", "Disable")(DisableConfig.default)
      .map(Disable(_))

  private val unlessInside =
    new DisableSymbolMatcher(config.unlessInside.flatMap(_.symbols))
  private val ifSynthetic =
    new DisableSymbolMatcher(config.ifSynthetic)

  private def createDiagnostic(
      symbol: Symbol,
      disabled: DisabledSymbol,
      pos: Position,
      details: String = ""): Diagnostic = {
    val message =
      disabled.message.getOrElse(s"${symbol.displayName} is disabled$details")
    val id = disabled.id.getOrElse(symbol.displayName)
    Diagnostic(id, message, pos)
  }

  private def checkTree(implicit ctx: SemanticDoc): Seq[Diagnostic] = {
    def filterBlockedSymbolsInBlock(
        blockedSymbols: List[DisabledSymbol],
        block: Tree): List[DisabledSymbol] = {
      val symbol = block.symbol.normalized
      if (!symbol.isGlobal) blockedSymbols
      else {
        val symbolsInMatchedBlocks =
          config.unlessInside.flatMap { u =>
          pprint.log(u.safeBlocks.map(_.symbol))
            if (u.safeBlocks.exists(_.matches(symbol))) {
              ???
              u.symbols
            } else {
              List.empty
            }
          }
        pprint.log(symbol)
        pprint.log(blockedSymbols.map(_.symbol))
        pprint.log(symbolsInMatchedBlocks.map(_.symbol))
        blockedSymbols.filterNot(symbolsInMatchedBlocks.contains)
      }
    }

    def skipTermSelect(term: Term): Boolean = term match {
      case _: Term.Name => true
      case Term.Select(q, _) => skipTermSelect(q)
      case _ => false
    }

    def handleName(
        t: Name,
        blockedSymbols: List[DisabledSymbol]
    ): Either[Diagnostic, List[DisabledSymbol]] = {
      val isBlocked = new DisableSymbolMatcher(blockedSymbols)
      t.symbol match {
        case isBlocked(s: Symbol, disabled) =>
          val g = SymbolOps.normalize(s)
          if (g.displayName != "<init>") {
            Left(createDiagnostic(g, disabled, t.pos))
          } else {
            Right(blockedSymbols)
          }
        case _ => Right(blockedSymbols)
      }
    }

    new ContextTraverser(config.allDisabledSymbols)({
      case (_: Import, _) => Right(List.empty)
      case (Term.Select(q, name), blockedSymbols) if skipTermSelect(q) =>
        handleName(name, blockedSymbols)
      case (Type.Select(q, name), blockedSymbols) if skipTermSelect(q) =>
        handleName(name, blockedSymbols)
      case (
          Term.Apply(
            Term.Select(block @ unlessInside(_, _), Term.Name("apply")),
            _
          ),
          blockedSymbols
          ) =>
        Right(filterBlockedSymbolsInBlock(blockedSymbols, block)) // <Block>.apply
      case (Term.Apply(block @ unlessInside(_, _), _), blockedSymbols) =>
        Right(filterBlockedSymbolsInBlock(blockedSymbols, block)) // <Block>(...)
      case (_: Defn.Def, _) =>
        Right(config.allDisabledSymbols) // reset blocked symbols in def
      case (_: Term.Function, _) =>
        Right(config.allDisabledSymbols) // reset blocked symbols in (...) => (...)
      case (t: Name, blockedSymbols) =>
        handleName(t, blockedSymbols)
    }).result(ctx.tree)
  }

  def synthetics(doc: SemanticDoc): Seq[v0.Synthetic] =
    doc.internal.textDocument.synthetics.map(s =>
      new LegacyCodePrinter(doc).convertSynthetic(s))

  private def checkSynthetics(implicit ctx: SemanticDoc): Seq[Diagnostic] = {
    for {
      synthetic <- synthetics(ctx)
      (pos, ifSynthetic(symbol, disabled)) <- {
        synthetic.names.map { s =>
          (s.position, Symbol(s.symbol.syntax))
        }
      }
    } yield {
      val (details, caret) = pos.input match {
        case synth @ Input.Stream(InputSynthetic(_, input, start, end), _) =>
          // For synthetics the caret should point to the original position
          // but display the inferred code.
          s" and it got inferred as `${synth.text}`" ->
            Position.Range(input, start, end)
        case _ =>
          "" -> pos
      }
      createDiagnostic(symbol, disabled, caret, details)
    }
  }

  override def fix(implicit ctx: SemanticDoc): Patch = {
    val diagnostics = checkTree(ctx) ++ checkSynthetics(ctx)
    diagnostics.map(Patch.lint).asPatch
  }
}
