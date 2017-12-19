package scalafix.internal.util

import scala.collection.mutable
import scala.meta._
import scalafix.SemanticdbIndex
import scalafix.util.SymbolMatcher

object DesugarOps {
  private lazy val TypeDialect = scala.meta.dialects.Scala212.copy(
    allowTypeLambdas = true,
    allowMethodTypes = true
  )
  object ResultType {
    def unapply(defn: Member)(
        implicit index: SemanticdbIndex): Option[(Member, Type)] = {
      for {
        sym <- index.symbol(defn.name)
        denot <- index.denotation(sym)
        tpe <- TypeDialect(Input.Denotation(denot.signature, sym))
          .parse[Type]
          .toOption
      } yield defn -> tpe
    }
  }
  def desugar(tree: Tree, _synthetics: Map[Position, Synthetic])(
      implicit index: SemanticdbIndex): Tree = {
    val Star = SymbolMatcher.Star
    val isUsedSynthetic = mutable.Set.empty[Position]
    def isForComprehensionSynthetic(arg: Term): Boolean = arg match {
      // Not possible to determine this by symbols since for-comprehension
      // contract is syntactic.
      case q"*.flatMap" | q"*.foreach" | q"*.map" | q"*.withFilter" => true
      case Term.Apply(qual, _) => isForComprehensionSynthetic(qual)
      case Term.ApplyType(qual, _) => isForComprehensionSynthetic(qual)
      case _ => false
    }

    def desugarOnce(tree: Tree): Option[Tree] = {
      if (tree.is[Enumerator]) return None // Not yet supported
      for {
        synthetic <- _synthetics
          .get(tree.pos)
          .orElse {
            tree.pos match {
              case Position.Range(input, _, end) =>
                _synthetics.get(Position.Range(input, end, end))
              case _ => None
            }
          }
        if !isUsedSynthetic(synthetic.position)
        syntheticTerm <- synthetic.input.parse[Term].toOption
        if !isForComprehensionSynthetic(syntheticTerm)
      } yield {
        val merged = syntheticTerm.transform {
          case Star(_: Term.Name) => tree
        }
        isUsedSynthetic += synthetic.position
        merged
      }
    }
    object Desugared {
      def unapply(tree: Tree): Option[Tree] =
        desugarOnce(tree)
    }
    val isDone = mutable.Set.empty[Position]
    val DesugaringTransformer = new Transformer {
      override def apply(tree: Tree): Tree = tree match {
        case base @ Desugared(desugared) if !isDone(base.pos) =>
          isDone += base.pos
          super.apply(desugared)
        case _ =>
          val next = tree match {
            case ResultType(
                Defn.Def(mods, pats, tparams, paramss, None, body),
                Type.Method(_, tpe)) =>
              Defn.Def(mods, pats, tparams, paramss, Some(tpe), body)
            case ResultType(Defn.Val(mods, pats, None, body), tpe) =>
              Defn.Val(mods, pats, Some(tpe), body)
            case ResultType(Defn.Var(mods, pats, None, body), tpe) =>
              Defn.Var(mods, pats, Some(tpe), body)
            case _ =>
              tree
          }
          super.apply(next)
      }
    }
    DesugaringTransformer(tree)
  }
}
