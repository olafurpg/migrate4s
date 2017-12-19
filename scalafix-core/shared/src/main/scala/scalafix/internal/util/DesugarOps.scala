package scalafix.internal.util

import scala.collection.mutable
import scala.meta._
import scala.util.Try
import scala.util.control.NonFatal
import scalafix.SemanticdbIndex
import scalafix.util.SymbolMatcher

object DesugarOps {

  /** Consolidates synthetics with non-synthetic tree
    *
    * Handles the following desugarings:
    *
    * - explicit type parameters
    * - explicit .apply
    * - explicit implicit parameters
    * - explicit implicit conversions
    * - explicit type annotations for val/var/def
    *
    * "Desugar" is kind of a misnomer since it does not for example desugar
    * for comprehensions.
    */
  def desugarTree(tree: Tree, _synthetics: Map[Position, Synthetic])(
      implicit index: SemanticdbIndex): Tree = {
    val Star = SymbolMatcher.Star
    val isUsedSynthetic = mutable.Set.empty[Position]
    def desugarOnce(tree: Tree): Option[Tree] = {
      if (tree.is[Pat] || tree.is[Case]) return None // ???
      if (tree.is[Term.Param]) return None // ???
      if (tree.is[Enumerator]) return None // Not yet supported
      if (tree.is[Defn]) return None // Makes no sense
      if (tree.is[Term.ApplyInfix]) return None // TODO(olafur) hack
      if (tree.is[Term.Name] && tree.parent.exists(_.is[Term.ApplyInfix]))
        // TODO(olafur) hack, figure out how to add tparam for infix apply
        return None
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
        if !isSpecialSynthetic(synthetic)
        if !isUsedSynthetic(synthetic.position)
        syntheticTerm <- synthetic.input.parse[Term].toOption
        merged <- try {
          Some(syntheticTerm.transform {
//            case nme: Type.Name if index.symbol(nme).isDefined =>
//              SymbolOps.toTermRef(index.symbol(nme).get)
            case Star(_: Term.Name) =>
              tree
          })
        } catch {
          case NonFatal(e) =>
            println(
              s"Failed to merge $syntheticTerm with $tree. ${e.getMessage}")
            None
        }
      } yield {
        isUsedSynthetic += synthetic.position
        merged
      }
    }
    object Desugared {
      def unapply(tree: Tree): Option[Tree] = desugarOnce(tree)
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

  // Not possible to determine this by symbols since for-comprehension
  // contract is syntactic.
  private def isSpecialSynthetic(synthetic: Synthetic): Boolean = {
    synthetic.text.startsWith("*.unapplySeq") ||
    synthetic.text.startsWith("*.unapply") ||
    synthetic.text.startsWith("*.withFilter") ||
    synthetic.text.startsWith("*.foreach") ||
    synthetic.text.startsWith("*.map") ||
    synthetic.text.startsWith("*.flatMap") ||
    synthetic.text.startsWith("*.foreach")
  }

  private lazy val TypeDialect = scala.meta.dialects.Scala212.copy(
    allowTypeLambdas = true,
    allowMethodTypes = true
  )

  private object ResultType {
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
}
