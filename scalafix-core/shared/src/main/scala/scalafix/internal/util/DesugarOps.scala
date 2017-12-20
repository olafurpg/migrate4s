package scalafix.internal.util

import scala.collection.mutable
import scala.meta._
import scala.meta.internal.scalafix.ScalafixScalametaHacks._
import scala.util.control.NonFatal
import scalafix.SemanticdbIndex
import scalafix.internal.util.ScalametaEnrichments._
import org.scalameta.logger

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
  def desugarTree(tree: Tree)(implicit index: SemanticdbIndex): Tree = {
    implicit class XtensionPosition(val pos: Position) {
      def isOffset: Boolean =
        pos != Position.None &&
          pos.start == pos.end
    }
    implicit class XtensionBang(val x: Any) {
      def unary_![B]: B = x.asInstanceOf[B]
    }
    implicit class XtensionSynthetic(val s: Synthetic) {
      def isConversion: Boolean = !s.text.startsWith("*")
      def isImplicitArg: Boolean = s.text.startsWith("*(")
      def isTypeParam: Boolean = s.text.startsWith("*[")
      def isApply: Boolean = s.text.startsWith("*.apply")
      def isSupported: Boolean =
        isConversion ||
          isImplicitArg ||
          isTypeParam ||
          isApply
    }
    val isDone = mutable.Set.empty[Synthetic]
    val infixEnds = mutable.Set.empty[Position]

    def desugarOnce(term: Tree): Option[Term] = {
      for {
        synthetic <- index.synthetic(term.pos)
        if !synthetic.isTypeParam || !infixEnds(synthetic.position)
        if synthetic.isSupported
        if !isDone(synthetic)
        syntheticTerm <- synthetic.input.parse[Term].toOption
      } yield {
        isDone += synthetic
        val result = syntheticTerm.transform {
          case index.Star(_: Term.Name) =>
            term
        }
        result.asInstanceOf[Term]
      }
    }
    new Transformer {
      override def apply(tree: Tree): Tree = {
        tree match {
          case Term.ApplyInfix(lhs, op, Nil, args) =>
            infixEnds += tree.pos.endOffset
            desugarOnce(op) match {
              case Some(d) =>
                val next = d match {
                  case Term.ApplyType(Term.Name(op2), targs)
                      if op2 == op.value =>
                    copyOrigin(Term.ApplyInfix(lhs, op, targs, args), tree)
                  case _ =>
                    tree
                }
                desugarOnce(next).fold(super.apply(next))(this.apply)
              case _ =>
                super.apply(tree)
            }
          case term: Term =>
            desugarOnce(term).fold(super.apply(tree))(this.apply)
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
    }.apply(tree)
  }

  def desugarTreeOld(tree: Tree, _synthetics: Map[Position, Synthetic])(
      implicit index: SemanticdbIndex): Tree = {
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
            case index.Star(_: Term.Name) =>
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
