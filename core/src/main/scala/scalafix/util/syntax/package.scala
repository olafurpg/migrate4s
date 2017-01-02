package scalafix.util

import scala.{meta => m}
import scala.meta._

package object syntax {

  //Allow two patterns to be combined.  Contributed by @nafg
  object & { def unapply[A](a: A) = Some((a, a)) }

  implicit class MetaOps(from: m.Tree) {
    def termNames: List[Term.Name] = {
      from collect {
        case t: Term.Name => t
      }
    }

    def typeNames: List[Type.Name] = {
      from collect {
        case t: Type.Name => t
      }
    }
  }

  implicit class ApplyPimp(t: Term.Apply) {
    def argss: Seq[Seq[Term.Arg]] = t.fun match {
      case tt: Term.Apply => t.args +: tt.argss
      case _ => Seq(t.args)
    }
  }

}
