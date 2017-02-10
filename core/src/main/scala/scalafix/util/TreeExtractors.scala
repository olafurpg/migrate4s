package scalafix.util

import scala.meta._
import scala.collection.immutable.Seq

import scala.util.Try

object TreeExtractors {
  object CurriedArgs {
    def unapply(tree: Tree): Option[Seq[Seq[Tree]]] =
      Try(tree match {
        case t: Term.Update => t.argss
        case t: Decl.Def => t.tparams +: t.paramss
        case t: Defn.Def => t.tparams +: t.paramss
        case t: Defn.Macro => t.tparams +: t.paramss
        case t: Ctor.Primary => t.paramss
        case t: Ctor.Secondary => t.paramss
      }).toOption
  }
  object Args {
    def unapply(tree: Tree): Option[Seq[Tree]] =
      Try(tree match {
        case t: Decl.Type => t.tparams
        case t: Defn.Class => t.tparams
        case t: Defn.Trait => t.tparams
        case t: Defn.Type => t.tparams
        case t: Importer => t.importees
        case t: Pat.Extract => t.args
        case t: Pat.Tuple => t.args
        case t: Pat.Type.Apply => t.args
        case t: Term.Apply => t.args
        case t: Term.ApplyType => t.targs
        case t: Term.Function => t.params
        case t: Term.Tuple => t.args
        case t: Type.Apply => t.args
        case t: Type.Function => t.params
        case t: Type.Param => t.tparams
        case t: Type.Tuple => t.args
      }).toOption
  }
  object LastArgument {
    def unapply(tree: Tree): Option[Seq[Tree]] = tree match {
      case Args(args) => args.lastOption.map(_ :: Nil)
      case CurriedArgs(args) => Option(args.flatMap(_.lastOption))
      case _ => None
    }
  }
}
