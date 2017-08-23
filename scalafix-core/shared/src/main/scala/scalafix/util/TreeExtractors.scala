package scalafix.util
import scala.meta._
object TreeExtractors {
  object `:WithParent:` {
    def unapply(tree: Tree): Option[(Tree, Tree)] =
      tree.parent.map(parent => tree -> parent)
  }
  object RelevantName {
    def unapply(term: Term): Option[Name] = term match {
      case name @ Name(_) => Some(name)
      case Term.Select(_, name @ Name(_)) => Some(name)
      case Type.Select(_, name @ Name(_)) => Some(name)
      case _ => None
    }
  }
}
