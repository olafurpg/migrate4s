package scalafix.internal.rule

import scala.meta._
import scalafix._

class DesugaringTransformer(index: SemanticdbIndex) extends Transformer {
  override def apply(tree: Tree): Tree = tree match {
    case term: Term =>
      index.desugar(term) match {
        case Some(desugared) => desugared
        case None => super.apply(term)
      }
    case t => super.apply(t)
  }
}
object DesugaringTransformer {
  def desugar(tree: Tree, index: SemanticdbIndex): Tree =
    new DesugaringTransformer(index).apply(tree)
}
