package scalafix.util

import scala.compat.Platform.EOL
import scala.meta.Tree
import scala.meta.inputs
import scala.meta.inputs.Input

object InputOps {
  def label(input: Input): String = input match {
    case inputs.Input.File(path, _) => path.toString()
    case inputs.Input.VirtualFile(label, _) => label
    case _ =>
      s"Input.${input.productPrefix}('<${input.chars.take(10).mkString}...>')"
        .replaceAllLiterally(EOL, "")
  }

}
object TreeOps {
  def input(tree: Tree): Input =
    tree.tokens.headOption.map(_.input).getOrElse(Input.None)
  def parents(tree: Tree): Stream[Tree] =
    tree #:: (tree.parent match {
      case Some(x) => parents(x)
      case _ => Stream.empty
    })
}
