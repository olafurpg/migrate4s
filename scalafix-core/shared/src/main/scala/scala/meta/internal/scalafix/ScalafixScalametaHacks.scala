package scala.meta.internal.scalafix

import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.internal.trees.Origin

object ScalafixScalametaHacks { self =>
  def dialect(language: String): Dialect = Dialect.standards(language)
  def resetOrigin(tree: Tree): Tree = tree.withOrigin(Origin.None)
  def copyOrigin[T <: Tree](to: T, from: Tree): T = withOrigin(to, origin(from))
  def withOrigin[T <: Tree](tree: T, origin: Origin): T =
    tree.withOrigin(origin)
  def origin(tree: Tree): Origin = tree.origin
}
