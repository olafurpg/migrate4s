package scalafix.test

import scala.collection.immutable.Seq
import scala.meta._
import scalafix.rewrite.Rewrite
import scalafix.rewrite.RewriteCtx
import scalafix.util.Patch
import scalafix.util.TreePatch.AddGlobalImport

case object FqnRewrite extends Rewrite {
  override def rewrite[B <: Any](ctx: RewriteCtx[B]): Patch =
    AddGlobalImport(importer"scala.meta._")
}
