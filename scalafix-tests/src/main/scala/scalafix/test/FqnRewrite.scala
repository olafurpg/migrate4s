package scalafix
package test

import scala.collection.immutable.Seq
import scala.meta._
import scalafix.patch.Patch
import scalafix.rewrite.RewriteCtx

case class FqnRewrite(implicit mirror: Mirror) extends SemanticRewrite(mirror) {
  override def rewrite(ctx: RewriteCtx): Patch =
    ctx.addGlobalImport(importer"scala.meta._")
}
