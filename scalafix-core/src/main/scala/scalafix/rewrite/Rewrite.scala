package scalafix
package rewrite

import sourcecode.Name
import scala.collection.immutable.Seq
import scala.meta._
import scala.collection.immutable.Seq
import scalafix.config.ReaderUtil
import scalafix.patch.SemanticPatchOps

import metaconfig.ConfDecoder

/** A rewrite is a named RewriteCtx => Patch function. */
abstract class Rewrite(implicit sourceName: Name) { self =>
  def name: String = sourceName.value
  override def toString: String = name
  def rewrite(ctx: RewriteCtx): Patch
  def andThen(other: Rewrite): Rewrite = Rewrite.merge(this, other)
  private[scalafix] def wrappedRewrite(ctx: RewriteCtx): Patch =
    patch.InCtx(rewrite(ctx), ctx, None)
}

abstract class SemanticRewrite(mirror: Mirror)(implicit name: Name)
    extends Rewrite {
  private[scalafix] override def wrappedRewrite(ctx: RewriteCtx): Patch =
    patch.InCtx(rewrite(ctx), ctx, Some(mirror))
}

object Rewrite {
  val syntaxRewriteConfDecoder = config.rewriteConfDecoder(None)
  def empty: Rewrite = syntactic(_ => Patch.empty)
  def syntactic(f: RewriteCtx => Patch)(implicit name: Name): Rewrite =
    new Rewrite() {
      override def rewrite(ctx: RewriteCtx): Patch = f(ctx)
    }
  def semantic(f: Mirror => RewriteCtx => Patch)(
      implicit name: Name): Mirror => SemanticRewrite = { mirror =>
    new SemanticRewrite(mirror) {
      override def rewrite(ctx: RewriteCtx): Patch = f(mirror)(ctx)
    }
  }
  def merge(a: Rewrite, b: Rewrite) =
    new Rewrite()(Name(s"${a.name}+${b.name}")) {
      override def rewrite(ctx: RewriteCtx) =
        a.rewrite(ctx) + b.rewrite(ctx)
      override def wrappedRewrite(ctx: RewriteCtx): Patch =
        a.wrappedRewrite(ctx) + b.wrappedRewrite(ctx)
    }
}
