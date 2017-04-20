package scalafix
package rewrite

import scala.collection.immutable.Seq
import scala.meta._
import scalafix.util.Patch
import scala.collection.immutable.Seq
import scalafix.config.ReaderUtil

import metaconfig.ConfDecoder

/** A rewrite is a named RewriteCtx => Patch function. */
abstract class Rewrite(implicit sourceName: sourcecode.Name) {
  def name: String = sourceName.value
  override def toString: String = name
  def rewrite(ctx: RewriteCtx): Patch
  def andThen(other: Rewrite): Rewrite =
    Rewrite(ctx => this.rewrite(ctx) + other.rewrite(ctx))
}

abstract class SemanticRewrite(mirror: Mirror) extends Rewrite

object Rewrite {
  val syntaxRewriteConfDecoder = config.rewriteConfDecoder(None)
  def empty: Rewrite = syntactic(_ => Patch.empty)
  def syntactic(f: RewriteCtx => Patch)(
      implicit name: sourcecode.Name): Rewrite = apply(f)
  def semantic(f: Mirror => RewriteCtx => Patch)(
      implicit name: sourcecode.Name): Mirror => SemanticRewrite = { mirror =>
    new SemanticRewrite(mirror) {
      override def rewrite(ctx: RewriteCtx): Patch = f(mirror)(ctx)
    }
  }
  def apply(f: RewriteCtx => Patch)(implicit name: sourcecode.Name): Rewrite =
    new Rewrite() {
      override def rewrite(ctx: RewriteCtx): Patch = f(ctx)
    }
}
