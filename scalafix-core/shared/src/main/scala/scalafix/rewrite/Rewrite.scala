package scalafix.rewrite

import scala.collection.immutable.Seq
import scala.meta._
import scalafix.internal.util.Failure
import scalafix.patch.Patch
import scalafix.util.InputOps
import scalafix.util.SemanticCtx
import scalafix.util.TreeOps
import metaconfig.Conf
import metaconfig.ConfDecoder
import metaconfig.Configured

/** A Rewrite is a program that produces a Patch from a scala.meta.Tree. */
abstract class Rewrite(implicit val rewriteName: RewriteName) { self =>

  /** Build patch for a single tree/compilation unit.
    *
    * Override this method to implement a rewrite.
    */
  def rewrite(ctx: RewriteCtx): Patch

  /** Initialize rewrite.
    *
    * This method is called once by scalafix before rewrite is called.
    * Use this method to either read custom configuration or to build
    * expensive indices.
    *
    * @param config The .scalafix.conf configuration.
    * @return the initialized rewrite or an error. If no initialization is needed,
    *         return Configured.Ok(this).
    */
  def init(config: Conf): Configured[Rewrite] =
    Configured.Ok(this)

  /** Combine this rewrite with another rewrite. */
  final def merge(other: Rewrite): Rewrite = Rewrite.merge(this, other)
  @deprecated("Renamed to merge.", "0.5.0")
  final def andThen(other: Rewrite): Rewrite = merge(other)

  /** Returns string output of applying this single patch. */
  final def apply(ctx: RewriteCtx): String = apply(ctx, rewrite(ctx))
  final def apply(ctx: RewriteCtx, patch: Patch): String = {
    val result = Patch(patch, ctx, semanticOption)
    Patch.lintMessages(patch, ctx).foreach { msg =>
      // Set the lint message owner. This allows us to distinguish
      // LintCategory with the same id from different rewrites.
      ctx.printLintMessage(msg, rewriteName)
    }
    result
  }

  /** Returns unified diff from applying this patch */
  final def diff(ctx: RewriteCtx): String =
    diff(ctx, rewrite(ctx))
  final protected def diff(ctx: RewriteCtx, patch: Patch): String = {
    val original = TreeOps.input(ctx.tree)
    Patch.unifiedDiff(
      original,
      Input.VirtualFile(InputOps.label(original), apply(ctx, patch)))

  }

  final def name: String = rewriteName.toString
  final def names: List[String] = rewriteName.identifiers.map(_.value)
  final override def toString: String = name.toString

  // NOTE. This is kind of hacky and hopefully we can find a better workaround.
  // The challenge is the following:
  // - a.andThen(b) needs to work for mixing semantic + syntactic rewrites.
  // - applied/appliedDiff should work without passing in SemanticCtx explicitly
  protected[scalafix] def semanticOption: Option[SemanticCtx] = None
}

abstract class SemanticRewrite(sctx: SemanticCtx)(implicit name: RewriteName)
    extends Rewrite {
  implicit val ImplicitSemanticCtx: SemanticCtx = sctx
  override def semanticOption: Option[SemanticCtx] = Some(sctx)
}

object Rewrite {
  lazy val empty: Rewrite = syntactic(_ => Patch.empty)(RewriteName.empty)
  def emptyConfigured: Configured[Rewrite] = Configured.Ok(empty)
  def emptyFromSemanticCtxOpt(sctx: Option[SemanticCtx]): Rewrite =
    sctx.fold(empty)(emptySemantic)
  def combine(rewrites: Seq[Rewrite]): Rewrite =
    rewrites.foldLeft(empty)(_ merge _)
  private[scalafix] def emptySemantic(sctx: SemanticCtx): Rewrite =
    semantic(_ => _ => Patch.empty)(RewriteName.empty)(sctx)

  /** Creates a syntactic rewrite. */
  def syntactic(f: RewriteCtx => Patch)(implicit name: RewriteName): Rewrite =
    new Rewrite() {
      override def rewrite(ctx: RewriteCtx): Patch = f(ctx)
    }

  /** Creates a semantic rewrite. */
  def semantic(f: SemanticCtx => RewriteCtx => Patch)(
      implicit rewriteName: RewriteName): SemanticCtx => Rewrite = { sctx =>
    new SemanticRewrite(sctx) {
      override def rewrite(ctx: RewriteCtx): Patch = f(sctx)(ctx)
    }
  }

  /** Creates a rewrite that always returns the same patch. */
  def constant(name: String, patch: Patch, sctx: SemanticCtx): Rewrite =
    new SemanticRewrite(sctx)(RewriteName(name)) {
      override def rewrite(ctx: RewriteCtx): Patch = patch
    }

  /** Combine two rewrites into a single rewrite */
  def merge(a: Rewrite, b: Rewrite): Rewrite = {
    new Rewrite()(a.rewriteName + b.rewriteName) {
      override def init(config: Conf): Configured[Rewrite] = {
        a.init(config).product(b.init(config)).map {
          case (x, y) => x.merge(y)
        }
      }
      override def rewrite(ctx: RewriteCtx): Patch =
        a.rewrite(ctx) + b.rewrite(ctx)
      override def semanticOption: Option[SemanticCtx] =
        (a.semanticOption, b.semanticOption) match {
          case (Some(m1), Some(m2)) =>
            if (m1 ne m2) throw Failure.MismatchingSemanticCtx(m1, m2)
            else Some(m1)
          case (a, b) => a.orElse(b)
        }
    }
  }
}
