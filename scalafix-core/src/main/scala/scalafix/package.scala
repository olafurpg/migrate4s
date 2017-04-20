import scala.meta._

package object scalafix {

//  type ScalafixConfig = config.ScalafixConfigT[rewrite.ScalafixMirror]

  type Rewrite = rewrite.Rewrite
  val Rewrite = rewrite.Rewrite

  type Patch = util.Patch
  val Patch = util.Patch

  type RewriteCtx[T] = rewrite.RewriteCtx[T]
  type RewriteCtx = RewriteCtx[Mirror]
  // Syntactic rewrite ctx is RewriteCtx[Null] because it is a subtype of any other rewritectx.
  type SyntacticRewriteCtx = RewriteCtx[Any]
  val RewriteCtx = rewrite.RewriteCtx

  implicit class XtensionRewriteCtx(val ctx: SemanticRewriteCtx)
      extends rewrite.SemanticPatchOps
  implicit class XtensionRewriteCtx[T](val ctx: RewriteCtx[T])
      extends rewrite.SyntacticPatchOps[T]
  implicit class XtensionSeqPatch(patches: Seq[Patch]) {
    def asPatch: Patch = Patch.fromSeq(patches)
  }
}
