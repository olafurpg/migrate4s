import scala.meta._

package object scalafix {

  type ScalafixConfig = config.ScalafixConfig
  val ScalafixConfig = config.ScalafixConfig

  type RewriteCtx = rewrite.RewriteCtx
  val RewriteCtx = rewrite.RewriteCtx

  type Rewrite = rewrite.Rewrite
  val Rewrite = rewrite.Rewrite

  type Patch = util.Patch
  val Patch = util.Patch

  implicit class XtensionMirrorRewriteCtx(val ctx: RewriteCtx)(
      implicit val mirror: Mirror)
      extends rewrite.SemanticPatchOps
  implicit class XtensionRewriteCtx(val ctx: RewriteCtx)
      extends rewrite.SyntacticPatchOps
  implicit class XtensionSeqPatch(patches: Seq[Patch]) {
    def asPatch: Patch = Patch.fromSeq(patches)
  }
}
