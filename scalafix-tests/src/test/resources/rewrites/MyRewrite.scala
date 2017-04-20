package foo.bar {

  import scalafix._
  import scala.meta._

  case object MyRewrite extends Rewrite {
    def rewrite[T](ctx: RewriteCtx): Patch = {
      ctx.tree.collect {
        case n: scala.meta.Name => ctx.rename(n, Term.Name(n.syntax + "1"))
      }.asPatch
    }
  }

  case object MyRewrite2 extends Rewrite {
    def rewrite[T <: Mirror](ctx: RewriteCtx): Patch =
      ctx.addGlobalImport(importer"scala.collection.immutable.Seq")
  }
}
