package scalafix.internal.rule

import scala.collection.mutable
import scalafix._
import scala.meta._

case object SAM extends Rule("SAM") {
  override def fix(ctx: RuleCtx): Patch = {
    val patches = ctx.tree.collect {
      case anon @ Term.NewAnonymous(
            Template(
              Nil,
              init :: Nil,
              _,
              Defn.Def(_, _, _, paramss, _, body) :: Nil))
          if paramss.lengthCompare(1) <= 0 =>
        val paramList = paramss match {
          case Nil => "() => "
          case params :: Nil => params.map(_.name).mkString("(", ", ", ") => ")
          case _ => sys.error("impossible")
        }
        var first = true
        val ok = mutable.Set.empty[Token]
        val nok = mutable.Set.empty[Token]
        val remove = anon.tokens.filterNot {
          case t: Token.LF if first =>
            first = false
            nok ++= ctx.tokenList.trailingSpaces(t).take(2)
            false
          case token =>
            body.tokens.contains(token) && !nok(token)
        }
        ctx.removeTokens(remove) + ctx.addLeft(body, paramList)
    }

    // val x = new Foo { def bar(): Unit = ??? }
    // val x: Foo = () => ???

    patches.asPatch
  }
}
