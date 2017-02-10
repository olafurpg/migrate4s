package scalafix.rewrite
import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens
import scalafix.ScalafixConfig
import scalafix.util.AssociatedComments
import scalafix.util.TokenList
import scalafix.util.TokenOps

class RewriteCtx(val ast: Tree,
                 val config: ScalafixConfig,
                 val semantic: Option[ScalafixMirror]) {
  implicit val dialect: Dialect = config.dialect
  lazy val tokens: Tokens = ast.tokens(config.dialect)
  lazy val tokenList: TokenList = new TokenList(tokens)
  lazy val comments: AssociatedComments = AssociatedComments(tokens)
  lazy val matchingParentheses: Map[Token, Token] =
    TokenOps.getMatchingParentheses(tokens)
}

object RewriteCtx {
  def fromCode(ast: Tree,
               config: ScalafixConfig = ScalafixConfig(),
               semanticApi: Option[ScalafixMirror] = None): RewriteCtx = {
    new RewriteCtx(ast, config, semanticApi)
  }
}
