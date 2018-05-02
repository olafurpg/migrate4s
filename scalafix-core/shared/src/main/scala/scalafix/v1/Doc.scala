package scalafix.v1

import scala.meta.Input
import scala.meta.Tokens
import scala.meta.Tree
import scala.meta.contrib.AssociatedComments
import scalafix.util.MatchingParens
import scalafix.util.TokenList

final class Doc private[scalafix] (
    val tree: Tree,
    val tokens: Tokens,
    val input: Input,
    val matching: MatchingParens,
    val tokenList: TokenList,
    val comments: AssociatedComments
)
object Doc {
  def apply(tree: Tree): Doc = {
    val tokens = tree.tokens
    val input = tokens.headOption match {
      case Some(token) => token.input
      case _ => Input.None
    }
    new Doc(
      tree = tree,
      tokens = tokens,
      input = input,
      matching = MatchingParens(tokens),
      tokenList = TokenList(tokens),
      comments = AssociatedComments(tokens)
    )
  }
}
