package scalafix.util

import scala.meta._
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

object TokenOps {

  /**
    * Finds matching parens [({})].
    *
    * Contains lookup keys in both directions, opening [({ and closing })].
    */
  def getMatchingParentheses(tokens: Tokens): Map[Token, Token] = {
    val ret = Map.newBuilder[Token, Token]
    var stack = List.empty[Token]
    tokens.foreach {
      case open @ (LeftBrace() | LeftBracket() | LeftParen() |
          Interpolation.Start()) =>
        stack = open :: stack
      case close @ (RightBrace() | RightBracket() | RightParen() |
          Interpolation.End()) =>
        val open = stack.head
        assertMatchingParens(open, close)
        ret += open -> close
        ret += close -> open
        stack = stack.tail
      case _ =>
    }
    val result = ret.result()
    result
  }
  def assertMatchingParens(open: Token, close: Token): Unit = {
    (open, close) match {
      case (Interpolation.Start(), Interpolation.End()) =>
      case (LeftBrace(), RightBrace()) =>
      case (LeftBracket(), RightBracket()) =>
      case (LeftParen(), RightParen()) =>
      case (o, c) =>
        throw new IllegalArgumentException(s"Mismatching parens ($o, $c)")
    }
  }
}
