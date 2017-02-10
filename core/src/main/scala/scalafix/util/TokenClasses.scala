package scalafix.util

import scala.meta.internal.classifiers.classifier
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

@classifier
trait Whitespace
object Whitespace {
  def unapply(token: Token): Boolean = {
    token.is[Space] || token.is[Tab] || token.is[CR] || token.is[LF] ||
    token.is[FF]
  }
}

@classifier
trait Trivia
object Trivia {
  def unapply(token: Token): Boolean = {
    token.is[Whitespace] || token.is[Comment]
  }
}

@classifier
trait CloseDelim
object CloseDelim {
  def unapply(token: Token): Boolean =
    token.is[RightBrace] ||
      token.is[RightParen] ||
      token.is[RightBracket]
}

@classifier
trait OpenDelim
object OpenDelim {
  def unapply(token: Token): Boolean =
    token.is[LeftBrace] ||
      token.is[LeftParen] ||
      token.is[LeftBracket]
}

@classifier
trait LineEnd
object LineEnd {
  def unapply(token: Token): Boolean =
    token.is[LF] || token.is[CR]
}
