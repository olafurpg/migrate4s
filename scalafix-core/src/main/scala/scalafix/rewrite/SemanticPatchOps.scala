// scalafmt: {maxColumn = 100}
package scalafix
package patch

import scala.meta._
import scala.meta.tokens.Token.Space
import scalafix.patch.TokenPatch._
import scalafix.patch.TreePatch._
import org.scalameta.logger

class SyntacticPatchOps(ctx: RewriteCtx) {
  def trimToken(token: Token): Patch = {
    val toRight = ctx.tokenList.from(token).tail
    val toLeft = ctx.tokenList.to(token).reverse
    def isSpace(token: Token) = token.is[Space]
    def isNotSpace(token: Token) = !isSpace(token)
    def isNewline(token: Token) = token.is[Token.LF]
    val startsLine = toLeft.find(isNotSpace).exists(isNewline)
    val endsLine = toRight.find(isNotSpace).exists(isNewline)
    val toRemove: Seq[Token] =
      if (startsLine && endsLine)
        toLeft.find(isNewline).toList ++
          toLeft.takeWhile(isSpace) ++
          toRight.takeWhile(isSpace)
      else if (startsLine) toRight.takeWhile(isSpace)
      else if (endsLine) toLeft.takeWhile(isSpace)
      else Nil
    toRemove.map(x => Add(x, "", "", keepTok = false)).asPatch
  }
  def replaceToken(token: Token, toReplace: String): Patch =
    Add(token, "", toReplace, keepTok = false)
  def removeToken(token: Token, trim: Boolean = false): Patch = {
    val remove = Add(token, "", "", keepTok = false)
    if (trim) trimToken(token) + remove
    else remove
  }
  def rename(from: Name, to: Name): Patch = Rename(from, to)
  def addRight(tok: Token, toAdd: String): Patch = Add(tok, "", toAdd)
  def addLeft(tok: Token, toAdd: String): Patch = Add(tok, toAdd, "")
}

class SemanticPatchOps(ctx: RewriteCtx, mirror: Mirror) {
  def removeGlobalImport(importer: Importer): Patch =
    RemoveGlobalImport(importer)
  def addGlobalImport(importer: Importer): Patch =
    AddGlobalImport(importer)
  def replace(from: Symbol,
              to: Term.Ref,
              additionalImports: List[Importer] = Nil,
              normalized: Boolean = true): Patch =
    Replace(from, to, additionalImports, normalized)
  def renameSymbol(from: Symbol, to: Name, normalize: Boolean = false): Patch =
    RenameSymbol(from, to, normalize)
}
