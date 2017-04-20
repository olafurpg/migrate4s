// scalafmt: {maxColumn = 100}
package scalafix
package patch

import scala.meta._
import scalafix.patch.TokenPatch._
import scalafix.patch.TreePatch._

class SyntacticPatchOps(ctx: RewriteCtx) {
  private[scalafix] def inCtx(p: Patch) = InCtx(p, ctx, None)
  def rename(from: Name, to: Name): Patch = Rename(from, to)
  def addRight(tok: Token, toAdd: String): TokenPatch = Add(tok, "", toAdd)
  def addLeft(tok: Token, toAdd: String): TokenPatch = Add(tok, toAdd, "")
}

class SemanticPatchOps(ctx: RewriteCtx, mirror: Mirror) {
  private[scalafix] def inSemanticCtx(p: Patch) = InCtx(p, ctx, Some(mirror))
  def removeGlobalImport(importer: Importer): Patch = RemoveGlobalImport(importer)
  def addGlobalImport(importer: Importer): Patch = AddGlobalImport(importer)
  def replace(from: Symbol,
              to: Term.Ref,
              additionalImports: List[Importer] = Nil,
              normalized: Boolean = true): Patch =
    Replace(from, to, additionalImports, normalized)
  def renameSymbol(from: Symbol, to: Name, normalize: Boolean = false): Patch =
    RenameSymbol(from, to, normalize)
}
