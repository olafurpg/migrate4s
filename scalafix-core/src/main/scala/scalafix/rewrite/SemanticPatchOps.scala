package scalafix
package rewrite
import scala.meta._
import scalafix.util.TreePatch._
import scalafix.util._

trait SyntacticPatchOps {
  def rename(from: Name, to: Name): Patch = Rename(from, to)
}

trait SemanticPatchOps extends SyntacticPatchOps {
  implicit def mirror: Mirror
  implicit def ctx: RewriteCtx
  def addGlobalImport(importer: Importer): Patch = AddGlobalImport(importer)
}
