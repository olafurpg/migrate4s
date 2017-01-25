package scalafix.util

import scala.collection.immutable.Seq
import scala.meta.Import
import scala.meta.Importee
import scala.meta.Importee.Wildcard
import scala.meta.Importer
import scala.meta.Term
import scala.meta.tokens.Token.Comment
import scalafix.rewrite.RewriteCtx

object CanonicalImport {
  def apply(ref: Term.Ref,
            wildcard: Wildcard,
            unimports: Seq[Importee.Unimport])(
      implicit ctx: RewriteCtx,
      ownerImport: Import
  ): CanonicalImport =
    new CanonicalImport(
      ref,
      wildcard,
      unimports,
      leadingComments = ctx.comments.leading(ownerImport),
      trailingComments = ctx.comments.trailing(ownerImport) ++
          (wildcard +: unimports).flatMap(ctx.comments.trailing)
    ) {}
  def apply(ref: Term.Ref, importee: Importee)(
      implicit ctx: RewriteCtx,
      ownerImport: Import
  ): CanonicalImport =
    new CanonicalImport(
      ref,
      importee,
      Nil,
      leadingComments = ctx.comments.leading(ownerImport),
      trailingComments = ctx.comments.trailing(ownerImport) ++
          ctx.comments.trailing(importee)
    ) {}
}

sealed case class CanonicalImport(
    ref: Term.Ref,
    importee: Importee,
    unimports: Seq[Importee.Unimport],
    leadingComments: Set[Comment],
    trailingComments: Set[Comment]
) {
  def isSpecialImport: Boolean = {
    val base = ref.syntax
    base.startsWith("scala.language") ||
    base.startsWith("scala.annotation")
  }
  def withoutLeading(leading: Set[Comment]): CanonicalImport =
    copy(leadingComments = leadingComments.filterNot(leading))
  def tree: Import = Import(Seq(Importer(ref, unimports :+ importee)))
  def syntax(implicit ctx: RewriteCtx): String =
    s"${leading}import $importerSyntax$trailing"
  def leading: String =
    if (leadingComments.isEmpty) ""
    else leadingComments.mkString("", "\n", "\n")
  def trailing: String =
    if (trailingComments.isEmpty) ""
    else trailingComments.mkString(" ", "\n", "")
  def importerSyntax(implicit ctx: RewriteCtx): String =
    s"$ref.$importeeSyntax"
  private def curlySpace(implicit ctx: RewriteCtx) =
    if (ctx.config.imports.spaceAroundCurlyBrace) " "
    else ""
  def importeeSyntax(implicit ctx: RewriteCtx): String =
    if (unimports.nonEmpty)
      s"""{$curlySpace${unimports
        .map(_.syntax)
        .mkString(", ")}, $importee$curlySpace}"""
    else
      importee match {
        case i: Importee.Rename => s"{$curlySpace$i$curlySpace}"
        case i => i.syntax
      }
  private def importeeOrder = importee match {
    case i: Importee.Rename => (1, i.name.syntax)
    case i: Importee.Wildcard => (0, i.syntax)
    case i => (1, i.syntax)
  }
  def sortOrder: (String, (Int, String)) = (ref.syntax, importeeOrder)
  def structure: String = Importer(ref, Seq(importee)).structure
}
