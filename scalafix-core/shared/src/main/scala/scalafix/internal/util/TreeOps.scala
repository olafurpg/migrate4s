package scalafix.internal.util

import scala.annotation.tailrec
import scala.meta._

object TreeOps {
  @tailrec def symbolPos(tree: Tree): Position = tree match {
    case name @ Name(_) =>
      val syntax = name.syntax
      // workaround for https://github.com/scalameta/scalameta/issues/1083
      if (syntax.startsWith("(") &&
        syntax.endsWith(")") &&
        syntax != name.value) {
        Position.Range(name.pos.input, name.pos.start + 1, name.pos.end - 1)
      } else {
        name.pos
      }
    case m: Member => symbolPos(m.name)
    case t: Term.Interpolate => symbolPos(t.prefix)
    case t: Term.Apply => symbolPos(t.fun)
    case t: Term.ApplyInfix => symbolPos(t.op)
    case t: Term.ApplyUnary => symbolPos(t.op)
    case t: Term.ApplyType => symbolPos(t.fun)
    case t: Term.Assign => symbolPos(t.lhs)
    case t: Term.Ascribe => symbolPos(t.expr)
    case t: Term.Annotate => symbolPos(t.expr)
    case t: Term.New => symbolPos(t.init)
    case t: Type.Select => symbolPos(t.name)
    case t: Type.Project => symbolPos(t.name)
    case t: Type.Singleton => symbolPos(t.ref)
    case t: Type.Apply => symbolPos(t.tpe)
    case t: Type.ApplyInfix => symbolPos(t.op)
    case t: Type.Annotate => symbolPos(t.tpe)
    case t: Type.ByName => symbolPos(t.tpe)
    case t: Type.Repeated => symbolPos(t.tpe)
    case t: Pat.Bind => symbolPos(t.lhs)
    case t: Pat.Extract => symbolPos(t.fun)
    case t: Pat.ExtractInfix => symbolPos(t.op)
    case t: Pat.Interpolate => symbolPos(t.prefix)
    case Defn.Val(_, p :: Nil, _, _) => symbolPos(p)
    case Decl.Val(_, p :: Nil, _) => symbolPos(p)
    case Defn.Var(_, p :: Nil, _, _) => symbolPos(p)
    case Decl.Var(_, p :: Nil, _) => symbolPos(p)
    case t: Importee.Rename => symbolPos(t.name)
    case t: Importee.Name => symbolPos(t.name)
    case Importer(_, i :: Nil) => symbolPos(i)
    case t: Init => symbolPos(t.tpe)
    case t: Ctor.Primary => symbolPos(t.name)
    case t: Ctor.Secondary => symbolPos(t.name)
    case t: Mod.Annot => symbolPos(t.init)
    case _ => tree.pos
  }

}
