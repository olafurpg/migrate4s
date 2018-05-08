package scalafix.internal.patch

import scala.meta._
import scala.meta.internal.trees._
import scalafix._
import scalafix.internal.util.SymbolOps.Root
import scalafix.internal.util.SymbolOps.SignatureName
import scalafix.patch.Patch
import scalafix.patch.TreePatch.ReplaceSymbol
import scalafix.syntax._
import scalafix.v1.SemanticDoc
import scalafix.v1.Sym

object ReplaceSymbolOps {
  private object Select {
    def unapply(arg: Ref): Option[(Ref, Name)] = arg match {
      case Term.Select(a: Ref, b) => Some(a -> b)
      case Type.Select(a, b) => Some(a -> b)
      case _ => None
    }
  }

  def naiveMoveSymbolPatch(moveSymbols: Seq[ReplaceSymbol])(
      implicit doc: SemanticDoc): Patch = {
    val moves: Map[Symbol, Symbol.Global] = moveSymbols.toIterator
      .map(_.msymbols)
      .flatMap {
        case (
            term @ Symbol.Global(qual, Signature.Method(name, _)),
            to: Symbol.Global) =>
          (term -> to) :: Nil
        case (
            term @ Symbol.Global(qual, Signature.Term(name)),
            to: Symbol.Global) =>
          (term -> to) ::
            (Symbol.Global(qual, Signature.Type(name)) -> to) ::
            Nil
      }
      .toMap
    def loop(ref: Ref, sym: Symbol): (Patch, Symbol) = {
      (ref, sym) match {
        // same length
        case (a @ Name(_), Symbol.Global(Symbol.None, SignatureName(b))) =>
          Patch.replaceTree(a, b) -> Symbol.None
        // ref is shorter
        case (a @ Name(_), sym @ Symbol.Global(qual, SignatureName(b))) =>
          Patch.replaceTree(a, b) -> sym
        // ref is longer
        case (
            Select(qual, Name(_)),
            Symbol.Global(Symbol.None, SignatureName(b))) =>
          Patch.replaceTree(qual, b) -> Symbol.None
        // recurse
        case (
            Select(qual: Ref, a @ Name(_)),
            Symbol.Global(symQual, SignatureName(b))) =>
          val (patch, toImport) = loop(qual, symQual)
          (patch + Patch.replaceTree(a, b)) -> toImport
      }
    }
    object Move {
      def unapply(name: Name): Option[Symbol.Global] = {
        val result = name.syms.map(_.msymbol).collectFirst {
          case x if moves.contains(x) => moves(x)
          case x if moves.contains(x.normalized) => moves(x.normalized)
        }
        result
      }
    }
    val patches = doc.tree.collect {
      case n @ Move(to) =>
        // was this written as `to = "blah"` instead of `to = _root_.blah`
        val isSelected = Root.unapply(to).isEmpty
        n.parent match {
          case Some(i @ Importee.Name(_)) =>
            Patch.removeImportee(i)
          case Some(parent @ Select(_, `n`)) if isSelected =>
            val (patch, imp) = loop(parent, to)
            Patch.addGlobalImport(imp) + patch
          case Some(_) =>
            val addImport =
              if (n.isDefinition) Patch.empty
              else Patch.addGlobalImport(to)
            addImport + Patch.replaceTree(n, to.signature.name)
        }
    }
    patches.asPatch
  }
}
