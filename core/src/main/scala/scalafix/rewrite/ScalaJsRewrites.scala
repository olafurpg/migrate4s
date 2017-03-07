package scalafix.rewrite
import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._
import scalafix.util.TokenPatch._
import scalafix.util.TreePatch.AddGlobalImport
import scalafix.util.TreePatch.Rename

object ScalaJsRewrites {
  val DemandJSGlobal: Rewrite[Mirror] = Rewrite[Mirror] { ctx =>
    import ctx._
    implicit val mirror = ctx.mirror
    val nativeSymbol  = Symbol("_root_.scala.scalajs.js.package.native#")
    object JsNative {
      def findNative(mods: Seq[Mod]): Option[(Mod, Option[Name])] =
        mods.collectFirst {
          case native @ Mod.Annot(ref: Ref) if ref.symbol == nativeSymbol =>
            native -> mods.collectFirst {
              case Mod.Annot(
                  Term.Apply(jsName @ Ctor.Ref.Name("JSName"), _)) =>
                jsName
            }
        }
      def unapply(defn: Defn): Option[(Mod, Option[Name])] = defn match {
        case Defn.Class(mods, _, _, _, _) => findNative(mods)
        case Defn.Object(mods, _, _) => findNative(mods)
        case _ => None
      }
    }

    val patches = tree.collect {
      case JsNative(_, Some(jsName)) => Rename(jsName, q"JSGlobal")
      case JsNative(native, None) => AddRight(native.tokens.last, " @JSGlobal")
    }

    if (patches.nonEmpty && config.imports.organize) {
      AddGlobalImport(importer"scala.scalajs.js.annotation.JSGlobal") +: patches
    } else {
      patches
    }
  }
}
