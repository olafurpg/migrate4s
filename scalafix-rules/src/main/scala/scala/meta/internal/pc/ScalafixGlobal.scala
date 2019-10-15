package scala.meta.internal.pc

import scala.collection.mutable
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.meta.io.AbsolutePath
import scala.reflect.io.VirtualDirectory
import java.io.File
import scala.tools.nsc.reporters.StoreReporter

class ScalafixGlobal(
    settings: Settings,
    reporter: StoreReporter
) extends Global(settings, reporter) {
  def inverseSemanticdbSymbols(sym: String): List[Symbol] = Nil
  def inverseSemanticdbSymbol(sym: String): Symbol =
    inverseSemanticdbSymbols(sym).headOption.getOrElse(NoSymbol)
  def semanticdbSymbol(sym: Symbol): String = ""
  def renamedSymbols(context: Context): Map[Symbol, Name] = Map.empty
  lazy val renameConfig: collection.Map[Symbol, Name] =
    Map[String, String](
      "scala/collection/mutable/" -> "mutable.",
      "java/util" -> "ju."
    ).map {
        case (sym, name) =>
          val nme =
            if (name.endsWith("#")) TypeName(name.stripSuffix("#"))
            else if (name.endsWith(".")) TermName(name.stripSuffix("."))
            else TermName(name)
          inverseSemanticdbSymbol(sym) -> nme
      }
      .filterKeys(_ != NoSymbol)
      .toMap

  case class ShortName(
      name: Name,
      symbol: Symbol
  ) {
    def isRename: Boolean = symbol.name != name
    def asImport: String = {
      val ident = Identifier(name)
      if (isRename) s"${Identifier(symbol.name)} => ${ident}"
      else ident
    }
    def owner: Symbol = symbol.owner
  }
  object ShortName {
    def apply(sym: Symbol): ShortName =
      ShortName(sym.name, sym)
  }

  class ShortenedNames(
      val history: mutable.Map[Name, ShortName] = mutable.Map.empty,
      val lookupSymbol: Name => List[NameLookup] = _ => Nil,
      val config: collection.Map[Symbol, Name] = Map.empty,
      val renames: collection.Map[Symbol, Name] = Map.empty,
      val owners: collection.Set[Symbol] = Set.empty
  ) {
    def this(context: Context) =
      this(lookupSymbol = { name =>
        context.lookupSymbol(name, _ => true) :: Nil
      })

    def fullname(sym: Symbol): String = {
      if (topSymbolResolves(sym)) sym.fullNameSyntax
      else s"_root_.${sym.fullNameSyntax}"
    }

    def topSymbolResolves(sym: Symbol): Boolean = {
      // Returns the package `a` for the symbol `_root_.a.b.c`
      def topPackage(s: Symbol): Symbol = {
        val owner = s.owner
        if (s.isRoot || s.isRootPackage || s == NoSymbol || s.owner.isEffectiveRoot || s == owner) {
          s
        } else {
          topPackage(owner)
        }
      }
      val top = topPackage(sym)
      nameResolvesToSymbol(top.name.toTermName, top)
    }

    def nameResolvesToSymbol(sym: Symbol): Boolean = {
      nameResolvesToSymbol(sym.name, sym)
    }
    def nameResolvesToSymbol(name: Name, sym: Symbol): Boolean = {
      lookupSymbol(name) match {
        case Nil => true
        case lookup => lookup.exists(_.symbol.isKindaTheSameAs(sym))
      }
    }

    def tryShortenName(short: ShortName): Boolean = {
      val ShortName(name, sym) = short
      history.get(name) match {
        case Some(ShortName(_, other)) =>
          if (other.isKindaTheSameAs(sym)) true
          else false
        case _ =>
          val isOk = lookupSymbol(name).filter(_ != LookupNotFound) match {
            case Nil => true
            case lookup =>
              lookup.exists(_.symbol.isKindaTheSameAs(sym))
          }
          if (isOk) {
            history(name) = short
            true
          } else {
            false // conflict, do not shorten name.
          }
      }
    }
    def tryShortenName(name: Option[ShortName]): Boolean =
      name match {
        case Some(short) =>
          tryShortenName(short)
        case _ =>
          false
      }

  }

  implicit class XtensionSymbolMetals(sym: Symbol) {
    def javaClassSymbol: Symbol = {
      if (sym.isJavaModule && !sym.hasPackageFlag) sym.companionClass
      else sym
    }
    def fullNameSyntax: String = {
      val out = new java.lang.StringBuilder
      def loop(s: Symbol): Unit = {
        if (s.isRoot || s.isRootPackage || s == NoSymbol || s.owner.isEffectiveRoot) {
          val name =
            if (s.isEmptyPackage || s.isEmptyPackageClass) TermName("_empty_")
            else if (s.isRootPackage || s.isRoot) TermName("_root_")
            else s.name
          out.append(Identifier(name))
        } else {
          loop(s.effectiveOwner.enclClass)
          out.append('.').append(Identifier(s.name))
        }
      }
      loop(sym)
      out.toString
    }
    def isLocallyDefinedSymbol: Boolean = {
      sym.isLocalToBlock && sym.pos.isDefined
    }

    def asInfixPattern: Option[String] =
      if (sym.isCase &&
        !Character.isUnicodeIdentifierStart(sym.decodedName.head)) {
        sym.primaryConstructor.paramss match {
          case (a :: b :: Nil) :: _ =>
            Some(s"${a.decodedName} ${sym.decodedName} ${b.decodedName}")
          case _ => None
        }
      } else {
        None
      }

    def isKindaTheSameAs(other: Symbol): Boolean = {
      if (other == NoSymbol) sym == NoSymbol
      else if (sym == NoSymbol) false
      else if (sym.hasPackageFlag) {
        // NOTE(olafur) hacky workaround for comparing module symbol with package symbol
        other.fullName == sym.fullName
      } else {
        other.dealiased == sym.dealiased ||
        other.companion == sym.dealiased ||
        semanticdbSymbol(other.dealiased) == semanticdbSymbol(sym.dealiased)
      }
    }

    def snippetCursor: String = sym.paramss match {
      case Nil =>
        "$0"
      case Nil :: Nil =>
        "()$0"
      case _ =>
        "($0)"
    }

    def isDefined: Boolean =
      sym != null &&
        sym != NoSymbol &&
        !sym.isErroneous

    def isNonNullaryMethod: Boolean =
      sym.isMethod &&
        !sym.info.isInstanceOf[NullaryMethodType] &&
        !sym.paramss.isEmpty

    def isJavaModule: Boolean =
      sym.isJava && sym.isModule

    def hasTypeParams: Boolean =
      sym.typeParams.nonEmpty ||
        (sym.isJavaModule && sym.companionClass.typeParams.nonEmpty)

    def requiresTemplateCurlyBraces: Boolean = {
      sym.isTrait || sym.isInterface || sym.isAbstractClass
    }
    def isTypeSymbol: Boolean =
      sym.isType ||
        sym.isClass ||
        sym.isTrait ||
        sym.isInterface ||
        sym.isJavaModule

    def dealiasedSingleType: Symbol =
      if (sym.isValue) {
        sym.info match {
          case SingleType(_, dealias) => dealias
          case _ => sym
        }
      } else {
        sym
      }
    def dealiased: Symbol =
      if (sym.isAliasType) sym.info.dealias.typeSymbol
      else sym
  }

}

object ScalafixGlobal {

  def newCompiler(
      cp: List[AbsolutePath],
      options: List[String]
  ): ScalafixGlobal = {
    val classpath = cp.mkString(File.pathSeparator)
    val vd = new VirtualDirectory("(memory)", None)
    val settings = new Settings
    settings.Ymacroexpand.value = "discard"
    settings.outputDirs.setSingleOutput(vd)
    settings.classpath.value = classpath
    settings.YpresentationAnyThread.value = true
    if (classpath.isEmpty) {
      settings.usejavacp.value = true
    }
    val (isSuccess, unprocessed) =
      settings.processArguments(options, processAll = true)
    require(isSuccess, unprocessed)
    require(unprocessed.isEmpty, unprocessed)
    new ScalafixGlobal(settings, new StoreReporter)
  }
}
