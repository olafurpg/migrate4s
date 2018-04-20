package scalafix.v1

import metaconfig.Conf
import scala.meta._
import scala.meta.contrib.AssociatedComments
import scalafix.rule.RuleName
import scalafix.util.MatchingParens
import scalafix.util.TokenList
import scala.meta.internal.{semanticdb3 => s}

sealed abstract class Patch
object Patch {}

trait Config {
  private[scalafix] def conf: Conf
}

trait ConfigError

trait RuleFactory {
  def load(conf: Config): Either[ConfigError, Rule]
}

object MyRule extends SyntacticRule("MyRule") with RuleFactory {
  override def fix(doc: Doc): Patch = ???
  override def load(conf: Config): Either[ConfigError, Rule] = Right(this)
}

abstract class Rule(name: RuleName)

abstract class SyntacticRule(name: RuleName) extends Rule(name) {
  def fix(doc: Doc): Patch
}

abstract class SemanticRule(name: RuleName) extends Rule(name) {
  def fix(doc: SemanticDoc): Patch
}

trait Sym {
  def isNone: Boolean
  def isRootPackage: Boolean
  def isGlobal: Boolean
  def isLocal: Boolean
}

object Sym {
  private[this] final class Symbol(sym: String) extends Sym {
    override def toString: String = sym
    override def isNone: Boolean = sym.isEmpty
    override def isRootPackage: Boolean = sym == "_root_."
    override def isGlobal: Boolean = !isLocal
    override def isLocal: Boolean = sym.startsWith("local")
  }
  val root: Sym = new Symbol("_root_.")
  val none: Sym = new Symbol("")
  def apply(sym: String): Sym = new Symbol(sym)
}

trait SymKind {
  def isClass: Boolean
  def isObject: Boolean
  def isTrait: Boolean
  def isDef: Boolean
  def isMacro: Boolean
  def isConstructor: Boolean
  def isType: Boolean
  def isParameter: Boolean
  def isTypeParameter: Boolean
  def isPackage: Boolean
  def isPackageObject: Boolean
}

trait SymProperties {
  def isMacro: Boolean
  def isAbstract: Boolean
  def isFinal: Boolean
  def isSealed: Boolean
  def isImplicit: Boolean
  def isLazy: Boolean
  def isCase: Boolean
  def isCovariant: Boolean
  def isContravariant: Boolean
  def isVal: Boolean
  def isVar: Boolean
  def isStatic: Boolean
  def isPrimary: Boolean
  def isEnum: Boolean
}

trait SymAccess {
  def isPrivateThis: Boolean
  def isPrivate: Option[Sym]
  def isProtectedThis: Boolean
  def isProtected: Option[Sym]
  def isPublic: Boolean
}

trait SymInfo {
  def sym: Sym
  def owner: Sym
  def name: String
  def kind: SymKind
  def props: SymProperties
  def access: SymAccess
  private[scalafix] def tpe: s.Type
}

trait Doc {
  def tree: Tree
  def tokens: Tokens
  def input: Input
  def matchingParens: MatchingParens
  def tokenList: TokenList
  def comments: AssociatedComments
}

trait SemanticDoc extends Doc {
  def symbol(tree: Tree): Sym
  def info(symbol: Sym): SymInfo
}

trait SymMatcher {
  def matches(sym: Sym): Boolean
  def matches(tree: Tree): Boolean

  final def unapply(sym: Sym): Boolean = matches(sym)
  final def unapply(tree: Tree): Boolean = matches(tree)
}
