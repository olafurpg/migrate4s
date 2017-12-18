package scalafix
package internal.util

import scala.collection.mutable
import scala.meta._
import scalafix.util.SymbolMatcher

case class EagerInMemorySemanticdbIndex(
    database: Database,
    sourcepath: Sourcepath,
    classpath: Classpath)
    extends SemanticdbIndex {
  override def toString: String =
    s"$productPrefix($sourcepath, $classpath, database.size=${database.documents.length})"
  override def hashCode(): Int = database.hashCode()
  private lazy val _denots: Map[Symbol, Denotation] = {
    val builder = Map.newBuilder[Symbol, Denotation]
    database.symbols.foreach(r => builder += (r.symbol -> r.denotation))
    builder.result()
  }
  private lazy val _names: Map[Position, ResolvedName] = {
    val builder = Map.newBuilder[Position, ResolvedName]
    def add(r: ResolvedName) = {
      builder += (r.position -> r)
    }
    database.documents.foreach { entry =>
      entry.names.foreach(add)
      entry.synthetics.foreach(_.names.foreach(add))
      entry.symbols.foreach(_.denotation.names.foreach(add))
    }
    builder.result()
  }
  private lazy val _synthetics: Map[Position, Synthetic] = {
    database.synthetics.iterator.map(s => s.position -> s).toMap
  }
  def symbol(position: Position): Option[Symbol] =
    _names.get(position).map(_.symbol)
  def symbol(tree: Tree): Option[Symbol] = tree match {
    case name @ Name(_) =>
      val syntax = name.syntax
      // workaround for https://github.com/scalameta/scalameta/issues/1083
      val pos =
        if (syntax.startsWith("(") &&
          syntax.endsWith(")") &&
          syntax != name.value)
          Position.Range(name.pos.input, name.pos.start + 1, name.pos.end - 1)
        else name.pos
      symbol(pos)
    case Importee.Rename(name, _) => symbol(name)
    case Importee.Name(name) => symbol(name)
    case Term.ApplyType(fun, _) => symbol(fun)
    case Term.Apply(fun, _) => symbol(fun)
    case Term.Select(_, name @ Name(_)) => symbol(name)
    case Type.Select(_, name @ Name(_)) => symbol(name)
    case _ => symbol(tree.pos)
  }
  def denotation(symbol: Symbol): Option[Denotation] =
    _denots.get(symbol)
  def denotation(tree: Tree): Option[Denotation] =
    symbol(tree).flatMap(denotation)
  private[this] val Star =
    SymbolMatcher.exact(scala.meta.Symbol("_star_."))(this)
  val isUsedSynthetic = mutable.Set.empty[Position]
  def desugar(tree: Tree): Tree = {
    def desugarOnce(tree: Tree): Option[Tree] = {
      for {
        synthetic <- _synthetics
          .get(tree.pos)
          .orElse {
            tree.pos match {
              case Position.Range(input, _, end) =>
                _synthetics.get(Position.Range(input, end, end))
              case _ => None
            }
          }
        if !isUsedSynthetic(synthetic.position)
        syntheticTerm <- synthetic.input.parse[Term].toOption
      } yield {
        val merged = syntheticTerm.transform {
          case Star(y: Term.Name) => tree
        }
        isUsedSynthetic += synthetic.position
        merged
      }
    }
    object Desugared {
      def unapply(tree: Tree): Option[Tree] =
        desugarOnce(tree)
    }
    val isDone = mutable.Set.empty[Position]
    val DesugaringTransformer = new Transformer {
      override def apply(tree: Tree): Tree = tree match {
        case base @ Desugared(desugared) if !isDone(base.pos) =>
          isDone += base.pos
          super.apply(desugared)
        case t => super.apply(t)
      }
    }
    DesugaringTransformer(tree)
  }
  override def names: Seq[ResolvedName] = _names.values.toSeq
  def withDocuments(documents: Seq[Document]): SemanticdbIndex =
    copy(database = Database(documents))
}
