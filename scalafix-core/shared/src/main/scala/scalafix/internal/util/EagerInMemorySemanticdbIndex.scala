package scalafix
package internal.util

import scala.meta._
import ScalametaEnrichments._

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
  def synthetic(pos: Position): Option[Synthetic] =
    _synthetics.get(pos).orElse(_synthetics.get(pos.endOffset))
  def desugar(tree: Tree): Tree =
    DesugarOps.desugarTree(tree)(this)
  override def names: Seq[ResolvedName] = _names.values.toSeq
  def withDocuments(documents: Seq[Document]): SemanticdbIndex =
    copy(database = Database(documents))
}
