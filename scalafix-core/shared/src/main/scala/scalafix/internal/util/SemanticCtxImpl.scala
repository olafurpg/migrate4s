package scalafix
package internal.util

import scala.meta._

case class SemanticCtxImpl(
    database: Database,
    sourcepath: Sourcepath,
    classpath: Classpath)
    extends SemanticCtx {
  override def toString: String =
    s"SemanticCtx($sourcepath, $classpath, database.size=${database.entries.length})"
  override def hashCode(): Int = database.hashCode()
  private lazy val _denots: Map[Symbol, Denotation] = {
    val builder = Map.newBuilder[Symbol, Denotation]
    database.symbols.foreach(r => builder += (r.sym -> r.denot))
    builder.result()
  }
  private lazy val _names: Map[Position, ResolvedName] = {
    val builder = Map.newBuilder[Position, ResolvedName]
    def add(r: ResolvedName) = {
      builder += (r.pos -> r)
    }
    database.entries.foreach { entry =>
      entry.names.foreach(add)
      entry.sugars.foreach(sugar => sugar.names.foreach(add))
    }
    builder.result()
  }
  def symbol(position: Position): Option[Symbol] =
    _names.get(position).map(_.sym)
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
    case Term.Select(_, name @ Name(_)) => symbol(name)
    case Type.Select(_, name @ Name(_)) => symbol(name)
    case _ => symbol(tree.pos)
  }
  def denotation(symbol: Symbol): Option[Denotation] =
    _denots.get(symbol)
  def denotation(tree: Tree): Option[Denotation] =
    symbol(tree).flatMap(denotation)
  override def names: Seq[ResolvedName] = _names.values.toSeq
  def withEntries(entries: Seq[Attributes]): SemanticCtx =
    copy(database = Database(entries))
}

object SemanticCtxImpl {
  val empty: SemanticCtx =
    SemanticCtxImpl(Database(Nil), Sourcepath(Nil), Classpath(Nil))
  def load(classpath: Classpath): SemanticCtx =
    SemanticCtxImpl(Database.load(classpath), Sourcepath(Nil), classpath)
  def load(sourcepath: Sourcepath, classpath: Classpath): SemanticCtx =
    SemanticCtxImpl(Database.load(classpath, sourcepath), sourcepath, classpath)
  def load(
      database: Database,
      sourcepath: Sourcepath,
      classpath: Classpath): SemanticCtx =
    SemanticCtxImpl(database, sourcepath, classpath)
  def load(bytes: Array[Byte]): SemanticCtx =
    empty.withEntries(Database.load(bytes).entries)
}
