package scalafix.util

import scala.meta._

/** Context for semantic rewrites.
  *
  * A SemanticCtx is a thin wrapper around [[scala.meta.Database]] with
  * additional in-memory indices for fast Position => Symbol and
  * Symbol => Denotation lookups.
  */
trait SemanticCtx {
  def sourcepath: Sourcepath
  def classpath: Classpath
  def database: Database
  def names: Seq[ResolvedName]
  def entries: Seq[Attributes] = database.entries
  def messages: Seq[Message] = database.messages
  def symbols: Seq[ResolvedSymbol] = database.symbols
  def sugars: Seq[Sugar] = database.sugars

  /** Lookup symbol at this position. */
  def symbol(position: Position): Option[Symbol]

  /** Lookup symbol at this tree.
    *
    * This method returns the same result as symbol(Tree.Position) in most cases
    * but handles some special cases:
    * - when tree is Term/Type.Select(_, name), query by name.position
    * - workaround for https://github.com/scalameta/scalameta/issues/1083
    */
  def symbol(tree: Tree): Option[Symbol]

  /** Lookup denotation of this symbol. */
  def denotation(symbol: Symbol): Option[Denotation]

  /** Lookup denotation of this tree.
    *
    * Shorthand method for symbol(tree).flatMap(denotation).
    */
  def denotation(tree: Tree): Option[Denotation]

  def withEntries(entries: Seq[Attributes]): SemanticCtx
}
