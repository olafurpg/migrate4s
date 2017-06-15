package scalafix.rewrite

import scala.collection.immutable.Seq
import scala.meta.inputs.Input
import scala.meta.io.Classpath
import scala.meta.semantic.Attributes
import scala.meta.semantic.Database
import scala.meta.semantic.Mirror
import scalafix.syntax._

case class ScalafixDatabase(database: Database, dependencyClasspath: Classpath)
    extends Mirror {
  override def toString: String = database.toString()
  def filterEntries(f: ((Input, Attributes)) => Boolean): ScalafixDatabase = {
    ScalafixDatabase(Database(entries.filter(f)), dependencyClasspath)
  }
  def forInput(input: Input): ScalafixDatabase = filterEntries(_._1 == input)
  def entries: Seq[(Input, Attributes)] = database.entries
}

object ScalafixDatabase {
  def empty = ScalafixDatabase(Database(Nil), Classpath(Nil))
}
