package scalafix.nsc
import scala.language.higherKinds

import scala.collection.mutable
import scala.reflect.internal.Positions
import scala.reflect.internal.util.NoPosition
import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.typechecker.Contexts

import org.scalameta.logger

trait HijackImportInfos { self: ReflectToolkit =>
  val allUsedSelectors
    : NonRemovableMap[g.analyzer.ImportInfo, Set[g.ImportSelector]] =
    new NonRemovableMap[g.analyzer.ImportInfo, Set[g.ImportSelector]](Set())
  val allImportInfos
    : NonRemovableMap[g.CompilationUnit, List[g.analyzer.ImportInfo]] =
    new NonRemovableMap[g.CompilationUnit, List[g.analyzer.ImportInfo]](Nil)

  /** overrides private lazy maps in g.analyzer's Contexts with custom maps */
  def hijackImportInfos(): Unit = {
    def hijackLazyField[T](name: String, value: T): T = {
      val clazz = g.analyzer.asInstanceOf[Contexts].getClass
      val field = clazz.getDeclaredFields.find(_.getName endsWith name).get
      val method = clazz.getDeclaredMethods.find(_.getName endsWith name).get
      field.setAccessible(true)
      method.invoke(g.analyzer) // invoke lazy mechanism before setting field.
      field.set(g.analyzer, value)
      value
    }
    hijackLazyField("allUsedSelectors", allUsedSelectors)
    hijackLazyField("allImportInfos", allImportInfos)
//    hijackValidatePosition()
    hijackReporter()
  }


  def hijackReporter(): Unit = {
    val scalafixReporter = new ScalahostReporter(g.reporter)
    val field = g.getClass.getDeclaredField("reporter")
    field.setAccessible(true)
    field.set(g, scalafixReporter)
    g.reporter.info(NoPosition, "HELLO!", true)
  }

  def hijackValidatePosition(): Unit = {
    g.reporter.hasErrors
    g.reporter.cancelled = true
    logger.elem(g.reporter.cancelled)
//    logger.elem(cancelled)
//    val validatePositionsMethod = g
//      .asInstanceOf[Positions]
//      .getClass
//      .getMethod("validatePositions", classOf[g.Tree])
//    validatePositionsMethod.setAccessible(true)
//
//    logger.elem(validatePositionsMethod)
  }
}

case class CompilerMessage(severity: Int, file: String, offset: Int, message: String)
//class Global
class ScalahostReporter(other: Reporter) extends Reporter {
  val messages = mutable.ListBuffer.empty[CompilerMessage]
  override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    logger.elem(msg)
    if (!pos.isEmpty && pos.source != null) {
      messages += new CompilerMessage(severity.id, pos.source.path, pos.point, msg)
    }
    severity.id match {
      case 0 => other.info(pos, msg, force)
      case 1 => other.warning(pos, msg)
      case 2 => other.error(pos, msg)
      case _ =>
    }
  }
}
