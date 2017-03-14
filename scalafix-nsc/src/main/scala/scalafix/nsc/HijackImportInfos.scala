package scalafix.nsc
import scala.language.higherKinds

import scala.reflect.internal.Positions
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

//class Global
