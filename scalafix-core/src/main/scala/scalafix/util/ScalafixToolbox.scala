package scalafix.util

import scala.{meta => m}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.util.AbstractFileClassLoader
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.reflect.ToolBoxError
import scalafix.rewrite.Rewrite

import java.io.File
import java.math.BigInteger
import java.security.MessageDigest

import metaconfig.ConfError
import metaconfig.Configured

object ScalafixToolbox {
  import scala.reflect.runtime.universe._
  import scala.tools.reflect.ToolBox
  private val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  private val rewriteCache: mutable.WeakHashMap[String, Any] =
    mutable.WeakHashMap.empty
  val compiler = new Compiler(None)

  def getRewrite(code: String, mirror: Option[m.Mirror]): Configured[Rewrite] =
    try {
      compiler.compile(code)
      ClassloadRewrite("foo.bar.MyRewrite", mirror.toList)
    } catch {
      case e: ToolBoxError =>
        ConfError.msg(e.getMessage).notOk
    }

  private def compile(code: String): Any = {
    rewriteCache.getOrElseUpdate(
      code, {
        val cls = compiler.compile(code)
//        tb.eval(tb.parse(code))
      }
    )
  }
}

class Compiler(targetDir: Option[File]) {

  val target = targetDir match {
    case Some(dir) => AbstractFile.getDirectory(dir)
    case None => new VirtualDirectory("(memory)", None)
  }

  val classCache = mutable.Map[String, Class[_]]()

  private val settings = new Settings()
  settings.deprecation.value = true // enable detailed deprecation warnings
  settings.unchecked.value = true // enable detailed unchecked warnings
  settings.outputDirs.setSingleOutput(target)
  settings.usejavacp.value = true

  private val global = new Global(settings)
  private lazy val run = new global.Run

  val classLoader =
    new AbstractFileClassLoader(target, this.getClass.getClassLoader)

  def compile(code: String): Unit = {
    val className = classNameForCode(code)
    findClass(className).getOrElse {
      val sourceFiles = List(new BatchSourceFile("(inline)", code))
      run.compileSources(sourceFiles)
    }
  }

  def findClass(className: String): Option[Class[_]] = {
    synchronized {
      classCache.get(className).orElse {
        try {
          val cls = classLoader.loadClass(className)
          classCache(className) = cls
          Some(cls)
        } catch {
          case e: ClassNotFoundException => None
        }
      }
    }
  }

  protected def classNameForCode(code: String): String = {
    val digest = MessageDigest.getInstance("SHA-1").digest(code.getBytes)
    "sha" + new BigInteger(1, digest).toString(16)
  }

}
