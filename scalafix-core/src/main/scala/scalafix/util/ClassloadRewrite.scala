package scalafix
package util

import scala.meta._
import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import java.lang.reflect.InvocationTargetException

import metaconfig.ConfError
import metaconfig.Configured
import org.scalameta.logger

class ClassloadRewrite[T](classLoader: ClassLoader)(implicit ev: ClassTag[T]) {
  private val t = ev.runtimeClass
  private val functionClasstag =
    implicitly[ClassTag[Function[Mirror, T]]].runtimeClass

  object ClassRewrite {
    def unapply(fqcn: String): Option[Class[_]] =
      getClassFor(fqcn).toOption
  }

  object ObjectRewrite {
    def unapply(fqcn: String): Option[Class[_]] =
      if (!fqcn.endsWith("$")) getClassFor(fqcn + "$").toOption
      else None // covered by ClassRewrite
  }
  object LambdaRewrite {
    def unapply(fqcn: String): Option[(Class[_], String)] = {
      val idx = fqcn.lastIndexOf(".")
      if (idx == -1) None
      else {
        val (obj, field) = fqcn.splitAt(idx)
        getClassFor(obj + "$").map(x => x -> field.stripPrefix(".")).toOption
      }
    }
  }

  private def getClassFor(fqcn: String): Try[Class[_]] =
    Try { Class.forName(fqcn, false, classLoader) }

  private def loadRewriteFromField(clazz: Class[_],
                                   args: Seq[AnyRef],
                                   fieldName: String): Try[T] = Try {
    val field = clazz.getDeclaredField(fieldName)
    val obj = {
      val constructor = clazz.getDeclaredConstructor()
      constructor.setAccessible(true)
      constructor.newInstance()
    }
    field.setAccessible(true)
    val rewrite = field.get(obj)
    if (t.isInstance(rewrite)) rewrite.asInstanceOf[T]
    else
      args match {
        case (mirror: Mirror) :: Nil if functionClasstag.isInstance(rewrite) =>
          rewrite.asInstanceOf[Function[Mirror, T]].apply(mirror)
        case _ =>
          throw new IllegalArgumentException(
            s"Unable to load rewrite from field $fieldName on object $obj with arguments $args")
      }
  }

  private def classloadRewriteFromConstructor(clazz: Class[_],
                                              args: Seq[AnyRef]): Try[T] =
    Try {
      val argsLen = args.length
      val constructors =
        Try(clazz.getDeclaredConstructor()).toOption.toList ++
          clazz.getDeclaredConstructors.toList

      logger.elem(clazz, clazz.getDeclaredConstructors.toList)
      val constructor = constructors
        .find(_.getParameterCount == argsLen)
        .orElse(constructors.find(_.getParameterCount == 0))
        .getOrElse {
          val argsMsg =
            if (args.isEmpty) ""
            else s" or constructor matching arguments $args"
          throw new IllegalArgumentException(
            s"""No suitable constructor on $clazz.
               |Expected : zero-argument constructor$argsMsg
               |Found    : $constructors
             """.stripMargin)
        }
      constructor.setAccessible(true)
      val obj = {
        if (constructor.getParameterCount == argsLen)
          constructor.newInstance(args: _*)
        else constructor.newInstance()
      }
      if (t.isInstance(obj)) obj.asInstanceOf[T]
      else {
        throw new ClassCastException(
          s"${clazz.getName} is not a subtype of $t")
      }
    } recover {
      case i: InvocationTargetException if i.getTargetException ne null =>
        throw i.getTargetException
    }

  def createInstanceFor(fqcn: String, args: Seq[AnyRef]): Try[T] = {
    val classRewrites =
      Seq(ClassRewrite.unapply(fqcn), ObjectRewrite.unapply(fqcn)).flatten
        .map(cls => classloadRewriteFromConstructor(cls, args))
    val lambdaRewrite =
      LambdaRewrite
        .unapply(fqcn)
        .map {
          case (cls, field) => loadRewriteFromField(cls, args, field)
        }
        .toList
    val combined = (classRewrites ++ lambdaRewrite)
    val successes = combined.collect { case Success(t) => t }
    val failures = combined.collect { case Failure(e) => e }
    if (successes.nonEmpty) Success(successes.head)
    else {

      Failure(new IllegalArgumentException(
        s"""Unable to load rewrite $fqcn with args $args. Tried the following:
           |${failures.mkString("\n")}""".stripMargin))
    }
  }
}

object ClassloadRewrite {
  def apply[T: ClassTag](fqn: String, args: Seq[AnyRef]): Configured[T] = {
    val result = new ClassloadRewrite(this.getClass.getClassLoader)
      .createInstanceFor(fqn, args)
    result match {
      case Success(e) => Configured.Ok(e)
      case Failure(e) =>
        throw e
//        Configured.NotOk(ConfError.msg(e.toString))
    }
  }
}
