package scalafix.util

import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import java.lang.reflect.InvocationTargetException

import metaconfig.ConfError
import metaconfig.Configured

class ClassloadObject[T](
    classLoader: ClassLoader,
    guessDollarSuffix: Boolean = true)(implicit ev: ClassTag[T]) {
  private val t = ev.runtimeClass

  private def getClassFor(fqcn: String): Try[Class[_]] =
    Try {
      val c = Class.forName(fqcn, false, classLoader)
      def fail() =
        throw new ClassCastException(s"$t is not assignable from $c")
      if (t.isAssignableFrom(c)) c
      else if (guessDollarSuffix && !fqcn.endsWith("$")) {
        // support skipping the trailing $ for case object rewrites.
        getClassFor(fqcn + "$").getOrElse(fail())
      } else fail()
    }

  private def createInstanceFor(clazz: Class[_], args: Seq[AnyRef]): Try[T] =
    Try {
      val argsLen = args.length
      val constructors = clazz.getDeclaredConstructors()
      val constructor = constructors
        .find(_.getParameterCount == argsLen)
        .orElse(constructors.find(_.getParameterCount == 0))
        .getOrElse {
          val argsMsg =
            if (args.isEmpty) "" else s" or matching arguments $args"
          throw new IllegalArgumentException(
            s"""Found suitable constructor on $clazz.
               |Expected : zero-argument constructor$argsMsg
               |Found    : ${constructors.toList}
             """.stripMargin)
        }
      constructor.setAccessible(true)
      val obj = {
        if (constructor.getParameterCount == argsLen)
          constructor.newInstance(args: _*)
        else constructor.newInstance()
      }
      if (t.isInstance(obj)) obj.asInstanceOf[T]
      else
        throw new ClassCastException(
          s"${clazz.getName} is not a subtype of $t")
    } recover {
      case i: InvocationTargetException if i.getTargetException ne null ⇒
        throw i.getTargetException
    }

  def createInstanceFor(fqcn: String, args: Seq[AnyRef]): Try[T] =
    getClassFor(fqcn).flatMap(c => createInstanceFor(c, args))
}

object ClassloadObject {
  def apply[T: ClassTag](fqn: String, args: Seq[AnyRef]): Configured[T] = {
    val result = new ClassloadObject(this.getClass.getClassLoader)
      .createInstanceFor(fqn, args)
    result match {
      case Success(e) => Configured.Ok(e)
      case Failure(e) => Configured.NotOk(ConfError.msg(e.toString))
    }
  }
}
