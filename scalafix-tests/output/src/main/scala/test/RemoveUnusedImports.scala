package test

import scala.util.control.NonFatal
import scala.concurrent.Future
import scala.util.{Properties, Try}
import scala.util.{Success => Successful}
import scala.concurrent.ExecutionContext
import scala.runtime.{RichBoolean}
import scala.concurrent.// formatting caveat
TimeoutException

//import scala.concurrent.{
//    CancellationException
//  , ExecutionException
//  , TimeoutException // ERROR
//}
import RemoveUnusedImports.Unused.b, RemoveUnusedImports.Unused.d

object RemoveUnusedImports {
  val NonFatal(a) = new Exception
  Future.successful(1)
  println(Properties.ScalaCompilerVersion)
  Try(1)
  Successful(1)
  ExecutionContext.defaultReporter
  new RichBoolean(true)
  new TimeoutException
  println(b + d)
  object Unused {
    val b = 1
    val c = 1
    val d = 1
  }
}
