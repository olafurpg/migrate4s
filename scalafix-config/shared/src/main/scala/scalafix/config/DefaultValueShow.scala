package scalafix.config

trait DefaultValueShow[T] {
  def show(e: T): String
}

object DefaultValueShow extends LowPriorityDefaultValueShow
trait LowPriorityDefaultValueShow {
  implicit def DefaultValueShowToString[T]: DefaultValueShow[T] =
    new DefaultValueShow[T] {
      override def show(e: T): String = e.toString
    }
}
