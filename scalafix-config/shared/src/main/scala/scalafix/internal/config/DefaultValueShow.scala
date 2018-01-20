package scalafix.internal.config

trait DefaultValueShow[T] {
  def show(e: T): String
}

trait LowPriorityDefaultValueShow {
  implicit def DefaultValueShowToString[T]: DefaultValueShow[T] =
    new DefaultValueShow[T] {
      override def show(e: T): String = e.toString
    }
}

object DefaultValueShow extends LowPriorityDefaultValueShow
