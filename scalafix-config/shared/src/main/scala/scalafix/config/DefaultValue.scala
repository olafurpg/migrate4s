package scalafix.config

case class DefaultValue[+T](value: T, show: () => String)

object DefaultValue {
  def apply[T](e: T)(implicit ev: DefaultValueShow[T]): DefaultValue[T] = {
    DefaultValue(e, () => ev.show(e))
  }
}