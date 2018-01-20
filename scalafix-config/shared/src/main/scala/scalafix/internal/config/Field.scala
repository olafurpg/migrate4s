package scalafix.internal.config

import scala.annotation.StaticAnnotation
import scala.reflect.ClassTag

case class DefaultValue[+T](value: T, show: () => String)

object DefaultValue {
  def apply[T](e: T)(implicit ev: DefaultValueShow[T]): DefaultValue[T] = {
    DefaultValue(e, () => ev.show(e))
  }
}

final case class Field(
    name: String,
    defaultValue: Option[DefaultValue[_]],
    classTag: ClassTag[_],
    annotations: List[StaticAnnotation]
)

case class Fields[T](fields: List[Field])
object Fields {
  def apply[T](implicit ev: Fields[T]): Fields[T] = ev
}
