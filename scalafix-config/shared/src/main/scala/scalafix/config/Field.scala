package scalafix.config

import scala.annotation.StaticAnnotation
import scala.reflect.ClassTag

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
