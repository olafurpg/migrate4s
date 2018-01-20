package scalafix.internal.config

import scalafix.config.Config
import scalafix.config.ConfigDecoder
import scalafix.config.Setting
import scalafix.config.Settings
import io.circe.AccumulatingDecoder
import io.circe.Decoder
import io.circe.HCursor
import io.circe.Json
import io.circe.config.parser

object CirceConfigDecoder {
  def parse(config: Config): Either[List[String], Json] = config match {
    case hocon: HoconConfig =>
      parser.parse(hocon.value).left.map { parse =>
        parse.message :: Nil
      }
    case els =>
      Left(s"Expected HoconConfig, got $els" :: Nil)
  }

  def fromDecoder[T](default: T, name: String)(
      implicit
      ev: Decoder[T => T],
      s: Settings[T]): ConfigDecoder[T] =
    fromAccumulatingDecoder(default, name)(ev.accumulating, s)

  def fromAccumulatingDecoder[T](default: T, name: String)(
      implicit
      ev: AccumulatingDecoder[T => T],
      s: Settings[T]): ConfigDecoder[T] =
    new ConfigDecoder[T] {
      override def settings: List[Setting] = s.settings
      override def decode(config: Config): Either[List[String], T] =
        parse(config).right.flatMap { json =>
          HCursor.fromJson(json).downField(name).success match {
            case Some(section) =>
              ev.apply(section).toEither match {
                case Left(errs) => Left(errs.map(_.message).toList)
                case Right(ok) => Right(ok(default))
              }
            case None => Right(default)
          }
        }
    }
}
