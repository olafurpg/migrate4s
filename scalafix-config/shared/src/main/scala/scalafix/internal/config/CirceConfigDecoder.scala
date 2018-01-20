package scalafix.internal.config

import scalafix.config.Config
import scalafix.config.ConfigDecoder
import scalafix.config.Setting
import scalafix.config.Settings
import io.circe.AccumulatingDecoder
import io.circe.Decoder
import io.circe.HCursor
import io.circe.config.parser

object CirceConfigDecoder {
  def fromDecoder[T](
      default: T,
      ev: Decoder[T => T],
      s: Settings[T]): ConfigDecoder[T] =
    new ConfigDecoder[T] {
      override def settings: List[Setting] = s.settings
      override def decode(config: Config): Either[List[String], T] =
        config match {
          case hocon: HoconConfig =>
            parser.decode[T => T](hocon.value)(ev) match {
              case Left(err) => Left(err.getMessage :: Nil)
              case Right(ok) => Right(ok(default))
            }
          case els =>
            Left(s"Expected HoconConfig, got $els" :: Nil)
        }
    }
  def fromAccumulatingDecoder[T](
      ev: AccumulatingDecoder[T],
      s: Settings[T]): ConfigDecoder[T] =
    new ConfigDecoder[T] {
      override def settings: List[Setting] = s.settings
      override def decode(config: Config): Either[List[String], T] =
        config match {
          case hocon: HoconConfig =>
            parser.parse(hocon.value) match {
              case Left(err) => Left(err.message :: Nil)
              case Right(json) =>
                ev.apply(HCursor.fromJson(json)).toEither match {
                  case Right(value) => Right(value)
                  case Left(errors) => Left(errors.toList.map(_.message))
                }
            }
          case els =>
            Left(s"Expected HoconConfig, got $els" :: Nil)
        }
    }
}
