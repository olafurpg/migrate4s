package scalafix.config

import scala.language.experimental.macros

import scalafix.internal.config.Macros

/**
  *  Dummy type for a config object.
  *
  *  The public api provides no means of inspection on Config.
  *  Users should not manipulate Config directly, but use derived ConfigDecoder[T]
  *  instead.
  */
trait Config

trait ConfigDecoder[T] {
  def decode(config: Config): Either[List[String], T]
  def settings: List[Setting]
}

object ConfigDecoder {
  def derive[T](default: T, name: String): ConfigDecoder[T] =
    macro Macros.deriveConfigImpl[T]
}
