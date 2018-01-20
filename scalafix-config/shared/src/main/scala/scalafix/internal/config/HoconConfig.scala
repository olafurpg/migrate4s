package scalafix.internal.config

import scalafix.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.{Config => Hocon}

class HoconConfig(val value: Hocon) extends Config

object HoconConfig {
  def fromString(string: String): Config =
    new HoconConfig(ConfigFactory.parseString(string))
  def fromHocon(hocon: Hocon): Config = new HoconConfig(hocon)
}