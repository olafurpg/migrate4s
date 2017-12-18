package scalafix.internal.config

import metaconfig.ConfDecoder
import org.langmeta.Symbol

import MetaconfigPendingUpstream.XtensionConfScalafix

case class NoInferConfig(
    symbols: List[Symbol.Global] = Nil,
    excludeEnclosing: List[Symbol.Global] = Nil
) {
  implicit val reader: ConfDecoder[NoInferConfig] =
    ConfDecoder.instanceF[NoInferConfig] { c =>
      (
        c.getField(symbols) |@|
          c.getField(excludeEnclosing)
      ).map {
        case (a, b) => NoInferConfig(a, b)
      }
    }
}

object NoInferConfig {
  val default = NoInferConfig()
  implicit val reader: ConfDecoder[NoInferConfig] = default.reader
}
