package scalafix.internal.util

import scala.meta.Position

object ScalametaEnrichments {
  implicit class XtensionPositionScalafix(val pos: Position) extends AnyVal {
    def endOffset: Position = pos match {
      case Position.None => pos
      case Position.Range(input, _, end) =>  Position.Range(input, end, end)
    }
  }
}
