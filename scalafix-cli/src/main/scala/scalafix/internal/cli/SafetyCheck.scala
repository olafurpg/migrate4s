package scalafix.internal.cli

sealed trait SafetyCheck

object SafetyCheck {
  case object OkWrite extends SafetyCheck
  case object DoNothing extends SafetyCheck
  case object AbortStaleDb extends SafetyCheck
}
