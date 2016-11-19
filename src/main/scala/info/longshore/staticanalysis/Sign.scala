package info.longshore.staticanalysis

sealed trait Sign

object Sign {
  case object Positive extends Sign

  case object Negative extends Sign

  case object Zero extends Sign
}
