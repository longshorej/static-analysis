package info.longshore.staticanalysis

/** tests alpha() */
object alphaSpec extends org.specs2.mutable.Specification {
  "primitive #1" >> { alpha.primitive(0) mustEqual Sign.Zero }
  "primitive #2" >> { alpha.primitive(-5) mustEqual Sign.Negative }
  "primitive #3" >> { alpha.primitive(8) mustEqual Sign.Positive }

  "int #1" >> { alpha(IntExp(5)) mustEqual Set(Sign.Positive) }
  "int #2" >> { alpha(IntExp(0)) mustEqual Set(Sign.Zero) }
  "int #3" >> { alpha(IntExp(-5)) mustEqual Set(Sign.Negative) }

  "product #1" >> { alpha(ProductExp(IntExp(5), IntExp(0))) mustEqual Set(Sign.Zero) }
  "product #2" >> { alpha(ProductExp(IntExp(5), IntExp(-0))) mustEqual Set(Sign.Zero) }
  "product #3" >> { alpha(ProductExp(IntExp(-5), IntExp(0))) mustEqual Set(Sign.Zero) }
  "product #4" >> { alpha(ProductExp(IntExp(-5), IntExp(-1))) mustEqual Set(Sign.Positive) }
  "product #5" >> { alpha(ProductExp(IntExp(5), IntExp(31))) mustEqual Set(Sign.Positive) }

  "sum #1" >> { alpha(SumExp(IntExp(5), IntExp(9))) mustEqual Set(Sign.Zero, Sign.Negative, Sign.Positive) }

  "eq #1" >> { alpha(EqExp(IntExp(0), IntExp(1))) mustEqual Set(Sign.Zero, Sign.Positive) }
  "eq #2" >> { alpha(EqExp(IntExp(1), IntExp(1))) mustEqual Set(Sign.Zero, Sign.Positive) }
}
