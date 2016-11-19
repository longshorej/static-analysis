package info.longshore.staticanalysis

class ExpSpec extends org.specs2.mutable.Specification {
  "IntExp.eval #1" >> { IntExp(3).eval mustEqual 3 }
  "IntExp.eval #2" >> { IntExp(0).eval mustEqual 0 }
  "IntExp.eval #3" >> { IntExp(-124).eval mustEqual -124 }

  "ProductExp.eval #1" >> { ProductExp(IntExp(3), IntExp(1)).eval mustEqual 3 }
  "ProductExp.eval #2" >> { ProductExp(IntExp(0), IntExp(1)).eval mustEqual 0 }
  "ProductExp.eval #3" >> { ProductExp(IntExp(-10), IntExp(10)).eval mustEqual -100 }

  "SumExp.eval #1" >> { SumExp(IntExp(3), IntExp(4)).eval mustEqual 7 }
  "SumExp.eval #2" >> { SumExp(IntExp(3), IntExp(0)).eval mustEqual 3 }
  "SumExp.eval #3" >> { SumExp(IntExp(-3), IntExp(3)).eval mustEqual 0 }

  "EqExp.eval #1" >> { EqExp(IntExp(5), IntExp(-5)).eval mustEqual 0 }
  "EqExp.eval #2" >> { EqExp(IntExp(5), IntExp(5)).eval mustEqual 1 }
  "EqExp.eval #3" >> { EqExp(IntExp(0), IntExp(0)).eval mustEqual 1 }
}
