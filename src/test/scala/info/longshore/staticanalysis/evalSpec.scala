package info.longshore.staticanalysis

/** tests eval() */
object evalSpec extends org.specs2.mutable.Specification {
  "IntExp) #1" >> { eval(IntExp(3)) mustEqual 3 }
  "IntExp) #2" >> { eval(IntExp(0)) mustEqual 0 }
  "IntExp) #3" >> { eval(IntExp(-124)) mustEqual -124 }

  "ProductExp) #1" >> { eval(ProductExp(IntExp(3), IntExp(1))) mustEqual 3 }
  "ProductExp) #2" >> { eval(ProductExp(IntExp(0), IntExp(1))) mustEqual 0 }
  "ProductExp) #3" >> { eval(ProductExp(IntExp(-10), IntExp(10))) mustEqual -100 }

  "SumExp) #1" >> { eval(SumExp(IntExp(3), IntExp(4))) mustEqual 7 }
  "SumExp) #2" >> { eval(SumExp(IntExp(3), IntExp(0))) mustEqual 3 }
  "SumExp) #3" >> { eval(SumExp(IntExp(-3), IntExp(3))) mustEqual 0 }

  "EqExp) #1" >> { eval(EqExp(IntExp(5), IntExp(-5))) mustEqual 0 }
  "EqExp) #2" >> { eval(EqExp(IntExp(5), IntExp(5))) mustEqual 1 }
  "EqExp) #3" >> { eval(EqExp(IntExp(0), IntExp(0))) mustEqual 1 }
}
