package info.longshore.staticanalysis

object Dsl {
  @inline class RichInt(val value: Int) {
    def :* (rhs: Int): Exp = ProductExp(IntExp(value), IntExp(rhs))
    def :+ (rhs: Int): Exp = SumExp(IntExp(value), IntExp(rhs))
    def := (rhs: Int): Exp = EqExp(IntExp(value), IntExp(rhs))
    def :> (rhs: Int): Exp = GtExp(IntExp(value), IntExp(rhs))
  }

  @inline class RichExp(val value: Exp) {
    def :* (rhs: Int): Exp = ProductExp(value, IntExp(rhs))
    def :+ (rhs: Int): Exp = SumExp(value, IntExp(rhs))
    def := (rhs: Int): Exp = EqExp(value, IntExp(rhs))
    def :> (rhs: Int): Exp = GtExp(value, IntExp(rhs))
  }

  implicit def intToRichInt(i: Int): RichInt = new RichInt(i)
  implicit def intToExp(i: Int): Exp = IntExp(i)
  implicit def expToRichExp(e: Exp): RichExp = new RichExp(e)

  def ife(value: Exp, t: Exp, f: Exp): Exp = IfExp(value, t, f)
}
