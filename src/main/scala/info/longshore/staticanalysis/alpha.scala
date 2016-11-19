package info.longshore.staticanalysis

object alpha {
  def primitive(i: Int): Sign =
    if (i < 0) Sign.Negative
    else if (i > 0) Sign.Positive
    else Sign.Zero

  def apply(exp: Exp): Set[Sign] = exp match {
    case IntExp(v)        =>
      Set(primitive(v))

    case ProductExp(l, r) =>
      val ls = primitive(eval(l))
      val rs = primitive(eval(r))

      if (ls == Sign.Zero || rs == Sign.Zero) Set(Sign.Zero)
      else if (ls == Sign.Negative ^ rs == Sign.Negative) Set(Sign.Negative)
      else Set(Sign.Positive)

    case EqExp(l, r)      =>
      Set(Sign.Positive, Sign.Zero)

    case SumExp(l, r)     =>
      Set(Sign.Zero, Sign.Negative, Sign.Positive)
  }
}
