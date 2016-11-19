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
      val ls = apply(l)
      val rs = apply(r)

      if (ls.forall(_ == Sign.Zero) || rs.forall(_ == Sign.Zero))
        Set(Sign.Zero)
      else if (ls.forall(_ == Sign.Negative) ^ rs.forall(_ == Sign.Negative))
        Set(Sign.Negative)
      else if (ls.forall(_ == Sign.Positive) && rs.forall(_ == Sign.Positive))
        Set(Sign.Positive)
      else
        Set(Sign.Zero, Sign.Negative, Sign.Positive)

    case EqExp(l, r)      =>
      Set(Sign.Positive, Sign.Zero)

    case SumExp(l, r)     =>
      Set(Sign.Zero, Sign.Negative, Sign.Positive)

    case GtExp(l, r)      =>
      Set(Sign.Positive, Sign.Zero)

    case IfExp(c, t, f)   =>
      apply(t) ++ apply(f)
  }
}
