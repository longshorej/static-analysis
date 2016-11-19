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

      val lz = ls.forall(_ == Sign.Zero)
      val rz = rs.forall(_ == Sign.Zero)

      val ln = ls.forall(_ == Sign.Negative)
      val rn = rs.forall(_ == Sign.Negative)

      val lp = ls.forall(_ == Sign.Positive)
      val rp = ls.forall(_ == Sign.Positive)

      if (lz || rz)
        Set(Sign.Zero)
      else if (ln ^ rn)
        Set(Sign.Negative)
      else if (lp && rp)
        Set(Sign.Positive)
      else if (ln && rn)
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
