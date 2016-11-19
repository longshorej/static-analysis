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

      val (lz, ln, lp) =
        ls.foldLeft((true, true, true)) { case (a, e) =>
          (a._1 && e == Sign.Zero, a._2 && e == Sign.Negative, a._3 && e == Sign.Positive)
        }

      val (rz, rn, rp) =
        rs.foldLeft((true, true, true)) { case (a, e) =>
          (a._1 && e == Sign.Zero, a._2 && e == Sign.Negative, a._3 && e == Sign.Positive)
        }

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
