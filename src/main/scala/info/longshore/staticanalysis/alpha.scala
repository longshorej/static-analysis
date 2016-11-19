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
      val (ls, lz, ln, lp, rs, rz, rn, rp) = lrInfo(l, r)

      if (lz || rz)      Set(Sign.Zero)
      else if (ln ^ rn)  Set(Sign.Negative)
      else if (lp && rp) Set(Sign.Positive)
      else if (ln && rn) Set(Sign.Positive)
      else               Set(Sign.Zero, Sign.Negative, Sign.Positive)

    case EqExp(l, r)      =>
      val (ls, lz, ln, lp, rs, rz, rn, rp) = lrInfo(l, r)

      if (lz && rz)      Set(Sign.Positive)
      else if (lz != rz) Set(Sign.Zero)
      else               Set(Sign.Positive, Sign.Zero)

    case SumExp(l, r)     =>
      val (ls, lz, ln, lp, rs, rz, rn, rp) = lrInfo(l, r)

      if (lz)            rs
      else if (rz)       ls
      else if (ln && rn) Set(Sign.Negative)
      else if (lp && rp) Set(Sign.Positive)
      else               Set(Sign.Zero, Sign.Negative, Sign.Positive)

    case GtExp(l, r)      =>
      val (ls, lz, ln, lp, rs, rz, rn, rp) = lrInfo(l, r)

      if (lz && rz)              Set(Sign.Zero)
      else if (ln && (rz || rp)) Set(Sign.Zero)
      else if (lp && (rz || rp)) Set(Sign.Positive)
      else if (lz && rn)         Set(Sign.Positive)
      else                       Set(Sign.Positive, Sign.Zero)

    case IfExp(c, t, f)   =>
      val (cs, cz, cn, cp) = info(c)

      if (cz) apply(f)
      else if (!cs.contains(Sign.Zero)) apply(t)
      else apply(t) ++ apply(f)
  }

  private def lrInfo(l: Exp, r: Exp) = {
    val li = info(l)
    val ri = info(r)

    (li._1, li._2, li._3, li._4, ri._1, ri._2, ri._3, ri._4)
  }

  private def info(l: Exp) = {
    val ls = apply(l)

    val (lz, ln, lp) =
      ls.foldLeft((true, true, true)) { case (a, e) =>
        (a._1 && e == Sign.Zero, a._2 && e == Sign.Negative, a._3 && e == Sign.Positive)
      }

    (ls, lz, ln, lp)
  }
}
