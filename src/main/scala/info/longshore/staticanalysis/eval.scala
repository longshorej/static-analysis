package info.longshore.staticanalysis

object eval {
  def apply(exp: Exp): Int = exp match {
    case IntExp(v)        => v
    case ProductExp(l, r) => eval(l) * eval(r)
    case EqExp(l, r)      => encode(eval(l) == eval(r))
    case SumExp(l, r)     => eval(l) + eval(r)
    case GtExp(l, r)      => encode(eval(l) > eval(r))
    case IfExp(c, t, f)   => if (eval(c) == 0) eval(f) else eval(t)
  }

  def encode(b: Boolean): Int = if (b) 1 else 0
}
