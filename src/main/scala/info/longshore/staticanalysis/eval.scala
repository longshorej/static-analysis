package info.longshore.staticanalysis

object eval {
  def apply(exp: Exp): Int = exp match {
    case IntExp(v)        => v
    case ProductExp(l, r) => eval(l) * eval(r)
    case EqExp(l, r)      => if (eval(l) == eval(r)) 1 else 0
    case SumExp(l, r)     => eval(l) + eval(r)
  }
}
