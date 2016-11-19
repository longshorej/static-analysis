package info.longshore.staticanalysis

sealed trait Exp {
  def eval: Int
}

case class IntExp(value: Int) extends Exp {
  def eval = value
}

case class ProductExp(lhs: Exp, rhs: Exp) extends Exp {
  def eval = lhs.eval * rhs.eval
}

case class EqExp(lhs: Exp, rhs: Exp) extends Exp {
  def eval =
    if (lhs.eval == rhs.eval) 1
    else 0
}

case class SumExp(lhs: Exp, rhs: Exp) extends Exp {
  def eval = lhs.eval + rhs.eval
}

