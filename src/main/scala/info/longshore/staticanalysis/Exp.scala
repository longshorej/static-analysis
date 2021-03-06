package info.longshore.staticanalysis

sealed trait Exp

case class IntExp(value: Int) extends Exp

case class ProductExp(lhs: Exp, rhs: Exp) extends Exp

case class EqExp(lhs: Exp, rhs: Exp) extends Exp

case class SumExp(lhs: Exp, rhs: Exp) extends Exp

case class GtExp(lhs: Exp, rhs: Exp) extends Exp

case class IfExp(cond: Exp, ifTrue: Exp, ifFalse: Exp) extends Exp
