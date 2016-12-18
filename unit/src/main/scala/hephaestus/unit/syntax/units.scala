package hephaestus
package unit
package syntax

import ops.units._

final class QuantityOps[A, U <: Units[_, _]](val q: Quantity[A, U]) {
  def add[U2 <: Units[_, _]](q2: Quantity[A, U2])(implicit a: Add[A, U, U2]): Quantity[A, a.Out] = a(q, q2) 
  def subtract[U2 <: Units[_, _]](q2: Quantity[A, U2])(implicit a: Add[A, U, U2], n: Negate[A]): Quantity[A, a.Out] = a(q, n(q2)) 
  def multiply[U2 <: Units[_, _]](q2: Quantity[A, U2])(implicit m: Multiply[A, U, U2]): Quantity[A, m.Out] = m(q, q2) 
}
