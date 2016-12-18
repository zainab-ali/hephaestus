package hephaestus
package unit

import singleton.ops._
import singleton.ops.impl._
import shapeless._
import shapeless.ops.hlist._

/* tag to represent dimension */
trait Dimension

object Dimension {
  object Mass extends Dimension
  object Length extends Dimension 
}

/* tag to represent a unit without prefix */
// trait PrimaryUnit[D]

// object PrimaryUnit {
//   type g = PrimaryUnit[Dimension.Mass.type]
// }

trait MetricPrefix[I]

object MetricPrefix {
  type kilo = MetricPrefix[3]
  type std = MetricPrefix[0]
  type milli = MetricPrefix[-3]
}

/* tag to represent a prefix and a set of positive dimensions and negative dimensions*/
trait Dimensions[N <: HList, D <: HList]

trait Units[P, D]
//have evidence that these are equal

// trait Convert[A, I1, I2] {
//   def apply[D](x: Quantity[A, Dimensionality[I1, D]]): Quantity[A, Dimensionality[I2, D]]
// }

/* op to add two quantities of the same dimensions */
object Add {
  //we can only add D1 and D2 if they have equivalent HLists
  //if we add quantities with prefixes 1 and 2, we must pick the max prefix
  //to add the quantities, we need to convert the min prefix quantity to the max quantity and 
  //I3 is the max of I1 and I2
  // implicit def add[A, I1 <: Singleton with Int, I2 <: Singleton with Int, D1 <: HList, D2 <: HList, I3 <: Singleton with Int](
  //   implicit c1: Convert[A, I1, I3], c2: Convert[A, I2, I3], m: OpInt.Aux[Max[I1, I2], I3]
  // ): Add[A, Dimensionality[I1, D1], Dimensionality[I2, D2]] =
  //   new Add[A, Dimensionality[I1, D1], Dimensionality[I2, D2]] {
  //     type Out = Dimensionality[I3, D1]
  //   def apply(x: Quantity[A, Dimensionality[I1, D1]], y: Quantity[A, Dimensionality[I2, D2]]): Quantity[A, Out] = ???
  // }
}


case class Quantity[A, U <: Units[_, _]](value: A) extends AnyVal

// /* tag to represent a metric unit with prefix */
// trait MetricUnit[P, U]

// object MetricUnit {
//   /* kg is a base unit */
//   type kg = MetricUnit[MetricPrefix.kilo, PrimaryUnit.g]
//   type g = MetricUnit[MetricPrefix.std, PrimaryUnit.g]

// }

// trait UnitConversion[A, M1, M2] {
//   def apply(x: Quantity[A, M1]): Quantity[A, M2]
// }

// object UnitConversion {
//   implicit def metricConversion[I1 <: Singleton with Int, I2 <: Singleton with Int, U](implicit v1: ValueOf[I1], v2: ValueOf[I2]): UnitConversion[Int, MetricUnit[MetricPrefix[I1], U], MetricUnit[MetricPrefix[I2], U]] = new UnitConversion[Int, MetricUnit[MetricPrefix[I1], U], MetricUnit[MetricPrefix[I2], U]] {
//     def apply(x: Quantity[Int, MetricUnit[MetricPrefix[I1], U]]): Quantity[Int, MetricUnit[MetricPrefix[I2], U]] = {
//       val f = math.pow(10.0, v1.value - v2.value)
//       Quantity((x.value.toDouble * f).toInt)
//     }
//   }
// }

// case class Quantity[A, M](value: A) extends AnyVal {
//   def to[M2](implicit c: UnitConversion[A, M, M2]): Quantity[A, M2] = c(this)
// }

// object Quantity {
//   val tg = Quantity[Int, MetricUnit.g](1000)
// }

// object Foo {
//   def main(args: Array[String]): Unit = {
//     val tg = Quantity[Int, MetricUnit.g](1000)
//     val result = tg.to[MetricUnit.kg]
//   }
// }

/**
next: multiplication / division
Quantity[A, M1] * Quantity[A, M2] = Quantity[A, M1 :: M2]

Laws:

kg m === km g

kg cm == g m

Thus, model as a MetricPrefix + a HList of dimensions


  * */

object Foo extends App {
  import ops.dimensions._
  type A = Dimensions[Dimension.Mass.type :: Dimension.Length.type :: HNil, HNil]
  type B = Dimensions[Dimension.Length.type :: Dimension.Mass.type :: HNil, HNil]
  implicitly[Eq[A, B]]

  import ops.units._
  val q1 = Quantity[Double, Units[3, A]](5.0) //5 kg m
  val q2 = Quantity[Double, Units[0, A]](4.0) //4 g m
  val a = the[Add[Double, Units[3, A], Units[0, A]]]
  val b = the[Add[Double, Units[3, A], Units[3, A]]]
  val r = a(q1, q2)
  val r2 = b(r, q1)

  type C = Dimensions[Dimension.Mass.type :: HNil, HNil]
  type D = Dimensions[Dimension.Length.type :: HNil, Dimension.Mass.type :: HNil]

  type E = Dimension.Mass.type :: HNil
  type F = Dimension.Length.type :: HNil

  val s: String = the[ops.units.Multiply[Double, Units[3, C], Units[0, D]]]
}
