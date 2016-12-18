package hephaestus
package unit
package ops

import shapeless._
import shapeless.ops.hlist._

import singleton.ops._
import singleton.ops.impl._

object dimensions {
  trait Eq[D1 <: Dimensions[_, _], D2 <: Dimensions[_, _]]

  object Eq {
    implicit def dimensionsEq[N1 <: HList, D1 <: HList, N2 <: HList, D2 <: HList](
      implicit an: Align[N1, N2], ad: Align[D1, D2])
        : Eq[Dimensions[N1, D1], Dimensions[N2, D2]] = 
      new Eq[Dimensions[N1, D1], Dimensions[N2, D2]] {}
  }

  trait Simplify[D <: Dimensions[_,_]] {
    type Out <: Dimensions[_,_]
  }

  object Simplify {

    type Aux[D <: Dimensions[_, _], Out0 <: Dimensions[_, _]] = Simplify[D] { type Out = Out0 }

    implicit def dimensionsSimplify[N <: HList, D <: HList, ND <: HList, DD <: HList](
      implicit ndiff: Diff.Aux[N, D, ND], ddiff: Diff.Aux[D, N, DD]
    ): Simplify.Aux[Dimensions[N, D], Dimensions[ND, DD]] = new Simplify[Dimensions[N, D]] {
      type Out = Dimensions[ND, DD]
    }
  }

  trait Multiply[D1 <: Dimensions[_, _], D2 <: Dimensions[_, _]] {
    type Out <: Dimensions[_, _]
  }

  object Multiply {
    type Aux[D1 <: Dimensions[_, _], D2 <: Dimensions[_, _], Out0 <: Dimensions[_, _]] = Multiply[D1, D2] { type Out = Out0 }

    implicit def dimensionsMultiply[N1 <: HList, D1 <: HList, N2 <: HList, D2 <: HList, NP <: HList, DP <: HList,
      DM <: Dimensions[_, _]](
      implicit np: Prepend.Aux[N1, N2, NP], dp: Prepend.Aux[D1, D2, DP], s: Simplify.Aux[Dimensions[NP, DP], DM]
    ) : Multiply.Aux[Dimensions[N1, D1], Dimensions[N2, D2], DM] =
      new Multiply[Dimensions[N1, D1], Dimensions[N2, D2]] {
        type Out = DM
      }
  }

  trait Divide[D1 <: Dimensions[_, _], D2 <: Dimensions[_, _]] {
    type Out <: Dimensions[_, _]
  }

  object Divide {
    type Aux[D1 <: Dimensions[_, _], D2 <: Dimensions[_, _], Out0 <: Dimensions[_, _]] = Divide[D1, D2] { type Out = Out0 }

    implicit def dimensionsDivide[N1 <: HList, D1 <: HList, N2 <: HList, D2 <: HList, NP <: HList, DP <: HList,
      DM <: Dimensions[_, _]](
      implicit np: Prepend.Aux[N1, D2, NP], dp: Prepend.Aux[D1, N2, DP], s: Simplify.Aux[Dimensions[NP, DP], DM]
    ) : Divide.Aux[Dimensions[N1, D1], Dimensions[N2, D2], DM] =
      new Divide[Dimensions[N1, D1], Dimensions[N2, D2]] {
        type Out = DM
      }
  }

  trait Invert[D <: Dimensions[_, _]] {
    type Out <: Dimensions[_, _]
  }

  object Invert {
    type Aux[D <: Dimensions[_, _], Out0 <: Dimensions[_, _]] = Invert[D] { type Out = Out0 }
  }
}

object units {
  trait Add[A, U1 <: Units[_, _], U2 <: Units[_, _]] {
    type Out <: Units[_, _]

    def apply(x: Quantity[A, U1], y: Quantity[A, U2]): Quantity[A, Out]
  }

  object Add {
    type Aux[A, U1 <: Units[_, _], U2 <: Units[_, _], Out0 <: Units[_, _]] = Add[A, U1, U2] { type Out = Out0 }

    implicit def unitsAddDouble[I1 <: Singleton with Int, I2 <: Singleton with Int, IMax <: Singleton with Int,
      D1 <: Dimensions[_, _], D2 <: Dimensions[_, _]](
      implicit ev0: dimensions.Eq[D1, D2],
      max: Max[I1, I2] { type Out = IMax },
      i1: ValueOf[I1],
      i2: ValueOf[I2]
    ): Aux[Double, Units[I1, D1], Units[I2, D2], Units[IMax, D1]] = 
      new Add[Double, Units[I1, D1], Units[I2, D2]] {
      type Out = Units[IMax, D1]

      def apply(x: Quantity[Double, Units[I1, D1]], y: Quantity[Double, Units[I2, D2]]): Quantity[Double, Out] = {
        val v1 = i1.value
        val v2 = i2.value
        val (xv, yv) = if(v1 > v2) (x.value, y.value * math.pow(10.0, (v2 - v1).toDouble))
        else if(v1 < v2) (x.value * math.pow(10.0, (v1 - v2).toDouble), y.value)
        else (x.value, y.value)
        Quantity[Double, Out](xv + yv)
      }
    }
  }

  trait Negate[A] {
    def apply[U <: Units[_, _]](q: Quantity[A, U]): Quantity[A, U]
  }

  object Negate {
    implicit def unitsNegateDouble: Negate[Double] = new Negate[Double] {
      def apply[U <: Units[_, _]](q: Quantity[Double, U]): Quantity[Double, U] = Quantity[Double, U](-q.value)
    }
  }

  trait Multiply[A, U1 <: Units[_, _], U2 <: Units[_, _]] {
    type Out <: Units[_, _]
    def apply(q1: Quantity[A, U1], q2: Quantity[A, U2]): Quantity[A, Out]
  }

  object Multiply {
    type Aux[A, U1 <: Units[_, _], U2 <: Units[_, _], Out0 <: Units[_, _]] = Multiply[A, U1, U2] { type Out = Out0 }

    implicit def unitsMultiplyDouble[I1 <: Singleton with Int, I2 <: Singleton with Int, D1 <: Dimensions[_, _], D2 <: Dimensions[_, _],
      DM <: Dimensions[_, _],
      IM <: Singleton with Int](
      implicit dm: dimensions.Multiply.Aux[D1, D2, DM],
      im: OpInt.Aux[I1 + I2, IM]
    )
: Multiply.Aux[Double, Units[I1, D1], Units[I2, D2], Units[IM, DM]] = new Multiply[Double, Units[I1, D1], Units[I2, D2]] {
      type Out = Units[IM, DM]

      def apply(q1: Quantity[Double, Units[I1, D1]], q2: Quantity[Double, Units[I2, D2]]): Quantity[Double, Out] = 
        Quantity[Double, Out](q1.value * q2.value)
    }
  }

  trait Divide[A, U1 <: Units[_, _], U2 <: Units[_, _]] {
    type Out <: Units[_, _]
    def apply(q1: Quantity[A, U1], q2: Quantity[A, U2]): Quantity[A, Out]
  }

  object Divide {
    type Aux[A, U1 <: Units[_, _], U2 <: Units[_, _], Out0 <: Units[_, _]] = Divide[A, U1, U2] { type Out = Out0 }

    implicit def unitsDivideDouble[I1 <: Singleton with Int, I2 <: Singleton with Int, D1 <: Dimensions[_, _], D2 <: Dimensions[_, _],
      DM <: Dimensions[_, _],
      IM <: Singleton with Int](
      implicit dm: dimensions.Divide.Aux[D1, D2, DM],
      im: OpInt.Aux[I1 - I2, IM]
    ) : Divide.Aux[Double, Units[I1, D1], Units[I2, D2], Units[IM, DM]] = new Divide[Double, Units[I1, D1], Units[I2, D2]] {
      type Out = Units[IM, DM]

      def apply(q1: Quantity[Double, Units[I1, D1]], q2: Quantity[Double, Units[I2, D2]]): Quantity[Double, Out] = 
        Quantity[Double, Out](q1.value / q2.value)
    }
  }

  trait Power[A, U <: Units[_, _], I <: Singleton with Int] {
    type Out <: Units[_, _]
    def apply(q: Quantity[A, U]): Quantity[A, Out]
  }

  //assuming the value can be taken to the power
  //we have to multiply the prefix by the power
  //we have to multiply the dimension by the power until the power is 0
  //if the power is negative, then we need to invert the dimensions
  //and then we need to multipy the dimensions by themselves until satisfied
}
