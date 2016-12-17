package hephaestus
package unit

/* tag to represent dimension */
trait Dimension

object Dimension {
  object Mass extends Dimension
}

/* tag to represent a unit without prefix */
trait PrimaryUnit[D]

object PrimaryUnit {
  type g = PrimaryUnit[Dimension.Mass.type]
}

trait MetricPrefix[I]

object MetricPrefix {
  type kilo = MetricPrefix[3]
  type std = MetricPrefix[0]
  type milli = MetricPrefix[-3]
}

/* tag to represent a metric unit with prefix */
trait MetricUnit[P, U]

object MetricUnit {
  /* kg is a base unit */
  type kg = MetricUnit[MetricPrefix.kilo, PrimaryUnit.g]
  type g = MetricUnit[MetricPrefix.std, PrimaryUnit.g]

}

trait UnitConversion[A, M1, M2] {
  def apply(x: Quantity[A, M1]): Quantity[A, M2]
}

object UnitConversion {
  implicit def metricConversion[I1 <: Singleton with Int, I2 <: Singleton with Int, U](implicit v1: ValueOf[I1], v2: ValueOf[I2]): UnitConversion[Int, MetricUnit[MetricPrefix[I1], U], MetricUnit[MetricPrefix[I2], U]] = new UnitConversion[Int, MetricUnit[MetricPrefix[I1], U], MetricUnit[MetricPrefix[I2], U]] {
    def apply(x: Quantity[Int, MetricUnit[MetricPrefix[I1], U]]): Quantity[Int, MetricUnit[MetricPrefix[I2], U]] = {
      val f = math.pow(10.0, v1.value - v2.value)
      Quantity((x.value.toDouble * f).toInt)
    }
  }
}

case class Quantity[A, M](value: A) extends AnyVal {
  def to[M2](implicit c: UnitConversion[A, M, M2]): Quantity[A, M2] = c(this)
}

object Quantity {
  val tg = Quantity[Int, MetricUnit.g](1000)
}

object Foo {
  def main(args: Array[String]): Unit = {
    val tg = Quantity[Int, MetricUnit.g](1000)
    val result = tg.to[MetricUnit.kg]
  }
}

/**
next: multiplication / division
Quantity[A, M1] * Quantity[A, M2] = Quantity[A, M1 :: M2]

Laws:

kg m === km g

kg cm == g m

Thus, model as a MetricPrefix + a HList of dimensions


  * */
