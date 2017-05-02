package com.altf7.rate

/*
  TimeUnit is time unit on which rates are expressed (1 per (Day), 3 per (Day), 1 per (Hour))
  rate is 10 per second ---> Second(10)
  10 in minute ---> PerMinute(10)).toPerSecond  ----> PerSecond(10)
  10 per second --->PerSecond(10)
 */

trait Defaults{
  val DaysInMonth=30.44
}

sealed trait RateTimeUnit extends Defaults{
  val baseUnits:Double
  val prev:RateTimeUnit
  val next:RateTimeUnit
  override def toString:String=this.getClass.getSimpleName.dropRight(1).toLowerCase
}


object Millisecond extends RateTimeUnit {
  val baseUnits=1.0
  val prev=Millisecond
  val next=Second
}
object Second extends RateTimeUnit {
  val baseUnits=Millisecond.baseUnits*1000
  val prev=Millisecond
  val next=Minute
}
object Minute extends RateTimeUnit {
  val baseUnits=Second.baseUnits*60
  val prev=Second
  val next=Hour
}
object Hour extends RateTimeUnit{
  val baseUnits=Minute.baseUnits*60
  val prev=Minute
  val next=Day
}
object Day extends RateTimeUnit{
  val baseUnits=Hour.baseUnits*24
  val prev=Hour
  val next=Week
}
object Week extends RateTimeUnit{
  val baseUnits=Day.baseUnits*(DaysInMonth/4)
  val prev=Day
  val next=Month
}
object Month extends RateTimeUnit{
  val baseUnits=Day.baseUnits*DaysInMonth
  val prev=Week
  val next=Year
}
object Year extends RateTimeUnit{
  val baseUnits=Month.baseUnits*DaysInMonth*12
  val prev=Month
  val next=Year
}


case class Rate(unit:Double,rateTimeUnit:RateTimeUnit) extends Ordered[Rate] {
  //def per(rateTimeUnit)

  /*
      gives unit based on rateTimeUnit from implicit val
   */
  def rate(implicit timeUnit:RateTimeUnit):Double=this to timeUnit unit

  /*
      Converts Rate into duration

      If duration was eg 20minutes then rate would be Rate of 1/20 per Minute
      1/20 per Minute is converted into 1/1/20 minutes
   */
  def toDuration:Double=1.0/unit

  override def toString():String={
    if (unit % 1 == 0) s"${unit.toInt} per ${rateTimeUnit.toString}" else
      s"${unit} per ${rateTimeUnit.toString}"
  }

  def toBaseUnits=unit/rateTimeUnit.baseUnits

  def to(thatRateTimeUnit: RateTimeUnit): Rate = {
    if (rateTimeUnit==thatRateTimeUnit) this else {
      if (rateTimeUnit.baseUnits < thatRateTimeUnit.baseUnits) {
        val f=rateTimeUnit.baseUnits/thatRateTimeUnit.baseUnits
        Rate(unit/f, thatRateTimeUnit)
      } else {
        val f=thatRateTimeUnit.baseUnits/rateTimeUnit.baseUnits
        Rate(f*unit, thatRateTimeUnit)
      }
    }

  }

  /**
    * Adds/Substracts/divide/multiply this rate to `that`. The result is of this Rate's rateTimeUnit.
    */
  def +(that: Rate): Rate = Rate(unit.+(that.to(rateTimeUnit).unit),rateTimeUnit)

  def *(that: Rate): Rate = Rate(unit.*(that.to(rateTimeUnit).unit),rateTimeUnit)
  /*
      negative values are not possible and 0 is returned
   */
  def -(that: Rate): Rate = {
    val newunit={
      val nunit=unit.-(that.to(rateTimeUnit).unit)
      if (nunit<0.0) 0 else nunit
    }
    Rate(newunit,rateTimeUnit)
  }

  def /(that: Rate): Rate = {
    val newunit = {
      val nunit = unit./(that.to(rateTimeUnit).unit)
      if (nunit < 0.0) 0 else nunit
    }
    Rate(newunit, rateTimeUnit)
  }


  /**
    * Increment/Substracts/divide/multiply unit value of this rate
    */

  def +(that: Double): Rate = this + Rate(that, this.rateTimeUnit)
  def -(that: Double): Rate = this - Rate(that, this.rateTimeUnit)
  def *(that: Double): Rate = this * Rate(that, this.rateTimeUnit)
  def /(that: Double): Rate = this / Rate(that, this.rateTimeUnit)


  def ===(that: Rate): Boolean = this.compare(that) == 0
  def !==(that: Rate): Boolean = this.compare(that) != 0

  //override def toString: String = toFormattedString()
  override def compare(that: Rate): Int = {
    val thatRate = that.to(rateTimeUnit)
    this.unit compare thatRate.unit
  }
}


object Implicits {
  implicit def BigDecimalToRate(unit: BigDecimal) = new {
    def per(rateTimeUnit: RateTimeUnit): Rate = Rate(unit.toDouble, rateTimeUnit)
    def toRate(rateTimeUnit:RateTimeUnit):Rate=Rate(1.0/unit.toDouble,rateTimeUnit)
  }

  implicit class DoubleToRate(val unit: Double) extends AnyVal {
    def per(rateTimeUnit: RateTimeUnit): Rate = Rate(unit, rateTimeUnit)
    def toRate(rateTimeUnit:RateTimeUnit):Rate=Rate(1.0/unit,rateTimeUnit)
  }

  implicit class IntToRate(val unit: Int) extends AnyVal {
    def per(rateTimeUnit: RateTimeUnit): Rate = Rate(unit.toDouble, rateTimeUnit)
    def toRate(rateTimeUnit:RateTimeUnit):Rate=Rate(1.0/unit.toDouble,rateTimeUnit)
  }
  implicit class FloatToRate(val unit: Float) extends AnyVal {
    def per(rateTimeUnit: RateTimeUnit): Rate = Rate(unit.toDouble, rateTimeUnit)
    def toRate(rateTimeUnit:RateTimeUnit):Rate=Rate(1.0/unit.toDouble,rateTimeUnit)
  }

}
