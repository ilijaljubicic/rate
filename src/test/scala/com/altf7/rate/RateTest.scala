package com.altf7.rate

import org.scalatest._
import org.scalatest.matchers._

class RateTest extends FlatSpec with Matchers {

  "Rate" should "convert X per Month to all other rates directly" in {
    val u=Month.baseUnits //per month when 1 per millisecond
    val ar=Rate(u,Month)
    (ar.to(Week).unit) shouldBe Week.baseUnits
    (ar.to(Day).unit) shouldBe Day.baseUnits
    (ar.to(Hour).unit) shouldBe Hour.baseUnits
    (ar.to(Minute).unit) shouldBe Minute.baseUnits
    assert(ar.to(Second).unit - Second.baseUnits < 0.0000001)
    (ar.to(Millisecond).unit) shouldBe 1
  }
  it should "convert X  per Millisecond to all other rates directly" in {
    val u=Month.baseUnits //per month when 1 per millisecond
    val ar=Rate(1,Millisecond)

    (ar.to(Second).unit) shouldBe Second.baseUnits
    (ar.to(Minute).unit) shouldBe Minute.baseUnits
    assert((ar.to(Hour).unit) - Hour.baseUnits <0.000001)
    (ar.to(Day).unit) shouldBe Day.baseUnits
    (ar.to(Week).unit) shouldBe Week.baseUnits
    (ar.to(Month).unit) shouldBe Month.baseUnits

  }
  it should "convert X per Month to Y per Millisecond chaining Rate.to's." in {
    val u=Month.baseUnits  //per month when 1 per millisecond
    val m=Rate(u,Month)
    m.to(Day).to(Hour).to(Minute).to(Millisecond).unit shouldBe 1.0
  }

  it should "convert From 1 per Millisecond to X per Month chaining Rate.to's." in {
    val u=Month.baseUnits //1 per month in  milliseconds
    val ml=Rate(1,Millisecond)
    (ml.to(Second).to(Minute).to(Hour).to(Day).to(Month)).unit shouldBe u

    val wk=ml.to(Second).to(Minute).to(Hour).to(Day).to(Week)
    wk.to(Month).unit equals 1.0
  }
  it should "convert week to month and month to week" in {
    Rate(1.0,Week).to(Month).unit shouldBe 4.0
    Rate(4.0,Month).to(Week).unit shouldBe 1.0
  }

  it should "sum/substract/divide/multiply same Rate TimeUnits correctly" in {
    (Rate(1.0,Week) + Rate(1.0,Week)).unit shouldBe 2.0
    (Rate(4.0,Week) / Rate(2.0,Week)).unit shouldBe 2.0
    (Rate(2.0,Week) * Rate(2.0,Week)).unit shouldBe 4.0
    (Rate(2.0,Week) - Rate(1.0,Week)).unit shouldBe 1.0
  }
  it should "sum/substract/divide/multiply different Rate TimeUnits correctly (eg. 1 per Day + 4 per Month )" in {
    val u=Month.baseUnits
    (Rate(1.0,Week) + Rate(4.0,Month)).unit shouldBe 2.0
    (Rate(1.0,Millisecond) + Rate(Month.baseUnits,Month)).unit shouldBe 2.0

    (Rate(2.0,Week) * Rate(8.0,Month)).unit shouldBe 4.0
    (Rate(2.0,Week) / Rate(8.0,Month)).unit shouldBe 1.0

    (Rate(2.0,Week) - Rate(8.0,Month)).unit shouldBe 0.0

    (Rate(8.0,Month) - Rate(1.0,Week)) shouldBe Rate(4.0,Month)
  }

  it should "sum/substract/divide/multiply Rate unit with a Double and output new Rate" in {
    (Rate(1.0,Week) + 2) shouldBe Rate(3.0,Week)
    (Rate(4.0,Week) - 1) shouldBe Rate(3.0,Week)
    (Rate(4.0,Week) / 2) shouldBe Rate(2.0,Week)
    (Rate(4.0,Week) * 2) shouldBe Rate(8.0,Week)
  }
}
