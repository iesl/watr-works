package edu.umass.cs.iesl.watr
package utils


object ExactFloats {
  import scalaz.Tag
  import TypeTags._

  sealed trait FloatRep

  val FloatRep = Tag.of[FloatRep]

  type FloatExact = Int@@FloatRep

  object FloatExact {
    val zero = FloatRep(0)
    val epsilon = FloatRep(1)
  }

  def max(d1:Int@@FloatRep, d2: Int@@FloatRep): Int@@FloatRep = FloatRep(math.max(d1.unwrap, d2.unwrap))
  def min(d1:Int@@FloatRep, d2: Int@@FloatRep): Int@@FloatRep = FloatRep(math.min(d1.unwrap, d2.unwrap))
  def abs(d1:Int@@FloatRep): Int@@FloatRep = FloatRep(math.abs(d1.unwrap))

  implicit class RicherFloatExact(val self: Int@@FloatRep) extends AnyVal {
    def +(r: Int@@FloatRep): Int@@FloatRep = FloatRep(self.unwrap+r.unwrap)
    def -(r: Int@@FloatRep): Int@@FloatRep = FloatRep(self.unwrap-r.unwrap)
    def *(r: Int@@FloatRep): Int@@FloatRep = (self.asDouble*r.asDouble).toFloatExact
    def /(r: Int@@FloatRep): Int@@FloatRep = (self.asDouble/r.asDouble).toFloatExact
    def unary_-(): Int@@FloatRep = FloatRep(-self.unwrap)

    def <(r: Int@@FloatRep): Boolean = self.unwrap<r.unwrap
    def <=(r: Int@@FloatRep): Boolean = self.unwrap<=r.unwrap
    def >=(r: Int@@FloatRep): Boolean = self.unwrap>=r.unwrap
    def >(r: Int@@FloatRep): Boolean = self.unwrap>r.unwrap


    def +(r: Double): Int@@FloatRep = self + r.toFloatExact()
    def -(r: Double): Int@@FloatRep = self - r.toFloatExact()
    def *(r: Double): Int@@FloatRep = (self.asDouble * r).toFloatExact
    def /(r: Double): Int@@FloatRep = (self.asDouble / r).toFloatExact

    def <(r:  Double): Boolean = self.asFloat<r
    def <=(r: Double): Boolean = self.asFloat<=r
    def >=(r: Double): Boolean = self.asFloat>=r
    def >(r: Double): Boolean  = self.asFloat>r

    def +(r: Int): Int@@FloatRep = self + r.toFloatExact()
    def -(r: Int): Int@@FloatRep = self - r.toFloatExact()
    def *(r: Int): Int@@FloatRep = (self.asDouble * r).toFloatExact
    def /(r: Int): Int@@FloatRep = (self.asDouble / r).toFloatExact

    def <(r:  Int): Boolean = self.asFloat<r
    def <=(r: Int): Boolean = self.asFloat<=r
    def >=(r: Int): Boolean = self.asFloat>=r
    def >(r: Int): Boolean  = self.asFloat>r

    def +(r: Float): Int@@FloatRep = self + r.toFloatExact
    def -(r: Float): Int@@FloatRep = self - r.toFloatExact
    def *(r: Float): Int@@FloatRep = (self.asFloat * r).toFloatExact
    def /(r: Float): Int@@FloatRep = (self.asFloat / r).toFloatExact


    def asFloat(): Float = { self.unwrap/100.0f }
    def asDouble(): Double = { self.unwrap/100.0d }
    def asInt(): Int = { self.unwrap/100 }

    def dblFormat(): String = {
      val intRep = self.unwrap
      val negSign = if(intRep < 0) "-" else ""
      val digits = math.abs(intRep).toString.toList
      val(decR, wholeR) = digits.reverse.splitAt(2)
      val decPad = decR ++ List.fill(2-decR.length)('0')
      val dec = decPad.reverse.mkString
      val whole = if (wholeR.isEmpty) "0" else wholeR.reverse.mkString

      s"${negSign}${whole}.${dec}"
    }

    def pp(): String = dblFormat()

    def eqFuzzy(tolerance: Double)(d2: Int@@FloatRep): Boolean =
      compareFuzzy(tolerance)(d2) == 0


    def compareFuzzy(tolerance: Double)(d20: Int@@FloatRep): Int = {
      val d2 = d20.asDouble
      val d1 = self.asDouble
      if (math.abs(d1 - d2) < tolerance) 0
      else if (d1 < d2) -1
      else 1
    }

    import Interval._

    def plusOrMinus(i: Double@@Percent): Interval.FloatExacts ={
      val half = self * i.unwrap
      Interval.FloatExacts(self-half, self+half)
    }

    def withinRange(r: Interval.FloatExacts): Boolean = {
      r.min <= self && self <= r.max
    }
  }

  implicit class RicherInt_2(val d: Int) extends AnyVal {
    def toFloatExact() = {
      FloatRep((d*100).toInt)
    }
  }

  implicit class RicherFloat_2(val d: Float) extends AnyVal {
    def float2fp2(d: Float): Int@@FloatRep = {
      FloatRep(
        (d*100.0d).toInt
      )
    }
    def toFloatExact() = float2fp2(d)
  }

  implicit class RicherDouble_2(val d: Double) extends AnyVal {
    def dbl2fp2(d: Double): Int@@FloatRep = {
      FloatRep(
        (d*100.0d).toInt
      )
    }
    def toFloatExact() = dbl2fp2(d)

  }

}
