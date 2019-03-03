
package org.sigfigs

import java.lang.Math
import java.text.DecimalFormat

object SDRoundingMode extends Enumeration {
  type Type = Value
  val Always, Never = Value
}

import SDRoundingMode._

/**
  * configuration context to set the type of printing, and if to round in 
  * between each operation or not
  */
trait SDContext extends util.PrettyPrinter {

  /** uses unicode to highlight digits that are significant */
  def highlight(s: String): String = toBold(s)

  /** 
    * if to round the results of each operation or use full accuracy 
    * in compuation 
    */
  def roundingMode(): SDRoundingMode.Value = Never
}

object SignificantDigits {

  object DefaultSDContext extends SDContext

  // This is actually what we want.  We want a global config that can be changed
  // on the fly to control how the SD objects created are printed and how they
  // compute new values.  In other words, a singleton.
  // After SignificantDigits are created, their behavior is fixed based upon
  // this variable at the time they are created.
  private[this] var context: SDContext = DefaultSDContext
  def setDefaultSDContext(): Unit = this.synchronized {
    context = DefaultSDContext
  }
  def setSDContext(c: SDContext): Unit = this.synchronized {
    context = c
  }

  /** 
    * constructs a new SignificantDigits wrapper for a numeric of type T,
    * computes the significant digits by trimming both leading and trailing
    * zeros
    */
  def apply[T](v: T)(
    implicit n: SignificantOps[T], nu: Numeric[T]): SignificantDigits[T] =
    apply[T](v, computeDigits[T](v))

  /**
    * constructs a new SignificantDigits wrapper in the requested numeric type T
    * and assumes all given (trailing) digits are significant, only leading 
    * zeros are discounted
    */
  def apply[T](str: String)(
    implicit n: SignificantOps[T], nu: Numeric[T]): SignificantDigits[T] =
    apply[T](n.to[T](str), computeDigits[T](str, false))

  /**
    * constructs a new SignificantDigits wrapper with the specified number of
    * significant digits
    */
  def apply[T](v: T, digits: Int)(
    implicit n: SignificantOps[T], nu: Numeric[T]): SignificantDigits[T] = {
    implicit val c: SDContext = context
    new SignificantDigits(v, digits)
  }

  private[this] val df: ThreadLocal[DecimalFormat] =
    ThreadLocal.withInitial[DecimalFormat](() => {
      val d = new DecimalFormat("#")
      d.setMaximumFractionDigits(340)
      d
    })

  /** compute given significant digits, assumes that we will be removing 
    * trailing zeros */
  def computeDigits[T](t: T)(implicit n: Numeric[T]): Int =
    computeDigits[T](df.get.format(t), true)

  /**
    * computes significant digits, possibly removing trailing zeros unless 
    * requested
    */
  def computeDigits[T](str: String, removeTrailingZeros: Boolean = true)(
    implicit n: Numeric[T]): Int = {
    val trailingZeros =
      str.reverseIterator.takeWhile(c => c == '0' || c == '.').size
    val negNum = (str(0) == '-')
    if (!str.contains('.')) { // isWhole
      (if (removeTrailingZeros) str.length - trailingZeros else str.length) -
      (if (negNum) 1 else 0)
    } else {
      val leadingZerosStr = str.takeWhile(c => c == '0' || c == '.' || c == '-')
      if (leadingZerosStr.contains('.')) {
        if (negNum) str.length - leadingZerosStr.size - 1
        else str.length - leadingZerosStr.size
      } else {
        if (negNum) str.length - leadingZerosStr.size - 2
        else str.length - leadingZerosStr.size - 1
      }
    }
  }

  /**
    * The Numeric implicit for all SignificantDigits instances
    * allows basic operators such as +,-,*,/ to work and lets scala treat
    * SignificantDigits instances just like regular numbers with extra
    * capabilities
    */
  trait SignificantOps[T] { self: Numeric[SignificantDigits[T]] =>

    type SDType = SignificantDigits[T]
    protected val num: Numeric[T]

    /** lifts a normal integer (sometimes an integer literal from the 
      * Scala compiler) into a SignificantDigits[T] */
    def fromInt(x: Int): SDType = {
      implicit val nu: Numeric[T] = num
      implicit val n: SignificantOps[T] = this
      SignificantDigits[T](num.fromInt(x), computeDigits[Int](x))
    }

    /** generic converter from a String to the underlying datatype */
    def to[T](str: String)(implicit n: Numeric[T]): T
    /** generic converter from a Double to the underlying datatype */
    def to[T](d: Double)(implicit n: Numeric[T]): T

    /** delegates to the plus method in SignificantDigits */
    def plus(x: SDType, y: SDType): SDType = x.plus(y)
    /** delegates to the minus method in SignificantDigits */
    def minus(x: SDType, y: SDType): SDType = x.minus(y)
    /** delegates to the negate method in SignificantDigits */
    def negate(x: SDType): SDType = x.negate()
    /** delegates to the times method in SignificantDigits */
    def times(x: SDType, y: SDType): SDType = x.times(y)

    /** delegates to the toDouble method in SignificantDigits */
    def toDouble(x: SDType): Double = x.toDouble
    /** delegates to the toFloat method in SignificantDigits */
    def toFloat(x: SDType): Float = x.toFloat
    /** delegates to the toInt method in SignificantDigits */
    def toInt(x: SDType): Int = x.toInt
    /** delegates to the toLong method in SignificantDigits */
    def toLong(x: SDType): Long = x.toLong

    /** delegates to the div method in SignificantDigits */
    def div(x: SDType, y: SDType): SDType = x.div(y)
    /** delegates to the quot method in SignificantDigits */
    def quot(x: SDType, y: SDType): SDType = x.quot(y)
    /** delegates to the rem method in SignificantDigits */
    def rem(x: SDType, y: SDType): SDType = x.rem(y)

    /** standard min using underlying data and its associated Numeric to
      * do the calculations */
    def sdmin[U <: SDType](x: U, y: U): U =
      if (num.gt(num.minus(x.v, y.v), num.zero)) y else x
    /** standard max using underlying data and its associated Numeric to
      * do the calculations */
    def sdmax[U <: SDType](x: U, y: U): U =
      if (num.gt(num.minus(x.v, y.v), num.zero)) x else y

    /** rounds one SignificantDigits type to another SignificantDigits type,
      * only supports Double -> Long and Float -> Int */
    // implementations are in the type specific subclasses in package.scala
    def round[T2](x: SDType)(implicit o: SignificantOps[T2]):
        SignificantDigits[T2] = 
      throw new Exception(s"No round method for $x")

    /** removes negations if present, does work here using the underlying value,
      * and keeps the same number of significant digits no matter what */
    def sdabs(x: SignificantDigits[T]): SignificantDigits[T] =
      makeSigDigits(num.abs(x.v), x.digits)

    // added in scala 2.13
    def parseString(str: String): Option[SDType] = num.parseString(str) match {
      case Some(a) => {
        implicit val ops = this
        implicit val nu = num
        Some(SignificantDigits[T](a))
      }
      case None => None
    }

    /** does a type dependant signum (ie T -> T) instead of a T -> Int typed
      * signum, uses the underlying value T and sets the number of significant
      * digits to 1 */
    def nativeSignum(x: SDType): SDType = {
      implicit val nu: Numeric[T] = num
      makeSigDigits(to[T](num.signum(x.v).toDouble), 1)
    }

    /** type independant signum T -> Int.  uses the Numeric[T] implementation
      * of signum and returns an Int which has no concept of significant digits
      */
    def sdsignum(x: SDType): Int = num.signum(x.v)

    /** only 1 significant digit returned by this method, only supported with
      * Doubles and Floats, probably shouldn't even exist */
    // implementations are in the type specific subclasses in package.scala
    def ulp(x: SDType): SDType = throw new Exception(s"No round method for $x")

    // for addition/subtraction:
    // the last significant decimal place (hundreds, tens, ones, tenths, and so
    // forth) in the calculated result should be the same as the leftmost or
    // largest decimal place of the last significant figure out of all the
    // measured quantities in the terms of the sum.
    private[sigfigs] def calculateAddSubDigits(x: SDType, y: SDType, res: T)(
      implicit numb: Numeric[T]): Int = {

      assert(numb != null)
      val resTopDigit = 
        Math.floor(Math.log10(Math.abs(numb.toDouble(res)))).toInt

      if (x.digits < Integer.MAX_VALUE && y.digits < Integer.MAX_VALUE) {
        val mostLeastSigDigit = Math.max(
          Math.floor(Math.log10(Math.abs(x.toDouble))).toInt - x.digits,
          Math.floor(Math.log10(Math.abs(y.toDouble))).toInt - y.digits)
        Math.max(1, resTopDigit - mostLeastSigDigit)
      } else if (x.digits < Integer.MAX_VALUE)
        Math.max(
          1, 
          resTopDigit - Math.floor(Math.log10(Math.abs(x.toDouble))).toInt + 
            x.digits)
      else if (y.digits < Integer.MAX_VALUE)
        Math.max(
          1, 
          resTopDigit - Math.floor(Math.log10(Math.abs(x.toDouble))).toInt + 
            y.digits)
      else Integer.MAX_VALUE
    }

    // calculates the number of significant digits for log operations
    private[sigfigs] def calculateLogDigits(x: SDType, res: T)(
      implicit numb: Numeric[T]): Int = x.digits + 1

    // calculates the numer of significant digits for rounding operations
    private[sigfigs] def calculateRoundedDigits(x: SDType)(
      implicit numb: Numeric[T]): Int = 
      Math.max(0, Math.floor(Math.log10(Math.abs(x.toDouble))).toInt + 1)
  
    private[sigfigs] def roundResult(res: T, digits: Int): T = {
      implicit val nu: Numeric[T] = num
      val resLeastDigit =
        Math.floor(Math.log10(Math.abs(num.toDouble(res)))).toInt - digits + 1
      val resRounder = Math.pow(10.0, resLeastDigit.toDouble)
      val resRounded = Math.round(num.toDouble(res) / resRounder) * resRounder
      to[T](resRounded)
    }

    private[sigfigs] def makeSigDigits(res: T, digits: Int): 
        SignificantDigits[T] = {
      implicit val nu = num
      implicit val n: SignificantOps[T] = this
      context.roundingMode match {
        case Always => SignificantDigits[T](roundResult(res, digits), digits)
        case Never => SignificantDigits[T](res, digits)
      }
    }

    def compare(x: SDType, y: SDType): Int
  }

  /**
    * super class of all the SignificantDigits Numerics implicits when
    * normal ordering is requested
    */
  abstract class ForwardSigFractionalOps[T](implicit numb: Fractional[T])
      extends SignificantOps[T] with Fractional[SignificantDigits[T]] {
    assert(numb != null)
    protected val num: Fractional[T] = numb
    def compare(x: SDType, y: SDType): Int = x.minus(y).toInt

    override def abs(x: SignificantDigits[T]): SignificantDigits[T] = sdabs(x)
    override def signum(x: SDType): Int = sdsignum(x)
    override def min[U <: SDType](x: U, y: U): U = sdmin(x, y)
    override def max[U <: SDType](x: U, y: U): U = sdmax(x, y)

    override implicit def mkNumericOps(lhs: SDType): FractionalOps =
      new SignificantOpsFractional(lhs)

    /** class which injects the math operator overloads +,-,*,/ into the
      * calling code via an implicit type conversion, used by non whole
      * Numeric types: Double, Float, etc */
    class SignificantOpsFractional(lhs: SDType) extends FractionalOps(lhs)
  }

  /**
    * super class of all the SignificantDigits Numerics implicits when
    * reverse ordering is requested
    */
  abstract class ReverseSigFractionalOps[T](implicit numb: Fractional[T])
      extends SignificantOps[T] with Fractional[SignificantDigits[T]] {
    protected val num: Fractional[T] = numb
    def compare(x: SDType, y: SDType): Int = y.minus(x).toInt

    override def abs(x: SignificantDigits[T]): SignificantDigits[T] = sdabs(x)
    override def signum(x: SDType): Int = sdsignum(x)
    override def min[U <: SDType](x: U, y: U): U = sdmin(x, y)
    override def max[U <: SDType](x: U, y: U): U = sdmax(x, y)

    override implicit def mkNumericOps(lhs: SDType): FractionalOps =
      new SignificantOpsFractional(lhs)

    /** class which injects the math operator overloads +,-,*,/ into the
      * calling code via an implicit type conversion, used by non whole
      * Numeric types: Double, Float, etc */
    class SignificantOpsFractional(lhs: SDType) extends FractionalOps(lhs)
  }

  /**
    * super class of all the SignificantDigits Numerics implicits when
    * normal ordering is requested
    */
  abstract class ForwardSigIntegralOps[T](implicit numb: Integral[T])
      extends SignificantOps[T] with Integral[SignificantDigits[T]] {
    assert(numb != null)
    protected val num: Integral[T] = numb
    def compare(x: SDType, y: SDType): Int = x.minus(y).toInt

    override def abs(x: SignificantDigits[T]): SignificantDigits[T] = sdabs(x)
    override def signum(x: SDType): Int = sdsignum(x)
    override def min[U <: SDType](x: U, y: U): U = sdmin(x, y)
    override def max[U <: SDType](x: U, y: U): U = sdmax(x, y)

    override implicit def mkNumericOps(lhs: SDType): IntegralOps =
      new SignificantOpsIntegral(lhs)

    /** class which injects the math operator overloads +,-,*,/,% into the
      * calling code via an implicit type conversion, used by whole
      * Numeric types: Int, Long etc
      */
    class SignificantOpsIntegral(lhs: SDType) extends IntegralOps(lhs)
  }

  /**
    * super class of all the SignificantDigits Numerics implicits when
    * reverse ordering is requested
    */
  abstract class ReverseSigIntegralOps[T](implicit numb: Integral[T])
      extends SignificantOps[T] with Integral[SignificantDigits[T]] {
    protected val num: Integral[T] = numb
    def compare(x: SDType, y: SDType): Int = y.minus(x).toInt

    override def abs(x: SignificantDigits[T]): SignificantDigits[T] = sdabs(x)
    override def signum(x: SDType): Int = sdsignum(x)
    override def min[U <: SDType](x: U, y: U): U = sdmin(x, y)
    override def max[U <: SDType](x: U, y: U): U = sdmax(x, y)

    override implicit def mkNumericOps(lhs: SDType): IntegralOps =
      new SignificantOpsIntegral(lhs)

    /** class which injects the math operator overloads +,-,*,/,% into the
      * calling code via an implicit type conversion, used by whole
      * Numeric types: Int, Long etc
      */
    class SignificantOpsIntegral(lhs: SDType) extends IntegralOps(lhs)
  }
}

import SignificantDigits._

/**
  * A wrapper class for Scala Numerics that keeps track of the number of
  * significant digits.  Takes an implicit SDContext to control its behavior
  * during its lifetime.  This class is immutable.  Most of the actual work
  * of basic math operations is here because we have access to the SDContext
  * here.  More complex operations (trig, log, exp, etc) are handled outside
  * of this class but work in much the same way.  New instances of this class
  * always use the singleton SDContext in use at the time of their creation.
  */
final class SignificantDigits[T](_v: T, _digits: Int)(
  implicit n: SignificantOps[T], num: Numeric[T], c: SDContext) 
    extends util.PrettyPrinter {

  type SDType = SignificantDigits[T]
  type UType = T

  /** accessor for the value being wrapped by this class */
  def v: T = _v
  /** number of significant digits in this number */
  def digits: Int = _digits

  /** adds this instance to another instance of the same data type */
  def plus(other: SDType): SDType = {
    val res: T = num.plus(_v, other.v)
    val newDigits = n.calculateAddSubDigits(this, other, res)
    c.roundingMode match {
      case Always => 
        SignificantDigits[T](n.roundResult(res, newDigits), newDigits)
      case Never => SignificantDigits[T](res, newDigits)
    }
  }

  /** subtracts another instance from this instance of the same data type */
  def minus(other: SDType): SDType = {
    val res: T = num.minus(_v, other.v)
    val newDigits = n.calculateAddSubDigits(this, other, res)
    c.roundingMode match {
      case Always => 
        SignificantDigits[T](n.roundResult(res, newDigits), newDigits)
      case Never => SignificantDigits[T](res, newDigits)
    }
  }

  // for multiple/divide/negate just use the smallest number of sig digits
  // of the operands
  /** negates this value ie multiply by -1 */
  def negate(): SDType = SignificantDigits[T](num.negate(_v), _digits)
  /** multiplies two instances together */
  def times(other: SDType): SDType = {
    val res = num.times(_v, other.v)
    val newDigits = Math.min(_digits, other.digits)
    c.roundingMode match {
      case Always => 
        SignificantDigits[T](n.roundResult(res, newDigits), newDigits)
      case Never => SignificantDigits[T](res, newDigits)
    }
  }

  /** divides this instance by the other instance */
  def div(other: SDType): SDType = num match {
    case f: Fractional[T] => {
      val res = f.div(_v, other.v)
      val newDigits = Math.min(_digits, other.digits)
      c.roundingMode match {
        case Always =>
          SignificantDigits[T](n.roundResult(res, newDigits), newDigits)
        case Never => SignificantDigits[T](res, newDigits)
      }
    }
    case _ => throw new Exception(s"div not supported for $num")
  }

  /** whole number division, divides ths instance by the other instance */
  def quot(other: SDType): SDType = num match {
    case i: Integral[T] => {
      val res = i.quot(_v, other.v)
      val newDigits = Math.min(_digits, other.digits)
      c.roundingMode match {
        case Always =>
          SignificantDigits[T](n.roundResult(res, newDigits), newDigits)
        case Never => SignificantDigits[T](res, newDigits)
      }
    }
    case _ => throw new Exception(s"quot not supported for $num")
  }

  /** remainder operation: returns the remainder after dividing this by other */
  def rem(other: SDType): SDType = num match {
    case i: Integral[T] => {
      val res = i.rem(_v, other.v)
      val newDigits = Math.min(_digits, other.digits)
      c.roundingMode match {
        case Always =>
          SignificantDigits[T](n.roundResult(res, newDigits), newDigits)
        case Never => SignificantDigits[T](res, newDigits)
      }
    }
    case _ => throw new Exception(s"rem not supported for $num")
  }

  /** returns this value as a double, note loses SD functionality, 
    * uses the underlying value's Numeric type to perform conversion */
  def toDouble: Double = num.toDouble(_v)
  /** returns this value as a float, note loses SD functionality
    * uses the underlying value's Numeric type to perform conversion */
  def toFloat: Float = num.toFloat(_v)
  /** returns this value as an intger, note loses SD functionality
    * uses the underlying value's Numeric type to perform conversion */
  def toInt: Int = num.toInt(_v)
  /** returns this value as a long, note loses SD functionality 
    * uses the underlying value's Numeric type to perform conversion */
  def toLong: Long = num.toLong(_v)

  /** declared in scala.math.Ordering, returns (this - other).toInt */
  def compare(other: SDType): Int = n.compare(this, other)

  private[this] lazy val leastDigit =
    Math.floor(Math.log10(Math.abs(toDouble))).toInt - _digits + 1
  private[this] lazy val rounder = Math.pow(10.0, leastDigit.toDouble)
  lazy val rounded = roundToSD

  private[this] def roundToSD: T = {
    val rounded = Math.round(toDouble / rounder) * rounder
    n.to[T](rounded)
  }

  private[this] lazy val df: DecimalFormat = {
    val df = new DecimalFormat("#")
    // TODO disable sci notation somehow???
    val digits = Math.abs(Math.min(0, leastDigit))
    df.setMinimumFractionDigits(digits)
    df.setMaximumFractionDigits(digits)
    df
  }

  override def equals(o: Any): Boolean = o match {
    case sd: SignificantDigits[_] => sd.v == v && sd.digits == digits
    case _ => false
  }

  /** returns the raw value as an unformatted string, mostly for debugging */
  def rawString: String = _v.toString
  
  /** 
    * does bolding of signficant digits and displays the underlying value 
    * rounded to the appropiate power of 10 
    */
  override def toString: String =
    if (_digits < Integer.MAX_VALUE) {
      val addNegSign = num.lt(_v, num.zero)
      val str = 
        if (addNegSign) df.format(num.negate(rounded)) else df.format(rounded)
      val prefix = str.takeWhile { c => c == '0' || c == '.' }
      val midEndIdx = prefix.size + _digits
      val mid = str.slice(prefix.size, midEndIdx)
      if (!mid.contains('.'))
        (if (addNegSign) "-" else "") +
          prefix + c.highlight(mid) + str.slice(midEndIdx, str.size)
      else
        (if (addNegSign) "-" else "") + prefix +
          c.highlight(str.slice(prefix.size, midEndIdx + 1)) +
          str.slice(midEndIdx + 1, str.size)
    } else {
      c.highlight(df.format(rounded))
    }
}
