
package org

import java.lang.Math
import sigfigs.SignificantDigits._

/**
  * The sigfigs package that when imported adds the SignificantDigits 
  * capabilities
  * to a piece of Scala code.  It includes all the standard math operations
  * commonly found in scala.math or java.math.Math.  It also includes some
  * type alias: SDI, SDL, SDF, SDD to use in place of Int, Long, Float and 
  * Double
  */
package object sigfigs {

  /** wraps the sin method, keeps the number of significant digits the same */
  def sin(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] =
    n.makeSigDigits(Math.sin(d.v), d.digits)

  /** wraps the cos method, keeps the number of significant digits the same */
  def cos(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] =
    n.makeSigDigits(Math.cos(d.v), d.digits)

  /** wraps the tan method, keeps the number of significant digits the same */
  def tan(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] =
    n.makeSigDigits(Math.tan(d.v), d.digits)

  /** wraps the asin method, keeps the number of significant digits the same */
  def asin(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] =
    n.makeSigDigits(Math.asin(d.v), d.digits)

  /** wraps the acos method, keeps the number of significant digits the same */
  def acos(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] = 
    n.makeSigDigits(Math.acos(d.v), d.digits)

  /** wraps the atan method, keeps the number of significant digits the same */
  def atan(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] = 
    n.makeSigDigits(Math.atan(d.v), d.digits)

  /** wraps the toRadians method, keeps the number of significant digits the 
    * same */
  def toRadians(d: SignificantDigits[Double])(
    implicit n: SignificantOps[Double]): SignificantDigits[Double] =
    n.makeSigDigits(Math.toRadians(d.v), d.digits)

  /** wraps the toDegrees method, keeps the number of significant digits the 
    * same */
  def toDegrees(d: SignificantDigits[Double])(
    implicit n: SignificantOps[Double]): SignificantDigits[Double] =
    n.makeSigDigits(Math.toDegrees(d.v), d.digits)

  /** wraps the atan2 method, sets the number of significant digits to the min
    * of number of significant digits of the two arguments */
  def atan2(y: SignificantDigits[Double], x: SignificantDigits[Double])(
    implicit n: SignificantOps[Double]): SignificantDigits[Double] =
    n.makeSigDigits(
      Math.atan2(y.v, x.v), Math.min(y.digits, x.digits))

  /** wraps the hypot method, sets the number of significant digits to the min
    * of number of significant digits of the two arguments */
  def hypot(y: SignificantDigits[Double], x: SignificantDigits[Double])(
    implicit n: SignificantOps[Double]): 
      SignificantDigits[Double] =
    n.makeSigDigits(Math.hypot(y.v, x.v), Math.min(y.digits, x.digits))

  /** wraps the ceil method, sets the significant digits to the number of 
    * significant digits to the left of the decimal point */
  def ceil(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] = 
    n.makeSigDigits(Math.ceil(d.v), n.calculateRoundedDigits(d))

  /** wraps the ceil method, sets the significant digits to the number of 
    * significant digits to the left of the decimal point */
  def floor(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] = 
    n.makeSigDigits(Math.floor(d.v), n.calculateRoundedDigits(d))

  /** wraps the ceil method, sets the significant digits to the number of 
    * significant digits to the left of the decimal point */
  def rint(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] = 
    n.makeSigDigits(Math.rint(d.v), n.calculateRoundedDigits(d))

  /** wraps the ceil method, sets the significant digits to the number of 
    * significant digits to the left of the decimal point */
  def round[T, T2](x: SignificantDigits[T])(
    implicit n: SignificantOps[T], n2: SignificantOps[T2]):
      SignificantDigits[T2] = n.round[T2](x)

  /** wraps the abs method, keeps the number of significant digits the same */
  def abs[T](x: SignificantDigits[T])(
    implicit n: SignificantOps[T]):
      SignificantDigits[T] = n.abs(x)

  /** delegates to the SignificantOps[T].max method */
  def max[T](x: SignificantDigits[T], y: SignificantDigits[T])(
    implicit n: SignificantOps[T]): SignificantDigits[T] = n.max(x, y)

  /** delegates to the SignificantOps[T].min method */
  def min[T](x: SignificantDigits[T], y: SignificantDigits[T])(
    implicit n: SignificantOps[T]): SignificantDigits[T] = n.min(x, y)

  /** delegates to the SignificantOps[T].signum method */
  def signum[T](x: SignificantDigits[T])(
    implicit n: SignificantOps[T]):
      SignificantDigits[T] = n.nativeSignum(x)

  /** wraps the sqrt method, keeps the number of significant digits the same */
  def sqrt(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] = 
    n.makeSigDigits(Math.sqrt(d.v), d.digits)

  /** wraps the cbrt method, keeps the number of significant digits the same */
  def cbrt(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] = 
    n.makeSigDigits(Math.cbrt(d.v), d.digits)

  /** wraps the hypot method, sets the number of significant digits to the min
    * of number of significant digits of the two arguments */
  def pow(x: SignificantDigits[Double], y: SignificantDigits[Double])(
    implicit n: SignificantOps[Double]): 
      SignificantDigits[Double] =
    n.makeSigDigits(Math.pow(x.v, y.v), Math.min(x.digits, y.digits))

  /** wraps the exp method, keeps the number of significant digits the same */
  def exp(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] =
    n.makeSigDigits(Math.exp(d.v), d.digits)

  /** wraps the expm1 method, keeps the number of significant digits the same */
  def expm1(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] =
    n.makeSigDigits(Math.expm1(d.v), d.digits)

  /** wraps the log method, and increments the number of significant digits 
    * by 1 */
  def log(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] = {
    val res = Math.log(d.v)
    n.makeSigDigits(res, n.calculateLogDigits(d, res))
  }

  /** wraps the log1p method, and increments the number of significant digits 
    * by 1 */
  def log1p(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] = {
    val res = Math.log1p(d.v)
    n.makeSigDigits(res, n.calculateLogDigits(d, res))
  }

  /** wraps the log10 method, and increments the number of significant digits 
    * by 1 */
  def log10(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] = {
    val res = Math.log10(d.v)
    n.makeSigDigits(res, n.calculateLogDigits(d, res))
  }

  /** wraps the sinh method, keeps the number of significant digits the same */
  def sinh(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] =
    n.makeSigDigits(Math.sinh(d.v), d.digits)

  /** wraps the cosh method, keeps the number of significant digits the same */
  def cosh(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] =
    n.makeSigDigits(Math.cosh(d.v), d.digits)

  /** wraps the tanh method, keeps the number of significant digits the same */
  def tanh(d: SignificantDigits[Double])(implicit n: SignificantOps[Double]):
      SignificantDigits[Double] =
    n.makeSigDigits(Math.tanh(d.v), d.digits)

  /** wraps the ulp method, sets the number of significant digits to 1 */
  def ulp[T](x: SignificantDigits[T])(implicit n: SignificantOps[T]):
      SignificantDigits[T] = n.ulp(x)

  /** wraps the IEEEremainder method, sets the number of significant digits to 
    * the min of number of significant digits of the two arguments */
  def IEEEremainder(x: SignificantDigits[Double], y: SignificantDigits[Double])(
    implicit n: SignificantOps[Double]): 
      SignificantDigits[Double] =
    n.makeSigDigits(Math.IEEEremainder(x.v, y.v), Math.min(x.digits, y.digits))

  // implicit Numeric implementations and implicit type conversions needed
  // to make the SignificantDigits[Byte] work
  implicit object ByteSigOps extends ForwardSigOps[Byte] {
    def to[T](str: String)(implicit n: Numeric[T]): T =
      str.toByte.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.toByte.asInstanceOf[T]
    override def reverse: Ordering[SDType] = ReverseByteSigOps
  }

  // implicit type conversion that injects the Ops classes with all the correct
  // operator overloads into the SignificantDigits[Byte] type
  implicit def mkByteOps(left: SignificantDigits[Byte]):
      ByteSigOps.SignificantOpsIntegral =
    new ByteSigOps.SignificantOpsIntegral(left)

  // same as ByteSigOps but provides a reversed ordering
  object ReverseByteSigOps extends ReverseSigOps[Byte] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      str.toByte.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.toByte.asInstanceOf[T]
    override def reverse: Ordering[SDType] = ByteSigOps
  }

  // implicit Numeric implementations and implicit type conversions needed
  // to make the SignificantDigits[Short] work
  implicit object ShortSigOps extends ForwardSigOps[Short] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      str.toShort.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.toShort.asInstanceOf[T]
    override def reverse: Ordering[SDType] = ReverseShortSigOps
  }

  // implicit type conversion that injects the Ops classes with all the correct
  // operator overloads into the SignificantDigits[Short] type
  implicit def mkShortOps(left: SignificantDigits[Short]):
      ShortSigOps.SignificantOpsIntegral =
    new ShortSigOps.SignificantOpsIntegral(left)

  // same as ShortSigOps but provides a reversed ordering
  object ReverseShortSigOps extends ReverseSigOps[Short] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      str.toShort.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.toShort.asInstanceOf[T]
    override def reverse: Ordering[SDType] = ShortSigOps
  }

  // implicit Numeric implementations and implicit type conversions needed
  // to make the SignificantDigits[Int] work
  implicit object IntSigOps extends ForwardSigOps[Int] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      str.toInt.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.toInt.asInstanceOf[T]
    override def reverse: Ordering[SDType] = ReverseIntSigOps
  }

  // implicit type conversion that injects the Ops classes with all the correct
  // operator overloads into the SignificantDigits[Int] type
  implicit def mkIntOps(left: SignificantDigits[Int]):
      IntSigOps.SignificantOpsIntegral =
    new IntSigOps.SignificantOpsIntegral(left)

  // same as IntSigOps but provides a reversed ordering
  object ReverseIntSigOps extends ReverseSigOps[Int] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      str.toInt.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.toInt.asInstanceOf[T]
    override def reverse: Ordering[SDType] = IntSigOps
  }

  // implicit Numeric implementations and implicit type conversions needed
  // to make the SignificantDigits[Long] work
  implicit object LongSigOps extends ForwardSigOps[Long] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      str.toLong.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.toLong.asInstanceOf[T]
    override def reverse: Ordering[SDType] = ReverseLongSigOps
  }

  // implicit type conversion that injects the Ops classes with all the correct
  // operator overloads into the SignificantDigits[Long] type
  implicit def mkLongOps(left: SignificantDigits[Long]):
      LongSigOps.SignificantOpsIntegral =
    new LongSigOps.SignificantOpsIntegral(left)

  // same as LongSigOps but provides a reversed ordering
  object ReverseLongSigOps extends ReverseSigOps[Long] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      str.toLong.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.toLong.asInstanceOf[T]
    override def reverse: Ordering[SDType] = LongSigOps
  }

  // implicit Numeric implementations and implicit type conversions needed
  // to make the SignificantDigits[Float] work
  implicit object FloatSigOps extends ForwardSigOps[Float] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      str.toFloat.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.toFloat.asInstanceOf[T]
    override def reverse: Ordering[SDType] = ReverseFloatSigOps
    override def round[T2](x: SDType)(implicit o: SignificantOps[T2]):
        SignificantDigits[T2] = 
      o.makeSigDigits(
        Math.round(x.toFloat).asInstanceOf[T2], calculateRoundedDigits(x))
    override def ulp(x: SDType): SDType = makeSigDigits(Math.ulp(x.toFloat), 1)
  }

  // implicit type conversion that injects the Ops classes with all the correct
  // operator overloads into the SignificantDigits[Float] type
  implicit def mkFloatOps(left: SignificantDigits[Float]): 
      FloatSigOps.FractionalOps =
    new FloatSigOps.SignificantOpsFractional(left)

  // same as FloatSigOps but provides a reversed ordering
  object ReverseFloatSigOps extends ReverseSigOps[Float] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      str.toFloat.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.toFloat.asInstanceOf[T]
    override def reverse: Ordering[SDType] = FloatSigOps
  }

  // implicit Numeric implementations and implicit type conversions needed
  // to make the SignificantDigits[Double] work
  implicit object DoubleSigOps extends ForwardSigOps[Double] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      str.toDouble.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.asInstanceOf[T]
    override def reverse: Ordering[SDType] = ReverseDoubleSigOps
    override def round[T2](x: SDType)(implicit o: SignificantOps[T2]):
        SignificantDigits[T2] =
      o.makeSigDigits(
        Math.round(x.toDouble).asInstanceOf[T2], calculateRoundedDigits(x))
    override def ulp(x: SDType): SDType = makeSigDigits(Math.ulp(x.v), 1)
  }

  // implicit type conversion that injects the Ops classes with all the correct
  // operator overloads into the SignificantDigits[Double] type
  implicit def mkDoubleOps(left: SignificantDigits[Double]): 
      DoubleSigOps.FractionalOps =
    new DoubleSigOps.SignificantOpsFractional(left)

  // same as DoubleSigOps but provides a reversed ordering
  object ReverseDoubleSigOps extends ReverseSigOps[Double] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      str.toDouble.asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = d.asInstanceOf[T]
    override def reverse: Ordering[SDType] = DoubleSigOps
  }

  // implicit Numeric implementations and implicit type conversions needed
  // to make the SignificantDigits[BigInt] work
  implicit object BigIntSigOps extends ForwardSigOps[BigInt] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      BigInt(str).asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = 
      BigInt(d.toLong).asInstanceOf[T]
    override def reverse: Ordering[SDType] = ReverseBigIntSigOps
  }

  // implicit type conversion that injects the Ops classes with all the correct
  // operator overloads into the SignificantDigits[BigInt] type
  implicit def mkBigIntOps(left: SignificantDigits[BigInt]):
      BigIntSigOps.SignificantOpsIntegral =
    new BigIntSigOps.SignificantOpsIntegral(left)

  // same as BigIntSigOps but provides a reversed ordering
  object ReverseBigIntSigOps extends ReverseSigOps[BigInt] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      BigInt(str).asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = 
      BigInt(d.toLong).asInstanceOf[T]
    override def reverse: Ordering[SDType] = BigIntSigOps
  }

  // implicit Numeric implementations and implicit type conversions needed
  // to make the SignificantDigits[BigDecimal] work
  implicit object BigDecimalSigOps extends ForwardSigOps[BigDecimal] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      BigDecimal(str).asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = 
      BigDecimal(d).asInstanceOf[T]
    override def reverse: Ordering[SDType] = ReverseBigDecimalSigOps
  }

  // implicit type conversion that injects the Ops classes with all the correct
  // operator overloads into the SignificantDigits[BigDecimal] type
  implicit def mkBigDecimalOps(left: SignificantDigits[BigDecimal]):
      BigDecimalSigOps.FractionalOps =
    new BigDecimalSigOps.SignificantOpsFractional(left)

  // same as BigDecimalSigOps but provides a reversed ordering
  object ReverseBigDecimalSigOps extends ReverseSigOps[BigDecimal] {
    def to[T](str: String)(implicit n: Numeric[T]): T = 
      BigDecimal(str).asInstanceOf[T]
    def to[T](d: Double)(implicit n: Numeric[T]): T = 
      BigDecimal(d).asInstanceOf[T]
    override def reverse: Ordering[SDType] = BigDecimalSigOps
  }

  // to T value from SignificantDigits
  implicit def fromSigDigits[T](t: SignificantDigits[T])(
    implicit n: SignificantOps[T], numb: Numeric[T], c: SDContext): T = t.v

  // from T value to SignificantDigits
  implicit def toSigDigits[T](t: T)(
    implicit n: SignificantOps[T], numb: Numeric[T]): 
      SignificantDigits[T] =
    SignificantDigits[T](t, java.lang.Integer.MAX_VALUE)

  // from string to SignificantDigits
  implicit def toSigDigitsFromString[T](str: String)(
    implicit n: SignificantOps[T], numb: Numeric[T]):
      SignificantDigits[T] =
    SignificantDigits[T](n.to[T](str), computeDigits[T](str, false))

  // universal convertor for all SignificantDigits types
  // conversions between SignificantDigits[T1] -> SignificantDigits[T2]
  implicit def converter[T1, T2](t: SignificantDigits[T1])(
    implicit n1: SignificantOps[T1], numb1: Numeric[T1],
      n2: SignificantOps[T2], numb2: Numeric[T2]): SignificantDigits[T2] =
    SignificantDigits[T2](n2.to[T2](t.toDouble), t.digits)


  /** syntatic sugar to save typing for users */
  trait SignificantDigitsSugar[T] {
    /** wrapper for the SignificantDigits[T](v: T) constructor */
    def apply(v: T)(implicit n: SignificantOps[T], nu: Numeric[T]):
        SignificantDigits[T] =
      SignificantDigits.apply[T](v)

    /** wrapper for the SignificantDigits[T](v: String) constructor */
    def apply(v: String)(implicit n: SignificantOps[T], nu: Numeric[T]):
        SignificantDigits[T] =
      SignificantDigits.apply[T](v)

    /** wrapper for the SignificantDigits[T](v: T, digits: Int) constructor */
    def apply(v: T, digits: Int)(implicit n: SignificantOps[T], nu: Numeric[T]):
        SignificantDigits[T] =
      SignificantDigits.apply[T](v, digits)
  }

  // syntatic sugar for the Significant Digits Int type
  type SDI = SignificantDigits[Int]
  object SDI extends SignificantDigitsSugar[Int]

  // syntatic sugar for the Significant Digits Long type
  type SDL = SignificantDigits[Long]
  object SDL extends SignificantDigitsSugar[Long]

  // syntatic sugar for the Significant Digits Float type
  type SDF = SignificantDigits[Float]
  object SDF extends SignificantDigitsSugar[Float]

  // syntatic sugar for the Significant Digits Double type
  type SDD = SignificantDigits[Double]
  object SDD extends SignificantDigitsSugar[Double]
}


