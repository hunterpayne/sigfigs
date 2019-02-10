
package org.sigfigs

import org.scalatest.{ FlatSpec, Matchers }
import scala.math.{ BigInt, BigDecimal }

class SignificantDigitsCustomSpec 
    extends FlatSpec with Matchers with util.PrettyPrinter {

  behavior of "Significant Custom Digits"

  it should "be automatically computed" in {

    SignificantDigits.setSDContext(new SDContext {

      override def highlight(s: String): String = s
      override def roundingMode(): SDRoundingMode.Value = SDRoundingMode.Always
    })

    // infinite significant digits
    val constInt: SDI = 2300
    val constDouble: SDD = 14.75

    // different numbers of significant digits expressed differently
    val digits = SDI(2300) // trims trailing zeros
    val digits1: SDI = "2300" // all given digits are sd
    val digits2 = SDI(2300, 3) // fixed number of sd
    val digits3 = SDI("2300") // all given digits are sd

    val sig = SDD(2300.0) // trims trailing zeros
    val sig1 = SDD(24.0) // trims trailing zeros
    val sig2 = SDD("24.303")
    val sig3 = SDD(2301.1)
    val sig4 = SDD(0.000122300) // trims trailing zeros
    val sig5: SDD = "0.000122300" // keeps all 6 SDs

    constInt.digits should be (Integer.MAX_VALUE)
    constDouble.digits should be (Integer.MAX_VALUE)

    digits.digits should be (2)
    digits1.digits should be (4)
    digits2.digits should be (3)
    digits3.digits should be (4)

    sig.digits should be (2)
    sig1.digits should be (2)
    sig2.digits should be (5)
    sig3.digits should be (5)
    sig4.digits should be (4)
    sig5.digits should be (6)

    digits.toString should be ("2300")
    digits1.toString should be ("2300")
    digits2.toString should be ("2300")
    digits3.toString should be ("2300")

    sig.toString should be ("2300")
    sig1.toString should be ("24")
    sig2.toString should be ("24.303")
    sig3.toString should be ("2301.1")
    sig4.toString should be (".0001223")
    sig5.toString should be (".000122300")
  }

  it should "handle basic byte math operations" in {

    val sig = SignificantDigits[Byte](100.toByte)
    val sig1 = SignificantDigits[Byte](24.toByte)
    val sig2 = SignificantDigits[Byte](12.toByte)
    val sig3 = SignificantDigits[Byte](6.toByte)

    val sumDigits = sig + sig1
    sumDigits.digits should be (1)
    sumDigits.toInt should be (100)
    sumDigits.toString should be ("100")

    val subDigits = sig - sig1
    subDigits.digits should be (1)
    subDigits.toInt should be (80)
    subDigits.toString should be ("80")

    val negDigits = -sig
    negDigits.digits should be (1)
    negDigits.toInt should be (-100)
    negDigits.toString should be ("-100")

    val multDigits = sig2 * sig3
    multDigits.digits should be (1)
    multDigits.toInt should be (70)
    multDigits.toString should be ("70")

    val divDigits = sig2 / sig3
    divDigits.digits should be (1)
    divDigits.toInt should be (2)
    divDigits.toString should be ("2")
  }

  it should "handle basic short math operations" in {

    val sig = SignificantDigits[Short](100.toShort)
    val sig1 = SignificantDigits[Short](24.toShort)
    val sig2 = SignificantDigits[Short](12.toShort)
    val sig3 = SignificantDigits[Short](6.toShort)

    val sumDigits = sig + sig1
    sumDigits.digits should be (1)
    sumDigits.toInt should be (100)
    sumDigits.toString should be ("100")

    val subDigits = sig - sig1
    subDigits.digits should be (1)
    subDigits.toInt should be (80)
    subDigits.toString should be ("80")

    val negDigits = -sig
    negDigits.digits should be (1)
    negDigits.toInt should be (-100)
    negDigits.toString should be ("-100")

    val multDigits = sig2 * sig3
    multDigits.digits should be (1)
    multDigits.toInt should be (70)
    multDigits.toString should be ("70")

    val divDigits = sig2 / sig3
    divDigits.digits should be (1)
    divDigits.toInt should be (2)
    divDigits.toString should be ("2")
  }

  it should "handle basic int math operations" in {

    val sig = SDI(100.toInt)
    val sig1 = SDI(24.toInt)
    val sig2 = SDI(12.toInt)
    val sig3 = SDI(6.toInt)

    val sumDigits = sig + sig1
    sumDigits.digits should be (1)
    sumDigits.toInt should be (100)
    sumDigits.toString should be ("100")

    val subDigits = sig - sig1
    subDigits.digits should be (1)
    subDigits.toInt should be (80)
    subDigits.toString should be ("80")

    val negDigits = -sig
    negDigits.digits should be (1)
    negDigits.toInt should be (-100)
    negDigits.toString should be ("-100")

    val multDigits = sig2 * sig3
    multDigits.digits should be (1)
    multDigits.toInt should be (70)
    multDigits.toString should be ("70")

    val divDigits = sig2 / sig3
    divDigits.digits should be (1)
    divDigits.toInt should be (2)
    divDigits.toString should be ("2")
  }

  it should "handle basic long math operations" in {

    val sig = SDL(100.toLong)
    val sig1 = SDL(24.toLong)
    val sig2 = SDL(12.toLong)
    val sig3 = SDL(6.toLong)

    val sumDigits = sig + sig1
    sumDigits.digits should be (1)
    sumDigits.toInt should be (100)
    sumDigits.toString should be ("100")

    val subDigits = sig - sig1
    subDigits.digits should be (1)
    subDigits.toInt should be (80)
    subDigits.toString should be ("80")

    val negDigits = -sig
    negDigits.digits should be (1)
    negDigits.toInt should be (-100)
    negDigits.toString should be ("-100")

    val multDigits = sig2 * sig3
    multDigits.digits should be (1)
    multDigits.toInt should be (70)
    multDigits.toString should be ("70")

    val divDigits = sig2 / sig3
    divDigits.digits should be (1)
    divDigits.toInt should be (2)
    divDigits.toString should be ("2")
  }

  it should "handle basic float math operations" in {

    implicit val tolerance = 0.00001

    val sig = SDF(100.1f, 4)
    val sig1 = SDF(24.23f, 4)
    val sig2 = SDF(12.0f)
    val sig3 = SDF(6.0f)

    sig.digits should be (4)
    sig1.digits should be (4)
    sig2.digits should be (2)
    sig3.digits should be (1)

    val sumDigits = sig + sig1
    sumDigits.digits should be (4)
    assert(Math.abs(sumDigits.toDouble - 124.3) < tolerance)
    sumDigits.toString should be ("124.3")

    val subDigits = sig - sig1
    subDigits.digits should be (3)
    assert(Math.abs(subDigits.toDouble - 75.9) < tolerance)
    subDigits.toString should be ("75.9")

    val negDigits = -sig
    negDigits.digits should be (4)
    assert(Math.abs(negDigits.toDouble + 100.1) < tolerance)
    negDigits.toString should be ("-100.1")

    val multDigits = sig2 * sig3
    multDigits.digits should be (1)
    multDigits.toDouble should be (70.0)
    multDigits.toString should be ("70")

    val divDigits = sig2 / sig3
    divDigits.digits should be (1)
    divDigits.toInt should be (2)
    divDigits.toString should be ("2")
  }

  it should "handle basic double math operations" in {

    val sig = SDD(2300.0)
    val sig1 = SDD(24.0)

    val sumDigits = sig + sig1
    sumDigits.digits should be (2)
    sumDigits.toInt should be (2300)
    sumDigits.toString should be ("2300")

    val subDigits = sig - sig1
    subDigits.digits should be (2)
    subDigits.toInt should be (2300)
    subDigits.toString should be ("2300")

    val negDigits = -sig
    negDigits.digits should be (2)
    negDigits.toInt should be (-2300)
    negDigits.toString should be ("-2300")

    val multDigits = sig * sig1
    multDigits.digits should be (2)
    multDigits.toInt should be (55000)
    multDigits.toString should be ("55000")

    val divDigits = sig / sig1
    divDigits.digits should be (2)
    divDigits.toDouble should be (96)
    divDigits.toString should be ("96")
  }

  it should "handle basic BigInt math operations" in {

    val sig = SignificantDigits[BigInt](2300, 2)
    val sig1 = SignificantDigits[BigInt](24)

    val sumDigits = sig + sig1
    sumDigits.digits should be (2)
    sumDigits.toInt should be (2300)
    sumDigits.toString should be ("2300")

    val subDigits = sig - sig1
    subDigits.digits should be (2)
    subDigits.toInt should be (2300)
    subDigits.toString should be ("2300")

    val negDigits = -sig
    negDigits.digits should be (2)
    negDigits.toInt should be (-2300)
    negDigits.toString should be ("-2300")

    val multDigits = sig * sig1
    multDigits.digits should be (2)
    multDigits.toInt should be (55000)
    multDigits.toString should be ("55000")

    val divDigits = sig / sig1
    divDigits.digits should be (2)
    divDigits.toDouble should be (95)
    divDigits.toString should be ("95")
  }

  it should "handle basic BigDecimal math operations" in {

    val sig = SignificantDigits[BigDecimal](2300.0)
    val sig1 = SignificantDigits[BigDecimal](24.0)

    val sumDigits = sig + sig1
    sumDigits.digits should be (2)
    sumDigits.toInt should be (2300)
    sumDigits.toString should be ("2300")

    val subDigits = sig - sig1
    subDigits.digits should be (2)
    subDigits.toInt should be (2300)
    subDigits.toString should be ("2300")

    val negDigits = -sig
    negDigits.digits should be (2)
    negDigits.toInt should be (-2300)
    negDigits.toString should be ("-2300")

    val multDigits = sig * sig1
    multDigits.digits should be (2)
    multDigits.toInt should be (55000)
    multDigits.toString should be ("55000")

    val divDigits = sig / sig1
    divDigits.digits should be (2)
    divDigits.toDouble should be (96)
    divDigits.toString should be ("96")
  }

  it should "handle double exponential and log math operations" in {

    implicit val tolerance = 0.00001
    val sig = SDD(10034.3)
    val sig1 = SDD(2.04)
    val sig2 = SDD(0.5456)

    sig.digits should be (6)
    sig1.digits should be (3)
    sig2.digits should be (4)

    val sinDigits = sin(sig1)
    sinDigits.digits should be (3)
    assert(Math.abs(sinDigits.toDouble - 0.892) < tolerance)
    sinDigits.toString should be (".892")

    val cosDigits = cos(sig1)
    cosDigits.digits should be (3)
    assert(Math.abs(cosDigits.toDouble + 0.452) < tolerance)
    cosDigits.toString should be ("-.452")

    val tanDigits = tan(sig1)
    tanDigits.digits should be (3)
    assert(Math.abs(tanDigits.toDouble + 1.97) < tolerance)
    tanDigits.toString should be ("-1.97")

    val asinDigits = asin(sig2)
    asinDigits.digits should be (4)
    assert(Math.abs(asinDigits.toDouble - 0.5771) < tolerance)
    asinDigits.toString should be (".5771")

    val acosDigits = acos(sig2)
    acosDigits.digits should be (4)
    assert(Math.abs(acosDigits.toDouble - 0.9937) < tolerance)
    acosDigits.toString should be (".9937")

    val atanDigits = atan(sig2)
    atanDigits.digits should be (4)
    assert(Math.abs(atanDigits.toDouble - 0.4995) < tolerance)
    atanDigits.toString should be (".4995")

    val toRadiansDigits = toRadians(sig1)
    toRadiansDigits.digits should be (3)
    assert(Math.abs(toRadiansDigits.toDouble - 0.0356) < tolerance)
    toRadiansDigits.toString should be (".0356")

    val toDegreesDigits = toDegrees(sig1)
    toDegreesDigits.digits should be (3)
    assert(Math.abs(toDegreesDigits.toDouble - 117) < tolerance)
    toDegreesDigits.toString should be ("117")

    val atan2Digits = atan2(sig1, sig2)
    atan2Digits.digits should be (3)
    assert(Math.abs(atan2Digits.toDouble - 1.31) < tolerance)
    atan2Digits.toString should be ("1.31")

    val hypotDigits = hypot(sig, sig1)
    hypotDigits.digits should be (3)
    assert(Math.abs(hypotDigits.toDouble - 10000) < tolerance)
    hypotDigits.toString should be ("10000")

    val ceilDigits = ceil(sig)
    ceilDigits.digits should be (5)
    assert(Math.abs(ceilDigits.toDouble - 10035.0) < tolerance)
    ceilDigits.toString should be ("10035")

    val floorDigits = floor(sig)
    floorDigits.digits should be (5)
    assert(Math.abs(floorDigits.toDouble - 10034.0) < tolerance)
    floorDigits.toString should be ("10034")

    val rintDigits = rint(sig)
    rintDigits.digits should be (5)
    assert(Math.abs(rintDigits.toDouble - 10034.0) < tolerance)
    rintDigits.toString should be ("10034")

    val roundDigits = round[Double, Long](sig)
    roundDigits.digits should be (5)
    assert(Math.abs(roundDigits.toDouble - 10034.0) < tolerance)
    roundDigits.toString should be ("10034")

    val absDigits = abs(sig)
    absDigits.digits should be (6)
    assert(Math.abs(absDigits.toDouble - 10034.3) < tolerance)
    absDigits.toString should be ("10034.3")

    val maxDigits = max(sig, sig1)
    maxDigits.digits should be (6)
    assert(Math.abs(maxDigits.toDouble - 10034.3) < tolerance)
    maxDigits.toString should be ("10034.3")

    val minDigits = min(sig, sig1)
    minDigits.digits should be (3)
    assert(Math.abs(minDigits.toDouble - 2.04) < tolerance)
    minDigits.toString should be ("2.04")

    val signumDigits = signum(sig)
    signumDigits.digits should be (1)
    signumDigits.toDouble should be (1.0)
    signumDigits.toString should be ("1")

    val sqrtDigits = sqrt(sig)
    sqrtDigits.digits should be (6)
    assert(Math.abs(sqrtDigits.toDouble - 100.171) < tolerance)
    sqrtDigits.toString should be ("100.171")

    val cbrtDigits = cbrt(sig)
    cbrtDigits.digits should be (6)
    assert(Math.abs(cbrtDigits.toDouble - 21.5690) < tolerance)
    cbrtDigits.toString should be ("21.5690")

    val powDigits = pow(sig, sig1)
    powDigits.digits should be (3)
    assert(Math.abs(powDigits.toDouble - 146000000) < tolerance)
    powDigits.toString should be ("146000000")

    val expDigits = exp(sig1)
    expDigits.digits should be (3)
    assert(Math.abs(expDigits.toDouble - 7.69) < tolerance)
    expDigits.toString should be ("7.69")

    val expm1Digits = expm1(sig1)
    expm1Digits.digits should be (3)
    assert(Math.abs(expm1Digits.toDouble - 6.69) < tolerance)
    expm1Digits.toString should be ("6.69")

    val logDigits = log(sig)
    logDigits.digits should be (7)
    assert(Math.abs(logDigits.toDouble - 9.2137645) < tolerance)
    logDigits.toString should be ("9.213765")

    val log1pDigits = log1p(sig)
    log1pDigits.digits should be (7)
    assert(Math.abs(log1pDigits.toDouble - 9.213864) < tolerance)
    log1pDigits.toString should be ("9.213864")

    val log10Digits = log10(sig)
    log10Digits.digits should be (7)
    assert(Math.abs(log10Digits.toDouble - 4.001487) < tolerance)
    log10Digits.toString should be ("4.001487")

    val sinhDigits = sinh(sig1)
    sinhDigits.digits should be (3)
    assert(Math.abs(sinhDigits.toDouble - 3.78) < tolerance)
    sinhDigits.toString should be ("3.78")

    val coshDigits = cosh(sig1)
    coshDigits.digits should be (3)
    assert(Math.abs(coshDigits.toDouble - 3.91) < tolerance)
    coshDigits.toString should be ("3.91")

    val tanhDigits = tanh(sig1)
    tanhDigits.digits should be (3)
    assert(Math.abs(tanhDigits.toDouble - 0.967) < tolerance)
    tanhDigits.toString should be (".967")

    val ulpDigits = ulp(sig1)
    ulpDigits.digits should be (1)
    assert(Math.abs(ulpDigits.toDouble) < tolerance)
    ulpDigits.toString should be (".0000000000000004")

    val remDigits = IEEEremainder(sig, sig1)
    remDigits.digits should be (3)
    assert(Math.abs(remDigits.toDouble + 0.46) < tolerance)
    remDigits.toString should be ("-.460")
  }

  it should "handle float exponential and log math operations" in {

    implicit val tolerance = 0.001
    val sig = SDF("10034.3")
    val sig1 = SDF(2.04f, 3)

    sig.digits should be (6)
    sig1.digits should be (3)

    val roundDigits = round[Float, Int](sig)
    roundDigits.digits should be (5)
    assert(Math.abs(roundDigits.toDouble - 10034.0) < tolerance)
    roundDigits.toString should be ("10034")

    val absDigits = abs(sig)
    absDigits.digits should be (6)
    assert(Math.abs(absDigits.toDouble - 10034.3) < tolerance)
    absDigits.toString should be ("10034.3")

    val maxDigits = max(sig, sig1)
    maxDigits.digits should be (6)
    assert(Math.abs(maxDigits.toDouble - 10034.3) < tolerance)
    maxDigits.toString should be ("10034.3")

    val minDigits = min(sig, sig1)
    minDigits.digits should be (3)
    assert(Math.abs(minDigits.toDouble - 2.04) < tolerance)
    minDigits.toString should be ("2.04")

    val signumDigits = signum(sig)
    signumDigits.digits should be (1)
    signumDigits.toDouble should be (1.0)
    signumDigits.toString should be ("1")

    val ulpDigits = ulp(sig1)
    ulpDigits.digits should be (1)
    assert(Math.abs(ulpDigits.toDouble) < tolerance)
    ulpDigits.toString should be (".000000" + "2")
  }

  it should "handle long exponential and log math operations" in {

    val sig = SDL(10034)
    val sig1 = SDL(-24)

    sig.digits should be (5)
    sig1.digits should be (2)

    val absDigits = abs(sig1)
    absDigits.digits should be (2)
    absDigits.toLong should be (24)
    absDigits.toString should be ("24")

    val maxDigits = max(sig, sig1)
    maxDigits.digits should be (5)
    maxDigits.toLong should be (10034)
    maxDigits.toString should be ("10034")

    val minDigits = min(sig, sig1)
    minDigits.digits should be (2)
    minDigits.toLong should be (-24)
    minDigits.toString should be ("-24")

    val signumDigits = signum(sig)
    signumDigits.digits should be (1)
    signumDigits.toDouble should be (1.0)
    signumDigits.toString should be ("1")
  }

  it should "handle int exponential and log math operations" in {

    val sig = SDI(10034)
    val sig1 = SDI(-24)

    sig.digits should be (5)
    sig1.digits should be (2)

    val absDigits = abs(sig1)
    absDigits.digits should be (2)
    absDigits.toLong should be (24)
    absDigits.toString should be ("24")

    val maxDigits = max(sig, sig1)
    maxDigits.digits should be (5)
    maxDigits.toLong should be (10034)
    maxDigits.toString should be ("10034")

    val minDigits = min(sig, sig1)
    minDigits.digits should be (2)
    minDigits.toLong should be (-24)
    minDigits.toString should be ("-24")

    val signumDigits = signum(sig)
    signumDigits.digits should be (1)
    signumDigits.toDouble should be (1.0)
    signumDigits.toString should be ("1")
  }

  it should "handle short exponential and log math operations" in {

    val sig = SignificantDigits[Short](10034.toShort)
    val sig1 = SignificantDigits[Short](-24.toShort)

    sig.digits should be (5)
    sig1.digits should be (2)

    val absDigits = abs(sig1)
    absDigits.digits should be (2)
    absDigits.toLong should be (24)
    absDigits.toString should be ("24")

    val maxDigits = max(sig, sig1)
    maxDigits.digits should be (5)
    maxDigits.toLong should be (10034)
    maxDigits.toString should be ("10034")

    val minDigits = min(sig, sig1)
    minDigits.digits should be (2)
    minDigits.toLong should be (-24)
    minDigits.toString should be ("-24")

    val signumDigits = signum(sig)
    signumDigits.digits should be (1)
    signumDigits.toDouble should be (1.0)
    signumDigits.toString should be ("1")
  }

  it should "handle byte exponential and log math operations" in {

    val sig = SignificantDigits[Byte](103.toByte)
    val sig1 = SignificantDigits[Byte](-24.toByte)

    sig.digits should be (3)
    sig1.digits should be (2)

    val absDigits = abs(sig1)
    absDigits.digits should be (2)
    absDigits.toLong should be (24)
    absDigits.toString should be ("24")

    val maxDigits = max(sig, sig1)
    maxDigits.digits should be (3)
    maxDigits.toLong should be (103)
    maxDigits.toString should be ("103")

    val minDigits = min(sig, sig1)
    minDigits.digits should be (2)
    minDigits.toLong should be (-24)
    minDigits.toString should be ("-24")

    val signumDigits = signum(sig)
    signumDigits.digits should be (1)
    signumDigits.toDouble should be (1.0)
    signumDigits.toString should be ("1")
  }

  it should "handle BigInt exponential and log math operations" in {

    val sig = SignificantDigits[BigInt](10034)
    val sig1 = SignificantDigits[BigInt](-24)

    sig.digits should be (5)
    sig1.digits should be (2)

    val absDigits = abs(sig1)
    absDigits.digits should be (2)
    absDigits.toLong should be (24)
    absDigits.toString should be ("24")

    val maxDigits = max(sig, sig1)
    maxDigits.digits should be (5)
    maxDigits.toLong should be (10034)
    maxDigits.toString should be ("10034")

    val minDigits = min(sig, sig1)
    minDigits.digits should be (2)
    minDigits.toLong should be (-24)
    minDigits.toString should be ("-24")

    val signumDigits = signum(sig)
    signumDigits.digits should be (1)
    signumDigits.toDouble should be (1.0)
    signumDigits.toString should be ("1")
  }

  it should "handle BigDecimal exponential and log math operations" in {

    val sig = SignificantDigits[BigDecimal](10034)
    val sig1 = SignificantDigits[BigDecimal](-24)

    sig.digits should be (5)
    sig1.digits should be (2)

    val absDigits = abs(sig1)
    absDigits.digits should be (2)
    absDigits.toLong should be (24)
    absDigits.toString should be ("24")

    val maxDigits = max(sig, sig1)
    maxDigits.digits should be (5)
    maxDigits.toLong should be (10034)
    maxDigits.toString should be ("10034")

    val minDigits = min(sig, sig1)
    minDigits.digits should be (2)
    minDigits.toLong should be (-24)
    minDigits.toString should be ("-24")

    val signumDigits = signum(sig)
    signumDigits.digits should be (1)
    signumDigits.toDouble should be (1.0)
    signumDigits.toString should be ("1")
  }

  it should "handle conversions between types easily" in {

    val b = SignificantDigits[Byte](31.toByte)
    val s = SignificantDigits[Short](31.toShort)
    val i = SDI(31)
    val l = SDL(31l)
    val f = SDF(31.0f)
    val d = SDD(31.0)
    val bi = SignificantDigits[BigInt](31)
    val bd = SignificantDigits[BigDecimal](31.0)

    b.digits should be (2)
    s.digits should be (2)
    i.digits should be (2)
    l.digits should be (2)
    f.digits should be (2)
    d.digits should be (2)
    bi.digits should be (2)
    bd.digits should be (2)

    val bs: SignificantDigits[Short] = b
    bs.digits should be (b.digits)
    bs.toDouble should be (b.toDouble)
    bs.toString should be (b.toString)

    val bi2: SDI = b
    bi2.digits should be (b.digits)
    bi2.toDouble should be (b.toDouble)
    bi2.toString should be (b.toString)

    val bl: SDL = b
    bl.digits should be (b.digits)
    bl.toDouble should be (b.toDouble)
    bl.toString should be (b.toString)

    val bf: SDF = b
    bf.digits should be (b.digits)
    bf.toDouble should be (b.toDouble)
    bf.toString should be (b.toString)

    val bd2: SDD = b
    bd2.digits should be (b.digits)
    bd2.toDouble should be (b.toDouble)
    bd2.toString should be (b.toString)

    val bbi: SignificantDigits[BigInt] = b
    bbi.digits should be (b.digits)
    bbi.toDouble should be (b.toDouble)
    bbi.toString should be (b.toString)

    val bbd: SignificantDigits[BigDecimal] = b
    bbd.digits should be (b.digits)
    bbd.toDouble should be (b.toDouble)
    bbd.toString should be (b.toString)


    val sb: SignificantDigits[Byte] = s
    sb.digits should be (s.digits)
    sb.toDouble should be (s.toDouble)
    sb.toString should be (s.toString)

    val si: SDI = s
    si.digits should be (s.digits)
    si.toDouble should be (s.toDouble)
    si.toString should be (s.toString)

    val sl: SDL = s
    sl.digits should be (s.digits)
    sl.toDouble should be (s.toDouble)
    sl.toString should be (s.toString)

    val sf: SDF = s
    sf.digits should be (s.digits)
    sf.toDouble should be (s.toDouble)
    sf.toString should be (s.toString)

    val sd: SDD = s
    sd.digits should be (s.digits)
    sd.toDouble should be (s.toDouble)
    sd.toString should be (s.toString)

    val sbi: SignificantDigits[BigInt] = s
    sbi.digits should be (s.digits)
    sbi.toDouble should be (s.toDouble)
    sbi.toString should be (s.toString)

    val sbd: SignificantDigits[BigDecimal] = s
    sbd.digits should be (s.digits)
    sbd.toDouble should be (s.toDouble)
    sbd.toString should be (s.toString)


    val ib: SignificantDigits[Byte] = i
    ib.digits should be (i.digits)
    ib.toDouble should be (i.toDouble)
    ib.toString should be (i.toString)

    val is: SignificantDigits[Short] = i
    is.digits should be (i.digits)
    is.toDouble should be (i.toDouble)
    is.toString should be (i.toString)

    val il: SDL = i
    il.digits should be (i.digits)
    il.toDouble should be (i.toDouble)
    il.toString should be (i.toString)

    val if2: SDF = i
    if2.digits should be (i.digits)
    if2.toDouble should be (i.toDouble)
    if2.toString should be (i.toString)

    val id: SDD = i
    id.digits should be (i.digits)
    id.toDouble should be (i.toDouble)
    id.toString should be (i.toString)

    val ibi: SignificantDigits[BigInt] = i
    ibi.digits should be (i.digits)
    ibi.toDouble should be (i.toDouble)
    ibi.toString should be (i.toString)

    val ibd: SignificantDigits[BigDecimal] = i
    ibd.digits should be (i.digits)
    ibd.toDouble should be (i.toDouble)
    ibd.toString should be (i.toString)


    val lb: SignificantDigits[Byte] = l
    lb.digits should be (l.digits)
    lb.toDouble should be (l.toDouble)
    lb.toString should be (l.toString)

    val ls: SignificantDigits[Short] = l
    ls.digits should be (l.digits)
    ls.toDouble should be (l.toDouble)
    ls.toString should be (l.toString)

    val li: SDI = l
    li.digits should be (l.digits)
    li.toDouble should be (l.toDouble)
    li.toString should be (l.toString)

    val lf: SDF = l
    lf.digits should be (l.digits)
    lf.toDouble should be (l.toDouble)
    lf.toString should be (l.toString)

    val ld: SDD = l
    ld.digits should be (l.digits)
    ld.toDouble should be (l.toDouble)
    ld.toString should be (l.toString)

    val lbi: SignificantDigits[BigInt] = l
    lbi.digits should be (l.digits)
    lbi.toDouble should be (l.toDouble)
    lbi.toString should be (l.toString)

    val lbd: SignificantDigits[BigDecimal] = l
    lbd.digits should be (l.digits)
    lbd.toDouble should be (l.toDouble)
    lbd.toString should be (l.toString)


    val fb: SignificantDigits[Byte] = f
    fb.digits should be (f.digits)
    fb.toDouble should be (f.toDouble)
    fb.toString should be (f.toString)

    val fs: SignificantDigits[Short] = f
    fs.digits should be (f.digits)
    fs.toDouble should be (f.toDouble)
    fs.toString should be (f.toString)

    val fi: SDI = f
    fi.digits should be (f.digits)
    fi.toDouble should be (f.toDouble)
    fi.toString should be (f.toString)

    val fl: SDL = f
    fl.digits should be (f.digits)
    fl.toDouble should be (f.toDouble)
    fl.toString should be (f.toString)

    val fd: SDD = f
    fd.digits should be (f.digits)
    fd.toDouble should be (f.toDouble)
    fd.toString should be (f.toString)

    val fbi: SignificantDigits[BigInt] = f
    fbi.digits should be (f.digits)
    fbi.toDouble should be (f.toDouble)
    fbi.toString should be (f.toString)

    val fbd: SignificantDigits[BigDecimal] = f
    fbd.digits should be (f.digits)
    fbd.toDouble should be (f.toDouble)
    fbd.toString should be (f.toString)


    val db: SignificantDigits[Byte] = d
    db.digits should be (d.digits)
    db.toDouble should be (d.toDouble)
    db.toString should be (d.toString)

    val ds: SignificantDigits[Short] = d
    ds.digits should be (d.digits)
    ds.toDouble should be (d.toDouble)
    ds.toString should be (d.toString)

    val di: SDI = d
    di.digits should be (d.digits)
    di.toDouble should be (d.toDouble)
    di.toString should be (d.toString)

    val dl: SDL = d
    dl.digits should be (d.digits)
    dl.toDouble should be (d.toDouble)
    dl.toString should be (d.toString)

    val df: SDF = d
    df.digits should be (d.digits)
    df.toDouble should be (d.toDouble)
    df.toString should be (d.toString)

    val dbi: SignificantDigits[BigInt] = d
    dbi.digits should be (d.digits)
    dbi.toDouble should be (d.toDouble)
    dbi.toString should be (d.toString)

    val dbd: SignificantDigits[BigDecimal] = d
    dbd.digits should be (d.digits)
    dbd.toDouble should be (d.toDouble)
    dbd.toString should be (d.toString)


    val bib: SignificantDigits[Byte] = bi
    bib.digits should be (bi.digits)
    bib.toDouble should be (bi.toDouble)
    bib.toString should be (bi.toString)

    val bis: SignificantDigits[Short] = bi
    bis.digits should be (bi.digits)
    bis.toDouble should be (bi.toDouble)
    bis.toString should be (bi.toString)

    val bii: SDI = bi
    bii.digits should be (bi.digits)
    bii.toDouble should be (bi.toDouble)
    bii.toString should be (bi.toString)

    val bil: SDL = bi
    bil.digits should be (bi.digits)
    bil.toDouble should be (bi.toDouble)
    bil.toString should be (bi.toString)

    val bif: SDF = bi
    bif.digits should be (bi.digits)
    bif.toDouble should be (bi.toDouble)
    bif.toString should be (bi.toString)

    val bid: SDD = bi
    bid.digits should be (bi.digits)
    bid.toDouble should be (bi.toDouble)
    bid.toString should be (bi.toString)

    val bibd: SignificantDigits[BigDecimal] = bi
    bibd.digits should be (bi.digits)
    bibd.toDouble should be (bi.toDouble)
    bibd.toString should be (bi.toString)


    val bdb: SignificantDigits[Byte] = bd
    bdb.digits should be (bd.digits)
    bdb.toDouble should be (bd.toDouble)
    bdb.toString should be (bd.toString)

    val bds: SignificantDigits[Short] = bd
    bds.digits should be (bd.digits)
    bds.toDouble should be (bd.toDouble)
    bds.toString should be (bd.toString)

    val bdi: SDI = bd
    bdi.digits should be (bd.digits)
    bdi.toDouble should be (bd.toDouble)
    bdi.toString should be (bd.toString)

    val bdl: SDL = bd
    bdl.digits should be (bd.digits)
    bdl.toDouble should be (bd.toDouble)
    bdl.toString should be (bd.toString)

    val bdf: SDF = bd
    bdf.digits should be (bd.digits)
    bdf.toDouble should be (bd.toDouble)
    bdf.toString should be (bd.toString)

    val bdd: SDD = bd
    bdd.digits should be (bd.digits)
    bdd.toDouble should be (bd.toDouble)
    bdd.toString should be (bd.toString)

    val bdbi: SignificantDigits[BigInt] = bd
    bdbi.digits should be (bd.digits)
    bdbi.toDouble should be (bd.toDouble)
    bdbi.toString should be (bd.toString)

    SignificantDigits.setDefaultSDContext()
  }
}
