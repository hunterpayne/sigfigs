/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.sigfigs
package time

import org.scalatest.{ FlatSpec, Matchers }

import terra._
import terra.energy.{ WattHours, Watts, WattsPerHour }
import terra.information._
import terra.mass.Kilograms
import terra.motion._
import terra.photo.{ LumenSeconds, Lumens, Lux, LuxSeconds }
import terra.space.{ CubicMeters, Meters, Radians }
import terra.time._
import terra.electro.{ Amperes, Coulombs, Volts, Webers }

import org.terra.{ QuantityParseException, BinarySystem, MetricSystem }

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 */
class FrequencySpec extends FlatSpec with Matchers {

  behavior of "Frequency and its Units of Measure"

  it should "create values using UOM factories" in {
    Hertz(1).toHertz should be(SDD(1))
    Kilohertz(1).toKilohertz should be(SDD(1))
    Megahertz(1).toMegahertz should be(SDD(1))
    Gigahertz(1).toGigahertz should be(SDD(1))
    Terahertz(1).toTerahertz should be(SDD(1))
    RevolutionsPerMinute(1).toRevolutionsPerMinute should be(SDD(1))
  }

  it should "create values from properly formatted Strings" in {
    Frequency("10.22 Hz").get should be(Hertz(10.22))
    Frequency("10.22 kHz").get should be(Kilohertz(10.22))
    Frequency("10.22 MHz").get should be(Megahertz(10.22))
    Frequency("10.22 GHz").get should be(Gigahertz(10.22))
    Frequency("10.22 THz").get should be(Terahertz(10.22))
    Frequency("10.22 rpm").get should be(RevolutionsPerMinute(10.22))
    Frequency("10.45 zz").failed.get should be(QuantityParseException("Unable to parse Frequency", "10.45 zz"))
    Frequency("zz Hz").failed.get should be(QuantityParseException("Unable to parse Frequency", "zz Hz"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = Hertz(1)
    x.toHertz should be(SDD(1))
    x.toKilohertz should be(SDD(1 / MetricSystem.Kilo))
    x.toMegahertz should be(SDD(1 / MetricSystem.Mega))
    x.toTerahertz should be(SDD(1 / MetricSystem.Tera))
    x.toRevolutionsPerMinute should be(SDD(60))
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    Hertz(1).toString(Hertz) should be("1.0 Hz")
    Kilohertz(1).toString(Kilohertz) should be("1.0 kHz")
    Megahertz(1).toString(Megahertz) should be("1.0 MHz")
    Gigahertz(1).toString(Gigahertz) should be("1.0 GHz")
    Terahertz(1).toString(Terahertz) should be("1.0 THz")
    RevolutionsPerMinute(1).toString(RevolutionsPerMinute) should be("1.0 rpm")
  }

  it should "return Count when multiplied by Time" in {
    Hertz(1) * Seconds(1) should be(Each(1))
  }

  it should "return the TimeDerivative when multiplied by a TimeIntegral" in {
    Hertz(100) * FeetPerSecondSquared(100) should be(FeetPerSecondCubed(10000))
    Hertz(100) * Radians(100) should be(RadiansPerSecond(10000))
    Hertz(100) * Each(100) should be(Hertz(10000))
    Hertz(100) * Coulombs(100) should be(Amperes(10000))
    Hertz(100) * WattHours(100) should be(Watts(SDD(36000000, 1)))
    Hertz(100) * Newtons(100) should be(NewtonsPerSecond(10000))
    Hertz(100) * Bytes(100) should be(BytesPerSecond(10000))
    Hertz(100) * Meters(100) should be(MetersPerSecond(10000))
    Hertz(100) * LumenSeconds(100) should be(Lumens(10000))
    Hertz(100) * LuxSeconds(100) should be(Lux(10000))
    Hertz(100) * Webers(100) should be(Volts(10000))
    Hertz(100) * Kilograms(100) should be(KilogramsPerSecond(10000))
    Hertz(100) * NewtonSeconds(100) should be(Newtons(10000))
    Hertz(100) * Watts(100) should be(WattsPerHour(SDD(36000000, 1)))
    Hertz(100) * Pascals(100) should be(PascalsPerSecond(10000))
    Hertz(100) * MetersPerSecond(100) should be(MetersPerSecondSquared(10000))
    Hertz(100) * CubicMeters(100) should be(CubicMetersPerSecond(10000))
  }

  behavior of "FrequencyConversions"

  it should "provide implicit conversion from Double" in {
    import FrequencyConversions._

    val d = SDD(10d)
    d.hertz should be(Hertz(d))
    d.kilohertz should be(Kilohertz(d))
    d.megahertz should be(Megahertz(d))
    d.gigahertz should be(Gigahertz(d))
    d.terahertz should be(Terahertz(d))
    d.rpm should be(RevolutionsPerMinute(d))
  }
}
