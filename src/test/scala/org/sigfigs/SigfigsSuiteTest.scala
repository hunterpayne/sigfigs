
package org.sigfigs

import org.scalatest._
import org.scalatest.events._

import org.junit.BeforeClass
import org.junit.runner.RunWith
import org.junit.runners.{ Suite => JUnit4Suite }

import com.carrotgarden.sjs.junit.{ 
  LinkerImpl, SuiteSetupImpl, TestInit, ScalaJS_ScalatestSuite }
import SigfigsSuiteTest._

object Linker extends LinkerImpl
class SuiteSetup extends SuiteSetupImpl(Linker)
object SuiteSetup extends SuiteSetup

@RunWith(classOf[JUnit4Suite])
@JUnit4Suite.SuiteClasses(Array(
  classOf[TopSuite], classOf[InformationSuite], classOf[TimeSuite]))
class SigfigsSuiteTest

object SigfigsSuiteTest {

  @BeforeClass
  def setup: Unit = TestInit.setup(SuiteSetup)

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[SignificantDigitsSpec], classOf[SignificantDigitsCustomSpec],
    classOf[DimensionlessSpec]
  ))
  class TopSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[information.DataRateSpec], classOf[information.InformationSpec]))
  class InformationSuite

  @RunWith(classOf[ScalaJS_ScalatestSuite])
  @JUnit4Suite.SuiteClasses(Array(
    classOf[time.TimeSpec], classOf[time.TimeSquaredSpec], 
    classOf[time.FrequencySpec]
  ))
  class TimeSuite
}

