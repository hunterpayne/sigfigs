
package org.sigfigs

import org.scalatest._
import org.scalatest.events._

import org.junit.{ BeforeClass, Test }
import org.junit.runner.{ RunWith, Result, Runner, Description }
import org.junit.runners.{ Suite => JUnit4Suite, ParentRunner }
import org.junit.runners.model.{ RunnerBuilder, Statement }
import org.junit.runner.notification.{ RunNotifier, Failure }

import com.carrotgarden.sjs.junit.{ 
  LinkerImpl, SuiteSetupImpl, TestInit, ScalaJS_Suite }
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

class ScalaJS_ScalatestSuite(klaz: Class[_], builder: RunnerBuilder) 
    extends JUnit4Suite(klaz, builder) {

  import collection.JavaConversions._

  protected override def getChildren(): java.util.List[Runner] =
    klaz.getAnnotationsByType(classOf[JUnit4Suite.SuiteClasses]).flatMap {
      _.value.map { clz => {
        val suite = clz.newInstance().asInstanceOf[Suite]
        new ScalatestRunner(clz.asInstanceOf[Class[Suite]], suite) } } }.toSeq

  class ScalatestRunner(clazz: Class[Suite], scalatestSuite: Suite)
      extends ScalaJS_Suite(clazz, Seq(this)) {

    lazy val desc =
      Description.createTestDescription(clazz, scalatestSuite.suiteName)

    override def run(notifier: RunNotifier): Unit = {
      val rep = new Reporter {
        def apply(event: Event): Unit = event match {
          case ts: TestStarting => notifier.fireTestStarted(
            Description.createTestDescription(clazz, ts.testText))
          case ts: TestSucceeded => notifier.fireTestFinished(
            Description.createTestDescription(clazz, ts.testText))
          case ti: TestIgnored => notifier.fireTestIgnored(
            Description.createTestDescription(clazz, ti.testText))
          case tf: TestFailed =>
            notifier.fireTestFailure(new Failure(
              Description.createTestDescription(clazz, tf.testText),
              tf.throwable.getOrElse(new Exception(tf.message))))
          case _ => // do nothing
        }
      }
      scalatestSuite.run(None, Args(reporter = rep))
    }

    override def getDescription(): Description = desc
    override def testCount(): Int = scalatestSuite.testNames.size

    override def toString(): String = s"ScalatestRunner[${clazz.getName()}]"
  }
}
