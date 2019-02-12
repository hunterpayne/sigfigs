
package org.sigfigs

import scala.reflect.{ ClassTag, classTag }
import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

import org.terra._
import org.terra.electro._
import org.terra.energy._
import org.terra.mass._
import org.terra.motion._
import org.terra.information._
import org.terra.space._
import org.terra.photo._
import org.terra.radio._
import org.terra.thermal._
import org.terra.time._
import org.terra.market._

/**
  * The types used by the SigFigsTerraOps version of Terra.  Specifies use of
  * <ul>
  * <li>SDD for floating point operations,</li>
  * <li>SDL for whole number operations (Information),</li>
  * <li>SignificantDigits[BigDecimal] for currency operation, and</li>
  * <li>SDD for time operations</li></ul>
  */
class SigFigsTuple extends TypeContext {
  type T = SDD
  type TL = SDL
  type TC = SignificantDigits[BigDecimal]
  type TT = SDD
}

object SigFigsConverters extends Converters[SigFigsTuple] {

  implicit val longIdConverter = new HasConverter[SDL, SDL] {
    override def conv(in: SDL): SDL = in
  }

  implicit val tttConverter = new HasConverter[T, TT] {
    override def conv(in: T): TT = in
  }
  implicit val ttlConverter = new HasConverter[T, TL] {
    override def conv(in: T): TL = round[Double, Long](in)
  }
  implicit val ttcConverter = new HasConverter[T, TC] {
    override def conv(in: T): TC = converter[Double, BigDecimal](in)
  }
  implicit val tltConverter = new HasConverter[TL, T] {
    override def conv(in: TL): T = converter[Long, Double](in)
  }
  implicit val tlttConverter = new HasConverter[TL, TT] {
    override def conv(in: TL): TT = converter[Long, Double](in)
  }
  implicit val tltcConverter = new HasConverter[TL, TC] {
    override def conv(in: TL): TC = converter[Long, BigDecimal](in)
  }
  implicit val tttConverter2 = new HasConverter[TT, T] {
    override def conv(in: TT): T = in
  }
  implicit val tttlConverter = new HasConverter[TT, TL] {
    override def conv(in: TT): TL = round[Double, Long](in)
  }
  implicit val tttcConverter = new HasConverter[TT, TC] {
    override def conv(in: TT): TC = converter[Double, BigDecimal](in)
  }
  implicit val tctConverter = new HasConverter[TC, T] {
    override def conv(in: TC): T = converter[BigDecimal, Double](in)
  }
  implicit val tctlConverter = new HasConverter[TC, TL] {
    override def conv(in: TC): TL = converter[BigDecimal, Long](in)
  }
  implicit val tcttConverter = new HasConverter[TC, TT] {
    override def conv(in: TC): TT = converter[BigDecimal, Double](in)
  }

  implicit val dtConverter = new HasConverter[Double, T] {
    override def conv(in: Double): T = SDD(in)
  }
  implicit val dtlConverter = new HasConverter[Double, TL] {
    override def conv(in: Double): TL = round[Double, Long](SDD(in))
  }
  implicit val dtcConverter = new HasConverter[Double, TC] {
    override def conv(in: Double): TC =
      SignificantDigits[BigDecimal](BigDecimal(in))
  }
  implicit val dttConverter = new HasConverter[Double, TT] {
    override def conv(in: Double): TT = SDD(in)
  }

  implicit val longConverter = new HasConverter[SDL, SDD] {
    override def conv(in: SDL): SDD = converter[Long, Double](in)
  }

  implicit val doubleConverter = new HasConverter[SDD, SDL] {
    override def conv(in: SDD): SDL = round[Double, Long](in)
  }
  implicit val ltConverter = new HasConverter[Long, T] {
    override def conv(in: Long): T = converter[Long, Double](SDL(in))
  }
  implicit val ltlConverter = new HasConverter[Long, TL] {
    override def conv(in: Long): TL = SDL(in)
  }
  implicit val lttConverter = new HasConverter[Long, TT] {
    override def conv(in: Long): TT = converter[Long, Double](SDL(in))
  }
  implicit val ltcConverter = new HasConverter[Long, TC] {
    override def conv(in: Long): TC =
      SignificantDigits[BigDecimal](BigDecimal(in))
  }
  implicit val bdtcConverter = new HasConverter[BigDecimal, TC] {
    override def conv(in: BigDecimal): TC = SignificantDigits[BigDecimal](in)
  }

  implicit val ensureT = new HasEnsureType[T] {
    override def ensureType(in: Any): T = in match {
      case b: Byte => SDD(b.toDouble)
      case s: Short => SDD(s.toDouble)
      case i: Int => SDD(i.toDouble)
      case l: Long => SDD(l.toDouble)
      case f: Float => SDD(f.toDouble)
      case d: Double => SDD(d)
      case bd: BigDecimal => SDD(bd.toDouble)
      case bi: BigInt => SDD(bi.toDouble)
      case sd: SignificantDigits[_] => sd.v match {
        case b: Byte => 
          converter[Byte, Double](sd.asInstanceOf[SignificantDigits[Byte]])
        case s: Short => 
          converter[Short, Double](sd.asInstanceOf[SignificantDigits[Short]])
        case i: Int => 
          converter[Int, Double](sd.asInstanceOf[SignificantDigits[Int]])
        case l: Long => 
          converter[Long, Double](sd.asInstanceOf[SignificantDigits[Long]])
        case f: Float => 
          converter[Float, Double](sd.asInstanceOf[SignificantDigits[Float]])
        case d: Double => sd.asInstanceOf[SignificantDigits[Double]]
        case bd: BigDecimal => 
          converter[BigDecimal, Double](
            sd.asInstanceOf[SignificantDigits[BigDecimal]])
        case bi: BigInt => 
          converter[BigInt, Double](sd.asInstanceOf[SignificantDigits[BigInt]])
        case o => throw new Exception(
          "unknown type " + o + " type=" + o.getClass.getName)
      }
      case o => throw new Exception(
        "unknown type " + o + " type=" + o.getClass.getName)
    }
  }
  implicit val ensureTL = new HasEnsureType[TL] {
    override def ensureType(in: Any): TL = in match {
      case b: Byte => b.toLong
      case s: Short => s.toLong
      case i: Int => i.toLong
      case l: Long => l
      case f: Float => scala.math.round(f).toLong
      case d: Double => scala.math.round(d).toLong
      case bd: BigDecimal => bd.toLong
      case bi: BigInt => bi.longValue
      case sd: SignificantDigits[_] => sd.v match {
        case b: Byte => 
          converter[Byte, Long](sd.asInstanceOf[SignificantDigits[Byte]])
        case s: Short => 
          converter[Short, Long](sd.asInstanceOf[SignificantDigits[Short]])
        case i: Int => 
          converter[Int, Long](sd.asInstanceOf[SignificantDigits[Int]])
        case l: Long => sd.asInstanceOf[SignificantDigits[Long]]
        case f: Float => 
          converter[Float, Long](sd.asInstanceOf[SignificantDigits[Float]])
        case d: Double =>
          converter[Double, Long](sd.asInstanceOf[SignificantDigits[Double]])
        case bd: BigDecimal => 
          converter[BigDecimal, Long](
            sd.asInstanceOf[SignificantDigits[BigDecimal]])
        case bi: BigInt => 
          converter[BigInt, Long](sd.asInstanceOf[SignificantDigits[BigInt]])
        case o => throw new Exception(
          "unknown type " + o + " type=" + o.getClass.getName)
      }
      case o => throw new Exception(
        "unknown type " + o + " type=" + o.getClass.getName)
    }
  }
  implicit val ensureTT = ensureT
  implicit val ensureTC = new HasEnsureType[TC] {
    override def ensureType(in: Any): TC = in match {
      case b: Byte => BigDecimal(b.toDouble)
      case s: Short => BigDecimal(s.toDouble)
      case i: Int => BigDecimal(i.toDouble)
      case l: Long => BigDecimal(l.toDouble)
      case f: Float => BigDecimal(f.toDouble)
      case d: Double => BigDecimal(d)
      case bd: BigDecimal => bd
      case bi: BigInt => BigDecimal(bi.doubleValue)
      case sd: SignificantDigits[_] => sd.v match {
        case b: Byte => 
          converter[Byte, BigDecimal](sd.asInstanceOf[SignificantDigits[Byte]])
        case s: Short => 
          converter[Short, BigDecimal](
            sd.asInstanceOf[SignificantDigits[Short]])
        case i: Int => 
          converter[Int, BigDecimal](sd.asInstanceOf[SignificantDigits[Int]])
        case l: Long => 
          converter[Long, BigDecimal](
            sd.asInstanceOf[SignificantDigits[BigDecimal]])
        case f: Float => 
          converter[Float, BigDecimal](
            sd.asInstanceOf[SignificantDigits[Float]])
        case d: Double => sd.asInstanceOf[SignificantDigits[BigDecimal]]
        case bd: BigDecimal => 
          converter[BigDecimal, BigDecimal](
            sd.asInstanceOf[SignificantDigits[BigDecimal]])
        case bi: BigInt => 
          converter[BigInt, BigDecimal](
            sd.asInstanceOf[SignificantDigits[BigInt]])
        case o => throw new Exception(
          "unknown type " + o + " type=" + o.getClass.getName)
      }
      case o => throw new Exception(
        "unknown type " + o + " type=" + o.getClass.getName)
    }
  }
}

package object terra extends TypeScope[SigFigsTuple] {

  trait AbstractSigFigsTerraOps[C <: TypeContext] extends TerraOps[C]
      with DimensionlessOps[C]
      with InformationOps[C]
      with DataRateOps[C]
      with TimeOps[C]
      with TimeSquaredOps[C]
      with FrequencyOps[C]
      with AreaElectricChargeDensityOps[C]
      with CapacitanceOps[C]
      with ConductivityOps[C]
      with ElectricChargeOps[C]
      with ElectricChargeDensityOps[C]
      with ElectricChargeMassRatioOps[C]
      with ElectricCurrentOps[C]
      with ElectricCurrentDensityOps[C]
      with ElectricFieldStrengthOps[C]
      with ElectricPotentialOps[C]
      with ElectricalConductanceOps[C]
      with ElectricalResistanceOps[C]
      with InductanceOps[C]
      with LinearElectricChargeDensityOps[C]
      with MagneticFieldStrengthOps[C]
      with MagneticFluxOps[C]
      with MagneticFluxDensityOps[C]
      with PermeabilityOps[C]
      with PermittivityOps[C]
      with ResistivityOps[C]
      with EnergyOps[C]
      with EnergyDensityOps[C]
      with MolarEnergyOps[C]
      with PowerOps[C]
      with PowerDensityOps[C]
      with PowerRampOps[C]
      with SpecificEnergyOps[C]
      with AreaDensityOps[C]
      with ChemicalAmountOps[C]
      with DensityOps[C]
      with MassOps[C]
      with MomentOfInertiaOps[C]
      with MolarMassOps[C]
      with ConcentrationOps[C]
      with CatalyticActivityOps[C]
      with AccelerationOps[C]
      with AngularAccelerationOps[C]
      with AngularVelocityOps[C]
      with ForceOps[C]
      with JerkOps[C]
      with MassFlowOps[C]
      with MomentumOps[C]
      with PressureOps[C]
      with PressureChangeOps[C]
      with TorqueOps[C]
      with VelocityOps[C]
      with VolumeFlowOps[C]
      with YankOps[C]
      with IlluminanceOps[C]
      with LuminanceOps[C]
      with LuminousEnergyOps[C]
      with LuminousExposureOps[C]
      with LuminousFluxOps[C]
      with LuminousIntensityOps[C]
      with ActivityOps[C]
      with AreaTimeOps[C]
      with DoseOps[C]
      with IrradianceOps[C]
      with ParticleFluxOps[C]
      with RadianceOps[C]
      with RadiantIntensityOps[C]
      with SpectralIntensityOps[C]
      with SpectralIrradianceOps[C]
      with SpectralPowerOps[C]
      with AngleOps[C]
      with AreaOps[C]
      with LengthOps[C]
      with SolidAngleOps[C]
      with VolumeOps[C]
      with TemperatureOps[C]
      with ThermalCapacityOps[C]
      with MoneyOps[C]
      with EmployeeOps[C]
      with LaborOps[C] {

    val SDDTag = classTag[SDD]
    val SDLTag = classTag[SDL]
    val SDBigDecimalTag = classTag[SignificantDigits[BigDecimal]]
    val IntTag = classTag[Int]

    override val getClassTagT: ClassTag[T] = SDDTag.asInstanceOf[ClassTag[T]]
    override val getClassTagTL: ClassTag[TL] = SDLTag.asInstanceOf[ClassTag[TL]]
    override val getClassTagTC: ClassTag[TC] = 
      SDBigDecimalTag.asInstanceOf[ClassTag[TC]]
    override val getClassTagTT: ClassTag[TT] = SDDTag.asInstanceOf[ClassTag[TT]]

    def nt[T1](implicit tag: ClassTag[T1]): Numeric[T1] =
      tag match {
        case `getClassTagT` => num.asInstanceOf[Numeric[T1]]
        case `getClassTagTL` => numL.asInstanceOf[Numeric[T1]]
        case `getClassTagTC` => numC.asInstanceOf[Numeric[T1]]
        case `getClassTagTT` => numT.asInstanceOf[Numeric[T1]]
        case SDDTag => num.asInstanceOf[Numeric[T1]]
        case SDLTag => numL.asInstanceOf[Numeric[T1]]
        case IntTag => Numeric.IntIsIntegral.asInstanceOf[Numeric[T1]]
        case clz if (clz != null) => {
          println("got clz " + clz + " of type " + clz.runtimeClass)
          //assert(nl.isInstanceOf[Numeric[T1]])
          num.asInstanceOf[Numeric[T1]]
        }
        case _ => {
          //(new Exception("null class tag guessing")).printStackTrace()
          println("null class tag guessing")
          num.asInstanceOf[Numeric[T1]]
        }
      }

    val dimensionlessOps: DimensionlessOps[C] = this

    val informationOps: InformationOps[C] = this
    val dataRateOps: DataRateOps[C] = this

    val timeOps: TimeOps[C] = this
    val timeSquaredOps: TimeSquaredOps[C] = this
    val frequencyOps: FrequencyOps[C] = this

    val areaElectricChargeDensityOps: AreaElectricChargeDensityOps[C] = this
    val capacitanceOps: CapacitanceOps[C] = this
    val conductivityOps: ConductivityOps[C] = this
    val electricChargeOps: ElectricChargeOps[C] = this
    val electricChargeDensityOps: ElectricChargeDensityOps[C] = this
    val electricChargeMassRatioOps: ElectricChargeMassRatioOps[C] = this
    val electricCurrentOps: ElectricCurrentOps[C] = this
    val electricCurrentDensityOps: ElectricCurrentDensityOps[C] = this
    val electricFieldStrengthOps: ElectricFieldStrengthOps[C] = this
    val electricPotentialOps: ElectricPotentialOps[C] = this
    val electricalConductanceOps: ElectricalConductanceOps[C] = this
    val electricalResistanceOps: ElectricalResistanceOps[C] = this
    val inductanceOps: InductanceOps[C] = this
    val linearElectricChargeDensityOps: LinearElectricChargeDensityOps[C] = this
    val magneticFieldStrengthOps: MagneticFieldStrengthOps[C] = this
    val magneticFluxOps: MagneticFluxOps[C] = this
    val magneticFluxDensityOps: MagneticFluxDensityOps[C] = this
    val permeabilityOps: PermeabilityOps[C] = this
    val permittivityOps: PermittivityOps[C] = this
    val resistivityOps: ResistivityOps[C] = this

    val energyOps: EnergyOps[C] = this
    val energyDensityOps: EnergyDensityOps[C] = this
    val molarEnergyOps: MolarEnergyOps[C] = this
    val powerOps: PowerOps[C] = this
    val powerDensityOps: PowerDensityOps[C] = this
    val powerRampOps: PowerRampOps[C] = this
    val specificEnergyOps: SpecificEnergyOps[C] = this

    val areaDensityOps: AreaDensityOps[C] = this
    val chemicalAmountOps: ChemicalAmountOps[C] = this
    val densityOps: DensityOps[C] = this
    val massOps: MassOps[C] = this
    val momentOfInertiaOps: MomentOfInertiaOps[C] = this
    val molarMassOps: MolarMassOps[C] = this
    val concentrationOps: ConcentrationOps[C] = this
    val catalyticActivityOps: CatalyticActivityOps[C] = this

    val accelerationOps: AccelerationOps[C] = this
    val angularAccelerationOps: AngularAccelerationOps[C] = this
    val angularVelocityOps: AngularVelocityOps[C] = this
    val forceOps: ForceOps[C] = this
    val jerkOps: JerkOps[C] = this
    val massFlowOps: MassFlowOps[C] = this
    val momentumOps: MomentumOps[C] = this
    val pressureOps: PressureOps[C] = this
    val pressureChangeOps: PressureChangeOps[C] = this
    val torqueOps: TorqueOps[C] = this
    val velocityOps: VelocityOps[C] = this
    val volumeFlowOps: VolumeFlowOps[C] = this
    val yankOps: YankOps[C] = this

    val illuminanceOps: IlluminanceOps[C] = this
    val luminanceOps: LuminanceOps[C] = this
    val luminousEnergyOps: LuminousEnergyOps[C] = this
    val luminousExposureOps: LuminousExposureOps[C] = this
    val luminousFluxOps: LuminousFluxOps[C] = this
    val luminousIntensityOps: LuminousIntensityOps[C] = this

    val activityOps: ActivityOps[C] = this
    val areaTimeOps: AreaTimeOps[C] = this
    val doseOps: DoseOps[C] = this
    val irradianceOps: IrradianceOps[C] = this
    val particleFluxOps: ParticleFluxOps[C] = this
    val radianceOps: RadianceOps[C] = this
    val radiantIntensityOps: RadiantIntensityOps[C] = this
    val spectralIntensityOps: SpectralIntensityOps[C] = this
    val spectralIrradianceOps: SpectralIrradianceOps[C] = this
    val spectralPowerOps: SpectralPowerOps[C] = this

    val angleOps: AngleOps[C] = this
    val areaOps: AreaOps[C] = this
    val lengthOps: LengthOps[C] = this
    val solidAngleOps: SolidAngleOps[C] = this
    val volumeOps: VolumeOps[C] = this

    val temperatureOps: TemperatureOps[C] = this
    val thermalCapacityOps: ThermalCapacityOps[C] = this

    val moneyOps: MoneyOps[C] = this
    val employeeOps: EmployeeOps[C] = this
    val laborOps: LaborOps[C] = this
  }

  /**
    * The TerraOps object for the org.sigfig.terra tree.  Provides conversion 
    * and math functions for the SigFigsTuple types: SDD, SDL, 
    * SigificantDigits[BigDecimal], and SDD
    */
  implicit object SigFigsTerraOps 
      extends AbstractSigFigsTerraOps[SigFigsTuple] {

    implicit val num: Numeric[T] = DoubleSigOps
    implicit val numL: Numeric[TL] = LongSigOps
    implicit val numC: Numeric[TC] = BigDecimalSigOps
    implicit val numT: Numeric[TT] = DoubleSigOps

    // already done in superclass, if we make more TerraOps, we need to override
    // these
    //override val getClassTagT: ClassTag[C#T] = SDDTag
    //override val getClassTagTL: ClassTag[C#TL] = SDLTag
    //override val getClassTagTC: ClassTag[C#TC] = SDBigDecimalTag
    //override val getClassTagTT: ClassTag[C#TT] = SDDTag

    val converters = SigFigsConverters

    def makeEnsureType[T1](test: Any): HasEnsureType[T1] = test match {
      case d: Double => converters.ensureT.asInstanceOf[HasEnsureType[T1]]
      case f: Float => converters.ensureT.asInstanceOf[HasEnsureType[T1]]
      case l: Long => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
      case i: Int => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
      case s: Short => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
      case b: Byte => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
      case bd: BigDecimal => converters.ensureTC.asInstanceOf[HasEnsureType[T1]]
      case sd: SignificantDigits[_] => sd.v match {
        case b: Byte => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
        case s: Short => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
        case i: Int => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
        case l: Long => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
        case f: Float => converters.ensureT.asInstanceOf[HasEnsureType[T1]]
        case d: Double => converters.ensureT.asInstanceOf[HasEnsureType[T1]]
        case bd: BigDecimal =>
          converters.ensureTC.asInstanceOf[HasEnsureType[T1]]
      }
    }

    def convTime(t: Any)(implicit ops: TerraOps[SigFigsTuple]): TT = t match {
      case d: Double => {
        implicit val hasConv = converters.dttConverter
        gconvTotal[Double, TT](d)
      }
      case l: Long => {
        // seems to be necessary to make the compiler happy
        implicit val hasConv = converters.lttConverter
        gconvTotal[Long, TT](l)
      }
      case sd: SignificantDigits[_] => sd.v match {
        case d: Double => sd.asInstanceOf[TT]
        case l: Long => converter[Long, Double](sd.asInstanceOf[SDL])
      }
      case _ => assert(false); numT.zero
    }

    def convCurrency(t: Any)(implicit ops: TerraOps[SigFigsTuple]): TC = 
      t match {
        case d: Double => {
          implicit val hasConv = converters.dtcConverter
          gconvTotal[Double, TC](d)
        }
        case l: Long => {
          implicit val hasConv = converters.ltcConverter
          gconvTotal[Long, TC](l)
        }
        case bd: BigDecimal => {
          implicit val hasConv = converters.bdtcConverter
          gconvTotal[BigDecimal, TC](bd)
        }
        case sd: SignificantDigits[_] => sd.v match {
          case d: Double => converter[Double, BigDecimal](sd.asInstanceOf[SDD])
          case l: Long => converter[Long, BigDecimal](sd.asInstanceOf[SDL])
          case bd: BigDecimal => sd.asInstanceOf[TC]
        }
        case _ => assert(false); numC.zero
      }

    override def conv(d: T): TL = round[Double, Long](d)
    // making the type conversion explicit for clairity
    override def rconv(l: TL): T = converter[Long, Double](l)
    override def convL(d: TT): TL = round[Double, Long](d)
    // making the type conversion explicit for clairity
    override def rconvL(l: TL): TT = converter[Long, Double](l) 
    override def convT(d: T): TT = d
    override def rconvT(d: TT): T = d

    def div[T1](dividend: T1, divisor: T1)(
      implicit e: HasEnsureType[T1], tag: ClassTag[T1]): T1 = dividend match {
      case sd: SignificantDigits[_] =>
        sd.v match {
          case d: Double =>
            ensureType[T1](sd.asInstanceOf[SDD].div(divisor.asInstanceOf[SDD]))
          case l: Long => 
            ensureType[T1](sd.asInstanceOf[SDL].div(divisor.asInstanceOf[SDL]))
          case bd: BigDecimal => 
            ensureType[T1](sd.asInstanceOf[SignificantDigits[BigDecimal]].div(
              divisor.asInstanceOf[SignificantDigits[BigDecimal]]))
          case _ => assert(false); nt[T1].zero
        }
        case _ => assert(false); nt[T1].zero
    }

    def mod[T1](dividend: T1, divisor: T1)(
      implicit e: HasEnsureType[T1], tag: ClassTag[T1]): T1 = dividend match {
      case sd: SignificantDigits[_] => sd.v match {
        case d: Double =>
          ensureType[T1](sd.asInstanceOf[SDD].rem(divisor.asInstanceOf[SDD]))
        case l: Long =>
          ensureType[T1](sd.asInstanceOf[SDL].rem(divisor.asInstanceOf[SDL]))
        case bd: BigDecimal =>
          ensureType[T1](sd.asInstanceOf[SignificantDigits[BigDecimal]].rem(
            divisor.asInstanceOf[SignificantDigits[BigDecimal]]))
        case _ => assert(false); nt[T1].zero
      }
      case _ => assert(false); nt[T1].zero
    }

    def floorT[T1](t: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](floor(ensureType[T](t)))
    }

    def ceilT[T1](t: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](ceil(ensureType[T](t)))
    }

    def rintT[T1](t: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](rint(ensureType[T](t)))
    }

    def roundT[T2](
      t: T2, scale: Int, mode: RoundingMode = RoundingMode.HALF_EVEN)(
      implicit e: HasEnsureType[T2]): T2 = {
      val sd: SignificantDigits[_] = t.asInstanceOf[SignificantDigits[_]]
      sd.v match {
        case d: Double => 
          ensureType[T2](round[Double, Long](sd.asInstanceOf[SDD]))
        case f: Float => ensureType[T2](round[Float, Int](sd.asInstanceOf[SDF]))
      }
    }

    def sqrtT[T1](t: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](sqrt(ensureType[T](t)))
    }
    def cbrtT[T1](t: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](cbrt(ensureType[T](t)))
    }

    def sinT[T1](t: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](sin(ensureType[T](t)))
    }
    def cosT[T1](t: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](cos(ensureType[T](t)))
    }
    def tanT[T1](t: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](tan(ensureType[T](t)))
    }
    def asinT[T1](t: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](asin(ensureType[T](t)))
    }
    def acosT[T1](t: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](acos(ensureType[T](t)))
    }
    def atanT[T1](t: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](atan(ensureType[T](t)))
    }
    def atan2T[T1](y: T1, x: T1)(implicit e: HasEnsureType[T1]): T1 = {
      implicit val e1: HasEnsureType[T] = converters.ensureT
      ensureType[T1](atan2(ensureType[T](y), ensureType[T](x)))
    }
  }

  val ops = SigFigsTerraOps

  trait SymbolMixin {
    implicit val ops: TerraOps[SigFigsTuple] = SigFigsTerraOps
  }

  object information extends SymbolMixin 
      with org.terra.information.InformationSymbols[SigFigsTuple]
  object time extends SymbolMixin 
      with org.terra.time.TimeSymbols[SigFigsTuple]
  object electro extends SymbolMixin 
      with org.terra.electro.ElectroSymbols[SigFigsTuple]
  object energy extends SymbolMixin 
      with org.terra.energy.EnergySymbols[SigFigsTuple]
  object mass extends SymbolMixin 
      with org.terra.mass.MassSymbols[SigFigsTuple]
  object motion extends SymbolMixin 
      with org.terra.motion.MotionSymbols[SigFigsTuple]
  object photo extends SymbolMixin 
      with org.terra.photo.PhotoSymbols[SigFigsTuple]
  object radio extends SymbolMixin 
      with org.terra.radio.RadioSymbols[SigFigsTuple]
  object space extends SymbolMixin 
      with org.terra.space.SpaceSymbols[SigFigsTuple]
  object thermal extends SymbolMixin
      with org.terra.thermal.ThermalSymbols[SigFigsTuple]
  object market extends SymbolMixin
      with org.terra.market.MarketSymbols[SigFigsTuple]

  implicit class QuantityDoubleT(d: Double)
      extends QuantityHelper[SigFigsTuple#T, SDD](SDD(d), DoubleSigOps) {

    def *[A <: Quantity[A, SigFigsTuple#T, SigFigsTuple]](that: A): A =
      times(that)
    def *[A <: Quantity[A, SigFigsTuple#T, SigFigsTuple]](
      that: SVector[A, SigFigsTuple#T, SigFigsTuple]): 
        SVector[A, SigFigsTuple#T, SigFigsTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityDoubleTL(d: Double) 
      extends QuantityHelper[SigFigsTuple#TL, SDD](SDD(d), DoubleSigOps) {
    def *[A <: Quantity[A, SigFigsTuple#TL, SigFigsTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, SigFigsTuple#TL, SigFigsTuple]](
      that: SVector[A, SigFigsTuple#TL, SigFigsTuple]): 
        SVector[A, SigFigsTuple#TL, SigFigsTuple] =
      times(that)
  }

  implicit class QuantityLongT(l: Long)
      extends QuantityHelper[SigFigsTuple#T, SDL](SDL(l), LongSigOps) {

    def *[A <: Quantity[A, SigFigsTuple#T, SigFigsTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, SigFigsTuple#T, SigFigsTuple]](
      that: SVector[A, SigFigsTuple#T, SigFigsTuple]): 
        SVector[A, SigFigsTuple#T, SigFigsTuple] = 
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityLongTL(l: Long) 
    extends QuantityHelper[SigFigsTuple#TL, SDL](SDL(l), LongSigOps) {

    def *[A <: Quantity[A, SigFigsTuple#TL, SigFigsTuple]](that: A): A =
      times(that)
    def *[A <: Quantity[A, SigFigsTuple#TL, SigFigsTuple]](
      that: SVector[A, SigFigsTuple#TL, SigFigsTuple]): 
        SVector[A, SigFigsTuple#TL, SigFigsTuple] =
      times(that)
  }

  //implicit class QuantityLongTT(l: Long) extends QuantityDoubleTT(l.toDouble)

  implicit class QuantityIntT(i: Int) 
      extends QuantityHelper[SigFigsTuple#T, SDI](SDI(i), IntSigOps) {

    def *[A <: Quantity[A, SigFigsTuple#T, SigFigsTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, SigFigsTuple#T, SigFigsTuple]](
      that: SVector[A, SigFigsTuple#T, SigFigsTuple]): 
        SVector[A, SigFigsTuple#T, SigFigsTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityIntTL(i: Int) 
      extends QuantityHelper[SigFigsTuple#TL, SDI](SDI(i), IntSigOps) {

    def *[A <: Quantity[A, SigFigsTuple#TL, SigFigsTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, SigFigsTuple#TL, SigFigsTuple]](
      that: SVector[A, SigFigsTuple#TL, SigFigsTuple]): 
        SVector[A, SigFigsTuple#TL, SigFigsTuple] =
      times(that)
  }

  //implicit class QuantityIntTT(i: Int) extends QuantityDoubleTT(i.toDouble)

  implicit class QuantityBigDecimalT(bd: BigDecimal) 
      extends QuantityHelper[SigFigsTuple#T, SignificantDigits[BigDecimal]](
    SignificantDigits[BigDecimal](bd), BigDecimalSigOps) {

    def *[A <: Quantity[A, SigFigsTuple#T, SigFigsTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, SigFigsTuple#T, SigFigsTuple]](
      that: SVector[A, SigFigsTuple#T, SigFigsTuple]): 
        SVector[A, SigFigsTuple#T, SigFigsTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityBigDecimalTL(bd: BigDecimal) 
      extends QuantityHelper[SigFigsTuple#TL, SignificantDigits[BigDecimal]](
    SignificantDigits[BigDecimal](bd), BigDecimalSigOps) {

    def *[A <: Quantity[A, SigFigsTuple#TL, SigFigsTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, SigFigsTuple#TL, SigFigsTuple]](
      that: SVector[A, SigFigsTuple#TL, SigFigsTuple]): 
        SVector[A, SigFigsTuple#TL, SigFigsTuple] =
      times(that)
  }
  
  implicit class QuantityBigDecimalTC(bd: BigDecimal) 
      extends QuantityHelper[SigFigsTuple#TC, SignificantDigits[BigDecimal]](
    SignificantDigits[BigDecimal](bd), BigDecimalSigOps) {

    def *[A <: Quantity[A, SigFigsTuple#TC, SigFigsTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, SigFigsTuple#TC, SigFigsTuple]](
      that: SVector[A, SigFigsTuple#TC, SigFigsTuple]): 
        SVector[A, SigFigsTuple#TC, SigFigsTuple] =
      times(that)
  }

  //implicit class QuantityBigDecimalTT(i: Int) extends QuantityHelper(i.toDouble)
}
