// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.Enumerated
import lucuma.core.syntax.time._
import lucuma.odb.api.model.time._
import cats.{Eq, Semigroup}
import cats.data.NonEmptyList
import cats.syntax.all._
import org.typelevel.cats.time.instances.duration._


final case class PlannedTime(
  setup:       NonNegDuration,
  acquisition: List[PlannedTime.CategorizedTime],
  science:     List[PlannedTime.CategorizedTime]
) {

  def acquisitionSum: PlannedTime.CategorizedTime =
    NonEmptyList(PlannedTime.CategorizedTime.Zero, acquisition).reduce

  def scienceSum: PlannedTime.CategorizedTime =
    NonEmptyList(PlannedTime.CategorizedTime.Zero, science).reduce

  def total: NonNegDuration =
    setup |+| acquisition.foldMap(_.total) |+| science.foldMap(_.total)

}

object PlannedTime {

  val Zero: PlannedTime =
    PlannedTime(
      NonNegDuration.zero,
      Nil,
      Nil
    )

  sealed trait Category extends Product with Serializable

  object Category {

    case object ConfigChange extends Category
    case object Exposure     extends Category
    case object Readout      extends Category
    case object Write        extends Category

    val configChange: Category = ConfigChange
    val exposure: Category     = Exposure
    val readout: Category      = Readout
    val write: Category        = Write

    implicit val EnumeratedCategory: Enumerated[Category] = {
      Enumerated.of(ConfigChange, Exposure, Readout, Write)
    }

  }

  final case class CategorizedTime(
    configChange: NonNegDuration,
    exposure:     NonNegDuration,
    readout:      NonNegDuration,
    write:        NonNegDuration
  ) {

    def total: NonNegDuration =
      configChange |+| exposure |+| readout |+| write

    def +(that: CategorizedTime): CategorizedTime =
      CategorizedTime(
        configChange |+| that.configChange,
        exposure     |+| that.exposure,
        readout      |+| that.readout,
        write        |+| that.write
      )

    def addTime(category: Category, time: NonNegDuration): CategorizedTime =
      category match {
        case Category.ConfigChange => copy(configChange |+| time)
        case Category.Exposure     => copy(exposure     |+| time)
        case Category.Readout      => copy(readout      |+| time)
        case Category.Write        => copy(write        |+| time)
      }

  }

  object CategorizedTime {

    private val zeroDuration: NonNegDuration =
      NonNegDuration.unsafeFrom(0L.seconds)

    // Zero but not a valid Monoid zero because of the time units
    val Zero: CategorizedTime =
      CategorizedTime(zeroDuration, zeroDuration, zeroDuration, zeroDuration)

    implicit val EqCategorizedTime: Eq[CategorizedTime] =
      Eq.by(a => (
        a.configChange.value,
        a.exposure.value,
        a.readout.value,
        a.write.value
      ))

    implicit val SemigroupCategorizedTime: Semigroup[CategorizedTime] =
      Semigroup.instance[CategorizedTime](_ + _)

  }

  // Placeholder estimate.  In reality you cannot estimate a step independently
  // like this because you need to account for changes from the previous step.
  def estimateStep[D](s: StepConfig[D]): CategorizedTime = {
    def forExposure(exposure: NonNegDuration): CategorizedTime =
      CategorizedTime(
        configChange = NonNegDuration.unsafeFrom(7.seconds),
        exposure     = NonNegDuration.unsafeFrom(exposure.value),
        readout      = NonNegDuration.unsafeFrom(71400.milliseconds),
        write        = NonNegDuration.unsafeFrom(10.seconds)
      )

    def forDynamicConfig(d: D): CategorizedTime =
      d match {
        case g: GmosModel.NorthDynamic => forExposure(g.exposure)
        case g: GmosModel.SouthDynamic => forExposure(g.exposure)
        case _                         => CategorizedTime.Zero
      }

    s match {
      case StepConfig.Bias(a)       => forDynamicConfig(a)
      case StepConfig.Dark(a)       => forDynamicConfig(a)
      case StepConfig.Gcal(a, _)    => forDynamicConfig(a)
      case StepConfig.Science(a, _) => forDynamicConfig(a)

    }
  }

  def estimateAtom[D](a: AtomModel[StepModel[D]]): CategorizedTime =
    a.steps.map(s => estimateStep(s.config)).reduce

  def estimateSequence[D](s: Sequence[D]): CategorizedTime =
    NonEmptyList(CategorizedTime.Zero, s.atoms.map(estimateAtom)).reduce

  def estimate(config: ExecutionModel): PlannedTime = {
    val gmosSetup = NonNegDuration.unsafeFrom(18.minutes)

    config match {
      case gn: ExecutionModel.GmosNorth =>
        PlannedTime(
          gmosSetup,
          gn.config.acquisition.atoms.map(estimateAtom),
          gn.config.science.atoms.map(estimateAtom)
        )

      case gs: ExecutionModel.GmosSouth =>
        PlannedTime(
          gmosSetup,
          gs.config.acquisition.atoms.map(estimateAtom),
          gs.config.science.atoms.map(estimateAtom)
        )
    }
  }

}
