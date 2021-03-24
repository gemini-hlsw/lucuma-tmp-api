// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.Enumerated
import cats.{Eq, Semigroup}
import cats.syntax.all._

import java.util.concurrent.TimeUnit

import scala.concurrent.duration._

object StepTimeModel {

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

  import duration._

  final case class CategorizedTime(
    configChange: NonNegativeFiniteDuration,
    exposure:     NonNegativeFiniteDuration,
    readout:      NonNegativeFiniteDuration,
    write:        NonNegativeFiniteDuration
  ) {

    def total: NonNegativeFiniteDuration =
      configChange |+| exposure |+| readout |+| write

    def +(that: CategorizedTime): CategorizedTime =
      CategorizedTime(
        configChange |+| that.configChange,
        exposure     |+| that.exposure,
        readout      |+| that.readout,
        write        |+| that.write
      )

    def addTime(category: Category, time: NonNegativeFiniteDuration): CategorizedTime =
      category match {
        case Category.ConfigChange => copy(configChange |+| time)
        case Category.Exposure     => copy(exposure     |+| time)
        case Category.Readout      => copy(readout      |+| time)
        case Category.Write        => copy(write        |+| time)
      }

  }

  object CategorizedTime {

    private val zeroDuration: NonNegativeFiniteDuration =
      NonNegativeFiniteDuration.unsafeFrom(
        FiniteDuration(0L, TimeUnit.SECONDS)
      )

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

  /**
   * Provides placeholder time estimate for the given step until an accurate
   * version can be implemented.
   */
  def estimate[D](s: StepModel[D]): CategorizedTime = {

    def forExposure(exposure: FiniteDuration): CategorizedTime =
      CategorizedTime(
        configChange = NonNegativeFiniteDuration.unsafeFrom(7.seconds),
        exposure     = NonNegativeFiniteDuration.unsafeFrom(if (exposure.length >= 0) exposure else FiniteDuration(0L, TimeUnit.SECONDS)),
        readout      = NonNegativeFiniteDuration.unsafeFrom(71400.milliseconds),
        write        = NonNegativeFiniteDuration.unsafeFrom(10.seconds)
      )

    def forDynamicConfig(d: D): CategorizedTime =
      d match {
        case g: GmosModel.NorthDynamic => forExposure(g.exposure)
        case g: GmosModel.SouthDynamic => forExposure(g.exposure)
        case _                         => CategorizedTime.Zero
      }

    s match {
      case StepModel.Bias(a)          => forDynamicConfig(a)
      case StepModel.Dark(a)          => forDynamicConfig(a)
      case StepModel.Gcal(_, g)       => forExposure(g.exposureTime)
      case StepModel.Science(a, _)    => forDynamicConfig(a)

    }
  }

}
