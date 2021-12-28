// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.MonadError
import cats.effect.std.Dispatcher
import cats.syntax.all._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.`enum`.{Band, PlanetSpectrum}
import lucuma.core.math.BrightnessUnits.{Brightness, FluxDensityContinuum, Integrated, LineFlux}
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional.UnitType
import lucuma.core.math.units.VegaMagnitude
import lucuma.core.model.{BandBrightness, EmissionLine, SourceProfile, SpectralDefinition, UnnormalizedSED}
import lucuma.odb.api.repo.{OdbRepo, ResultPage}
import lucuma.odb.api.model.targetModel.TargetModel
import lucuma.odb.api.schema.TargetSchema.ArgumentTargetId
import sangria.schema._
import shapeless.tag.@@

import scala.collection.immutable.SortedMap


trait TargetQuery {
  import context._

  import GeneralSchema.ArgumentIncludeDeleted
  import ObservationSchema.{ ObservationIdArgument, OptionalListObservationIdArgument }
  import Paging._
  import ProgramSchema.{ OptionalProgramIdArgument, ProgramIdArgument }
  import TargetSchema.{TargetEnvironmentType, TargetConnectionType, TargetType}

  def target[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "target",
      fieldType   = OptionType(TargetType[F]),
      description = "Retrieves the target with the given id, if it exists".some,
      arguments   = List(
        ArgumentTargetId,
        ArgumentIncludeDeleted
      ),
      resolve     = c => c.target(_.select(c.targetId, c.includeDeleted))
    )

  def referencedScienceTargets[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "referencedScienceTargets",
      fieldType   = TargetConnectionType[F],
      description = "All the science targets that are used by one or more observations in the given program".some,
      arguments   = List(
        ProgramIdArgument,
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve = c =>
        unsafeSelectTopLevelPageFuture(c.pagingTargetId) { gid =>
          c.ctx.target.selectReferencedPageForProgram(c.programId, c.pagingFirst, gid, c.includeDeleted)
        }
    )

  def allScienceTargets[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "allScienceTargets",
      fieldType   = TargetConnectionType[F],
      description = "All the science targets (used or not) associated with a given program or specific observations".some,
      arguments   = List(
        OptionalProgramIdArgument,
        OptionalListObservationIdArgument,
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve = c =>
        unsafeSelectTopLevelPageFuture(c.pagingTargetId) { gid =>
          (c.optionalProgramId, c.arg(OptionalListObservationIdArgument)) match {
            case (_, Some(oids)) => c.ctx.target.selectPageForObservations(oids.toSet, c.pagingFirst, gid, c.includeDeleted)
            case (Some(pid), _)  => c.ctx.target.selectPageForProgram(pid, c.pagingFirst, gid, c.includeDeleted)
            case _               => ResultPage.empty[TargetModel].pure[F]
          }
        }
    )

  def firstScienceTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "firstScienceTarget",
      fieldType   = OptionType(TargetType[F]),
      description = "The first (or only) science target (if any) for the given observation.  This will essentially pick a random target from the observation's asterism and is meant as a convenience when there is only one target.".some,
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.target(_.selectObservationFirstTarget(c.observationId))
    )

  def asterism[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "asterism",
      fieldType   = ListType(TargetType[F]),
      description = "All science targets (if any) for the given observation (or environment)".some,
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.target(_.selectObservationAsterism(c.observationId, c.includeDeleted).map(_.toList))
    )

  def targetEnvironment[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "targetEnvironment",
      fieldType   = OptionType(TargetEnvironmentType[F]),
      description = "Target environment for the given observation (or environment id)".some,
      arguments   = List(ObservationIdArgument),
      resolve     = c => c.target(_.selectObservationTargetEnvironment(c.observationId))
    )

  def testBandNormalizedIntegrated[F[_]]: Field[OdbRepo[F], Unit] = {
    import SourceProfileSchema._

    Field(
      name        = "testBandNormalizedIntegrated",
      fieldType   = BandNormalizedIntegrated,
      description = "test band normalized integrated".some,
      resolve     = _ =>

          SpectralDefinition.BandNormalized(
            UnnormalizedSED.Planet(PlanetSpectrum.Mars),
            SortedMap.from[Band, BandBrightness[Integrated]](
              List(
                (Band.R: Band) ->
                  BandBrightness[Integrated, VegaMagnitude](
                    BrightnessValue.fromDouble(10.0),
                    Band.R: Band
                  )
              )
            )
          )
    )
  }

  def testEmissionLinesIntegrated[F[_]]: Field[OdbRepo[F], Unit] = {
    import SourceProfileSchema._

    import cats.Order.catsKernelOrderingForOrder
    import coulomb._
    import lucuma.core.math.Wavelength
    import lucuma.core.math.dimensional._
    import lucuma.core.math.units.KilometersPerSecond

    val one: PosBigDecimal = PosBigDecimal.from(BigDecimal("1.0")).toOption.get

    Field(
      name        = "testEmissionLinesIntegrated",
      fieldType   = EmissionLinesIntegrated,
      description = "test emission lines integrated".some,
      resolve     = _ =>

          SpectralDefinition.EmissionLines(
            SortedMap.from(
              List(
                Wavelength.Min ->
                  EmissionLine[Integrated](
                    Wavelength.Min,
                    Quantity[PosBigDecimal, KilometersPerSecond](one),
                    shapeless.tag[LineFlux[Integrated]](Qty(one, LineFlux.Integrated.all.head))
                  )
              )
            ),
            shapeless.tag[FluxDensityContinuum[Integrated]](Qty(one, FluxDensityContinuum.Integrated.all.head))
          )
    )
  }

  def testEmissionLineIntegrated[F[_]]: Field[OdbRepo[F], Unit] = {
    import SourceProfileSchema._

    import coulomb._
    import lucuma.core.math.Wavelength
    import lucuma.core.math.dimensional._
    import lucuma.core.math.units.KilometersPerSecond

    val one: PosBigDecimal = PosBigDecimal.from(BigDecimal("1.0")).toOption.get

    Field(
      name        = "testEmissionLineIntegrated",
      fieldType   = EmissionLineIntegrated,
      description = "test emission line integrated".some,
      resolve     = _ =>

                  EmissionLine[Integrated](
                    Wavelength.Min,
                    Quantity[PosBigDecimal, KilometersPerSecond](one),
                    shapeless.tag[LineFlux[Integrated]](Qty(one, LineFlux.Integrated.all.head))
                  )
    )
  }

  def testGroupedUnitQty[F[_]]: Field[OdbRepo[F], Unit] = {
    import SourceProfileSchema._

    import lucuma.core.math.dimensional._

    val one: PosBigDecimal = PosBigDecimal.from(BigDecimal("1.0")).toOption.get

    println("XXXXXXX" + shapeless.tag[LineFlux[Integrated]](Qty(one, LineFlux.Integrated.all.head)))

    Field(
      name        = "testGroupedUnitQty",
      fieldType   = LineFluxIntegratedType,
      description = "test grouped unit qty".some,
      resolve     = _ => {
        shapeless.tag[LineFlux[Integrated]](Qty(one, LineFlux.Integrated.all.head)): Qty[PosBigDecimal] @@ LineFlux[Integrated]
      }
    )
  }

  def testPosBigDecimal[F[_]]: Field[OdbRepo[F], Unit] = {

    import GeneralSchema.PosBigDecimalType

    Field(
      name        = "testPosBigDecimal",
      fieldType   = PosBigDecimalType,
      description = "test pos big decimal".some,
      resolve     = _ => PosBigDecimal.from(BigDecimal("3.0")).toOption.get
    )
  }



  def testSpectralDefinitionIntegrated[F[_]]: Field[OdbRepo[F], Unit] = {
    import SourceProfileSchema._

    Field(
      name        = "testSpectralDefinitionIntegrated",
      fieldType   = SpectralDefinitionIntegrated,
      description = "test spectral definition integrated".some,
      resolve     = _ =>

          SpectralDefinition.BandNormalized(
            UnnormalizedSED.Planet(PlanetSpectrum.Mars),
            SortedMap.from[Band, BandBrightness[Integrated]](
              List(
                (Band.R: Band) ->
                  BandBrightness[Integrated, VegaMagnitude](
                    BrightnessValue.fromDouble(10.0),
                    Band.R: Band
                  )
              )
            )
          )
    )
  }

  def testPointSource[F[_]]: Field[OdbRepo[F], Unit] = {
    import SourceProfileSchema._

    Field(
      name        = "testPointSource",
      fieldType   = PointType,
      description = "test point source".some,
      resolve     = _ =>

        SourceProfile.Point(
          SpectralDefinition.BandNormalized(
            UnnormalizedSED.Planet(PlanetSpectrum.Mars),
            SortedMap.from[Band, BandBrightness[Integrated]](
              List(
                (Band.R: Band) ->
                  BandBrightness[Integrated, VegaMagnitude](
                    BrightnessValue.fromDouble(10.0),
                    Band.R: Band
                  )
              )
            )
          )
        )
    )
  }

  def testLineFluxIntegrated[F[_]]: Field[OdbRepo[F], Unit] = {

    import SourceProfileSchema._

    Field(
      name        = "testLineFluxIntegrated",
      fieldType   = EnumTypeLineFluxIntegrated,
      description = "line flux integrated enum".some,
      resolve     = _ => LineFlux.Integrated.all.head
    )

  }
  def testLineFluxSurface[F[_]]: Field[OdbRepo[F], Unit] = {

    import SourceProfileSchema._

    Field(
      name        = "testLineFluxSurface",
      fieldType   = EnumTypeLineFluxSurface,
      description = "line flux surface enum".some,
      resolve     = _ => LineFlux.Surface.all.head
    )

  }

  def testBrightnessIntegrated[F[_]]: Field[OdbRepo[F], Unit] = {

    import SourceProfileSchema._

    Field(
      name        = "testBrightnessIntegrated",
      fieldType   = EnumTypeBrightnessIntegrated,
      description = "brightness integrated enum".some,
      resolve     = _ => Brightness.Integrated.all.head
    )

  }

  def testBrightnessSurface[F[_]]: Field[OdbRepo[F], Unit] = {

    import SourceProfileSchema._

    Field(
      name        = "testBrightnessSurface",
      fieldType   = EnumTypeBrightnessSurface,
      description = "brightness surface enum".some,
      resolve     = _ => Brightness.Surface.all.head
    )

  }


  def testFluxDensityContinuumIntegrated[F[_]]: Field[OdbRepo[F], Unit] = {

    import SourceProfileSchema._

    Field(
      name        = "testFluxDensityContinuumIntegrated",
      fieldType   = EnumTypeFluxDensityContinuumIntegrated,
      description = "flux density continuum integrated enum".some,
      resolve     = _ => FluxDensityContinuum.Integrated.all.head
    )

  }



  def testSourceProfile[F[_]]: Field[OdbRepo[F], Unit] = {
    import SourceProfileSchema._

    Field(
      name        = "testSourceProfile",
      fieldType   = SourceProfileType,
      description = "test source profile".some,
      resolve     = _ =>

        SourceProfile.Point(
          SpectralDefinition.BandNormalized(
            UnnormalizedSED.Planet(PlanetSpectrum.Mars),
            SortedMap.from[Band, BandBrightness[Integrated]](
              List(
                (Band.R: Band) ->
                  BandBrightness[Integrated, VegaMagnitude](
                    BrightnessValue.fromDouble(10.0),
                    Band.R: Band
                  )
              )
            )
          )
        )
    )
  }

  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      target[F],
      referencedScienceTargets[F],
      firstScienceTarget[F],
      asterism[F],
      targetEnvironment[F],
      testBandNormalizedIntegrated[F],
      testPosBigDecimal[F],
      testEmissionLineIntegrated[F],
//      testBrightnessIntegrated[F],
//      testBrightnessSurface[F],
      testLineFluxIntegrated[F],
//      testLineFluxSurface[F],
//      testFluxDensityContinuumIntegrated[F],
      testGroupedUnitQty[F],
      testEmissionLinesIntegrated[F],
      testSpectralDefinitionIntegrated[F],
      testPointSource[F],
      testSourceProfile[F]
    )
}

object TargetQuery extends TargetQuery
