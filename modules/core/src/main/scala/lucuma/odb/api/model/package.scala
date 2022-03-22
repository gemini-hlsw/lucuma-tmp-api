// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api

import cats.data.{EitherNec, ValidatedNec}

package object model {

  type EitherInput[A] = EitherNec[InputError, A]

  type ValidatedInput[A] = ValidatedNec[InputError, A]

  type Sequence[D] = SequenceModel[AtomModel[StepModel[D]]]

}
