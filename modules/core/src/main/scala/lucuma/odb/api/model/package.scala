// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api

import cats.data.ValidatedNec

package object model {

  type ValidatedInput[A] = ValidatedNec[InputError, A]

  type DereferencedSequence[D] = SequenceModel[AtomModel[StepModel[D]]]

}
