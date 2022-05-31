// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

package object syntax {

  object all extends ToDatabaseStateOps
                with ToEitherInputOps
                with ToInputOps
                with ToInputValidatorOps
                with ToLensOps
                with ToOptionalOps
                with ToPosAngleConstraintCompanionOps
                with ToPrismOps
                with ToTopLevelOps
                with ToValidatedInputOps

}
