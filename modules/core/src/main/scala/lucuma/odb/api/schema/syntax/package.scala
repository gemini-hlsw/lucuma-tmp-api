// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

package object syntax {

  object all
    extends ToEnumTypeCompanionOps
       with ToFromInputCompanionOps
       with ToFromInputOps
       with ToInputTypeOps
       with ToToInputCompanionOps
       with ToToInputOps
       with ToScalarTypeCompanionOps

}
