// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.gmos.longslit

package object syntax {

  object all extends ToGmosNorthGratingOps
                with ToGmosNorthFilterCompanionOps
                with ToGmosNorthFpuOps
                with ToGmosSouthGratingOps
                with ToGmosSouthFilterCompanionOps
                with ToGmosSouthFpuOps

}
