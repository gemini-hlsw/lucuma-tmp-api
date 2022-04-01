// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.gmos.longslit

package object syntax {

  object all extends ToGmosNorthDisperserOps
                with ToGmosNorthFilterCompanionOps
                with ToGmosNorthFpuOps
                with ToGmosSouthDisperserOps
                with ToGmosSouthFilterCompanionOps
                with ToGmosSouthFpuOps

}
