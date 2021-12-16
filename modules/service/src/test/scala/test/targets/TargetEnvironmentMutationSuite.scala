// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class TargetEnvironmentMutationSuite extends OdbSuite {

  //
  // Observations and their targets:
  //
  // o-2: NGC 5949
  // o-3: NGC 3312
  // o-4: NGC 3312
  // o-5: NGC 3312 (explicit base)
  // o-6: NGC 5949, NGC 3269, NGC 3312
  // o-7: <none>
  //

  // In o-3, replace NGC 3312 with NGC 5949
  queryTest(
    query ="""
      mutation UpdateTargetEnvironment($envEdit: BulkEditTargetEnvironmentInput!) {
        updateTargetEnvironment(input: $envEdit) {
          id
          targets {
            asterism {
              name
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "updateTargetEnvironment": [
          {
            "id": "o-3",
            "targets": {
              "asterism": [
                {
                  "name": "NGC 5949"
                }
              ]
            }
          }
        ]
      }
    """,
    variables =json"""
      {
        "envEdit": {
          "selectObservations": [ "o-3" ],
          "edit": {
            "asterism": [ "t-2" ]
          }
        }
      }
    """.some
  )

  //
  queryTestFailure(
    query ="""
      mutation UpdateTargetEnvironment($envEdit: BulkEditTargetEnvironmentInput!) {
        updateTargetEnvironment(input: $envEdit) {
          id
          targets {
            asterism {
              name
            }
          }
        }
      }
    """,
    errors = List("Cannot assign targets from programs other than p-2"),
    variables =json"""
      {
        "envEdit": {
          "selectObservations": [ "o-3" ],
          "edit": {
            "asterism": [ "t-6" ]
          }
        }
      }
    """.some
  )


  // Add an explicit base to o-3
  queryTest(
    query ="""
      mutation UpdateTargetEnvironment($envEdit: BulkEditTargetEnvironmentInput!) {
        updateTargetEnvironment(input: $envEdit) {
          id
          targets {
            explicitBase {
              ra { hms }
              dec { dms }
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "updateTargetEnvironment": [
          {
            "id": "o-3",
            "targets": {
              "explicitBase": {
                "ra": {
                  "hms": "01:00:00.000000"
                },
                "dec": {
                  "dms": "+02:00:00.000000"
                }
              }
            }
          }
        ]
      }
    """,
    variables =json"""
      {
        "envEdit": {
          "selectObservations": [ "o-3" ],
          "edit": {
            "explicitBase": {
              "ra": {
                "hms": "01:00:00.00"
              },
              "dec": {
                "dms": "02:00:00.00"
              }
            }
          }
        }
      }
    """.some

  )

}
