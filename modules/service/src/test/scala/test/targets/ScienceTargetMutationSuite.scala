// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class ScienceTargetMutationSuite extends OdbSuite {

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

  // Bulk edit NGC 3312 to remove parallax altogether.
  queryTest(
    query ="""
      mutation UpdateScienceTarget($targetEdit: BulkEditScienceTargetInput!) {
        updateScienceTarget(input: $targetEdit) {
          id
          targets {
            science {
              name
              tracking {
                ... on Sidereal {
                  parallax { microarcseconds }
                }
              }
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "updateScienceTarget": [
          {
            "id": "o-3",
            "targets": {
              "science": [
                {
                  "name": "NGC 3312",
                  "tracking": {
                    "parallax": null
                  }
                }
              ]
            }
          },
          {
            "id": "o-4",
            "targets": {
              "science": [
                {
                  "name": "NGC 3312",
                  "tracking": {
                    "parallax": null
                  }
                }
              ]
            }
          },
          {
            "id": "o-5",
            "targets": {
              "science": [
                {
                  "name": "NGC 3312",
                  "tracking": {
                    "parallax": null
                  }
                }
              ]
            }
          },
          {
            "id": "o-6",
            "targets": {
              "science": [
                {
                  "name": "NGC 3269",
                  "tracking": {
                    "parallax": {
                      "microarcseconds": 0
                    }
                  }
                },
                {
                  "name": "NGC 3312",
                  "tracking": {
                    "parallax": null
                  }
                },
                {
                  "name": "NGC 5949",
                  "tracking": {
                    "parallax": {
                      "microarcseconds": 0
                    }
                  }
                }
              ]
            }
          }
        ]
      }
    """,
    variables =json"""
      {
        "targetEdit": {
          "selectObservations": [ "o-3", "o-4", "o-5", "o-6" ],
          "edit": {
            "selectTarget": "NGC 3312",
            "sidereal": {
              "parallax": null
            }
          }
        }
      }
    """.some
  )

  // Missing named target.
  queryTestFailure(
    query ="""
      mutation UpdateScienceTarget($targetEdit: BulkEditScienceTargetInput!) {
        updateScienceTarget(input: $targetEdit) {
          id
          targets {
            science {
              name
            }
          }
        }
      }
    """,
    errors = List(
      "Missing science target 'NGC 9999' in observation o-3"
    ),
    variables =json"""
      {
        "targetEdit": {
          "selectObservations": [ "o-3" ],
          "edit": {
            "selectTarget": "NGC 9999",
            "sidereal": {
              "parallax": null
            }
          }
        }
      }
    """.some
  )

}
