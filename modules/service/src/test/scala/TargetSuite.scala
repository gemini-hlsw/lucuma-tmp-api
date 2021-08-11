// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import io.circe.literal._
import cats.syntax.option._

class TargetSuite extends OdbSuite {

  //
  // Observations and their targets:
  //
  // o-2: NGC 5949
  // o-3: NGC 3312
  // o-4: NGC 3312
  // o-5: NGC 3312* (explicit base)
  // o-6: NGC 5949, NGC 3269, NGC 3312
  // o-7: <none>
  //

  // Pick the first alphabetically by name (or only) science target for an observation.
  queryTest(
    query ="""
      query ScienceTargetForObservation {
        scienceTarget(observationId: "o-2") {
          name
        }
      }
    """,
    expected =json"""
      {
        "scienceTarget": {
          "name": "NGC 5949"
        }
      }
    """
  )

  // All science targets
  queryTest(
    query ="""
      query ScienceTargetsForObservation {
        scienceTargets(observationId: "o-6") {
          name
        }
      }
    """,
    expected =json"""
      {
        "scienceTargets": [
          {
            "name": "NGC 3269"
          },
          {
            "name": "NGC 3312"
          },
          {
            "name": "NGC 5949"
          }
        ]
      }
    """
  )

  // Target environment
  queryTest(
    query ="""
      query TargetEnvironmentForObservation {
        targetEnvironment(observationId: "o-5") {
          explicitBase {
            ra { hms }
            dec { dms }
          }
          science {
            name
          }
        }
      }
    """,
    expected =json"""
      {
        "targetEnvironment": {
          "explicitBase": {
            "ra": {
              "hms": "10:37:01.992000"
            },
            "dec": {
              "dms": "-27:33:54.000000"
            }
          },
          "science": [
            {
              "name": "NGC 3312"
            }
          ]
        }
      }
    """
  )

  // Group by individual science target.
  //
  // NGC 5949 => o-2, o-6
  // NGC 3312 => o-3, o-4, o-5, o-6
  // NGC 3269 => o-6
  queryTest(
    query ="""
      query GroupByScienceTarget {
        scienceTargetGroup(programId: "p-2") {
          nodes {
             observationIds
             scienceTarget {
               name
             }
          }
        }
      }
    """,
    expected = json"""
      {
        "scienceTargetGroup": {
          "nodes": [
            {
              "observationIds": [
                "o-2",
                "o-6"
              ],
              "scienceTarget": {
                "name": "NGC 5949"
              }
            },
            {
              "observationIds": [
                "o-3",
                "o-4",
                "o-5",
                "o-6"
              ],
              "scienceTarget": {
                "name": "NGC 3312"
              }
            },
            {
              "observationIds": [
                "o-6"
              ],
              "scienceTarget": {
                "name": "NGC 3269"
              }
            }
          ]
        }
      }
    """
  )

  // Group by all science targets
  queryTest(
    query ="""
      query GroupByAllScienceTargets {
        allScienceTargetsGroup(programId: "p-2") {
          nodes {
             observationIds
             allScienceTargets {
               name
             }
          }
        }
      }
    """,
    expected = json"""
      {
        "allScienceTargetsGroup": {
          "nodes": [
            {
              "observationIds": [
                "o-2"
              ],
              "allScienceTargets": [
                {
                  "name": "NGC 5949"
                }
              ]
            },
            {
              "observationIds": [
                "o-3",
                "o-4",
                "o-5"
              ],
              "allScienceTargets": [
                {
                  "name": "NGC 3312"
                }
              ]
            },
            {
              "observationIds": [
                "o-6"
              ],
              "allScienceTargets": [
                {
                  "name": "NGC 3269"
                },
                {
                  "name": "NGC 3312"
                },
                {
                  "name": "NGC 5949"
                }
              ]
            },
            {
              "observationIds": [
                "o-7"
              ],
              "allScienceTargets": []
            }
          ]
        }
      }
    """
  )

  // Group by target environment, including all properties (e.g., explicit base)
  queryTest(
    query ="""
      query GroupByTargetEnvironment {
        targetEnvironmentGroup(programId: "p-2") {
          nodes {
             observationIds
             targetEnvironment {
               explicitBase {
                 ra { hms }
                 dec { dms }
               }
               science {
                 name
               }
             }
          }
        }
      }
    """,
    expected = json"""
      {
        "targetEnvironmentGroup": {
          "nodes": [
            {
              "observationIds": [
                "o-2"
              ],
              "targetEnvironment": {
                "explicitBase": null,
                "science": [
                  {
                    "name": "NGC 5949"
                  }
                ]
              }
            },
            {
              "observationIds": [
                "o-3",
                "o-4"
              ],
              "targetEnvironment": {
                "explicitBase": null,
                "science": [
                  {
                    "name": "NGC 3312"
                  }
                ]
              }
            },
            {
              "observationIds": [
                "o-5"
              ],
              "targetEnvironment": {
                "explicitBase": {
                  "ra": {
                    "hms": "10:37:01.992000"
                  },
                  "dec": {
                    "dms": "-27:33:54.000000"
                  }
                },
                "science": [
                  {
                    "name": "NGC 3312"
                  }
                ]
              }
            },
            {
              "observationIds": [
                "o-6"
              ],
              "targetEnvironment": {
                "explicitBase": null,
                "science": [
                  {
                    "name": "NGC 3269"
                  },
                  {
                    "name": "NGC 3312"
                  },
                  {
                    "name": "NGC 5949"
                  }
                ]
              }
            },
            {
              "observationIds": [
                "o-7"
              ],
              "targetEnvironment": {
                "explicitBase": null,
                "science": []
              }
            }
          ]
        }
      }
    """
  )

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

  // Rename target NGC 3312 in o-3
  queryTest(
    query ="""
      mutation UpdateScienceTarget($renameEdit: BulkEditScienceTargetInput!) {
        updateScienceTarget(input: $renameEdit) {
          id
          targets {
            science {
              name
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
                  "name": "NGC 3312*"
                }
              ]
            }
          }
        ]
      }
    """,
    variables =json"""
      {
        "renameEdit": {
          "selectObservations": [ "o-3" ],
          "edit": {
            "selectTarget": "NGC 3312",
            "sidereal": {
              "name": "NGC 3312*"
            }
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)  // cannot run this test twice since it changes required state
  )

  // Renaming disallowed since it would replace an existing target.
  queryTestFailure(
    query ="""
      mutation UpdateScienceTarget($renameEdit: BulkEditScienceTargetInput!) {
        updateScienceTarget(input: $renameEdit) {
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
      "Cannot rename 'NGC 3312' to 'NGC 5949' because there is already a science target named 'NGC 5949' in observation o-6"
    ),
    variables =json"""
      {
        "renameEdit": {
          "selectObservations": [ "o-6" ],
          "edit": {
            "selectTarget": "NGC 3312",
            "sidereal": {
              "name": "NGC 5949"
            }
          }
        }
      }
    """.some
  )

}
