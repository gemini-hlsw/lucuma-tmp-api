// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import io.circe.literal._

class MutationSuite extends OdbSuite {

  queryTest(
    query = """
      mutation BulkEditConstraints($bulkEditConstraints: UpdateObservationsInput!) {
        updateObservations(input: $bulkEditConstraints) {
          observations {
            id
            constraintSet {
              skyBackground
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "updateObservations" : {
          "observations": [
            {
              "id" : "o-3",
              "constraintSet" : {
                "skyBackground" : "GRAY"
              }
            },
            {
              "id" : "o-4",
              "constraintSet" : {
                "skyBackground" : "GRAY"
              }
            }
          ]
        }
      }
    """,
    variables = Some(json"""
      {
        "bulkEditConstraints": {
          "SET": {
            "constraintSet": {
              "skyBackground": "GRAY"
            }
          },
          "WHERE": {
            "id": { "IN": [ "o-3", "o-4" ] }
          }
        }
      }
    """)
  )

  queryTest(
    query = """
      mutation BulkEditScienceMode($bulkEditScienceMode: UpdateObservationsInput!) {
        updateObservations(input: $bulkEditScienceMode) {
          observations {
            id
            scienceMode {
              gmosSouthLongSlit {
                basic {
                  grating
                }
                advanced {
                  overrideGrating
                }
              }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "updateObservations" : {
          "observations": [
            {
              "id" : "o-3",
              "scienceMode": {
                "gmosSouthLongSlit": {
                  "basic": {
                    "grating": "B600_G5323"
                  },
                  "advanced": {
                    "overrideGrating": "R600_G5324"
                  }
                }
              }
            },
            {
              "id" : "o-4",
              "scienceMode": {
                "gmosSouthLongSlit": {
                  "basic": {
                    "grating": "B600_G5323"
                  },
                  "advanced": {
                    "overrideGrating": "R600_G5324"
                  }
                }
              }
            }
          ]
        }
      }
    """,
    variables = Some(json"""
      {
        "bulkEditScienceMode": {
          "SET": {
            "scienceMode": {
              "gmosSouthLongSlit": {
                "advanced": {
                  "overrideGrating": "R600_G5324"
                }
              }
            }
          },
          "WHERE": {
            "id": { "IN": [ "o-3", "o-4" ] }
          }
        }
      }
    """)
  )

  // Now if we group by science mode, 3 and 4 will be together and the others
  // separate.
  queryTest(
    query = """
      query ObservationsByScienceMode {
        scienceModeGroup(programId:"p-2") {
          matches {
            scienceMode {
              gmosSouthLongSlit {
                advanced {
                  overrideGrating
                }
              }
            }
            observationIds
            observations(LIMIT: 10) {
              matches {
                id
                title
              }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "scienceModeGroup" : {
          "matches" : [
            {
              "scienceMode" : {
                "gmosSouthLongSlit": {
                  "advanced" : null
                }
              },
              "observationIds" : [
                "o-2",
                "o-5",
                "o-6",
                "o-7"
              ],
              "observations" : {
                "matches" : [
                  {
                    "id" : "o-2",
                    "title" : "NGC 5949"
                  },
                  {
                    "id" : "o-5",
                    "title" : "NGC 3312"
                  },
                  {
                    "id" : "o-6",
                    "title" : "NGC 5949, NGC 3269, NGC 3312"
                  },
                  {
                    "id" : "o-7",
                    "title" : ""
                  }
                ]
              }
            },
            {
              "scienceMode" : {
                "gmosSouthLongSlit": {
                  "advanced" : {
                    "overrideGrating" : "R600_G5324"
                  }
                }
              },
              "observationIds" : [
                "o-3",
                "o-4"
              ],
              "observations" : {
                "matches" : [
                  {
                    "id" : "o-3",
                    "title" : "NGC 3312"
                  },
                  {
                    "id" : "o-4",
                    "title" : "NGC 3312"
                  }
                ]
              }
            }
          ]
        }
      }
    """
  )

  // Attempts to edit the elevation range but it fails because the min range is
  // set to 0.  There should be only one error message even though the edit
  // would be to two observations.
  queryTestFailure(
    query =
      """
        mutation BulkEditConstraints($bulkEditConstraints: UpdateObservationsInput!) {
          updateObservations(input: $bulkEditConstraints) {
            observations {
              id
              constraintSet {
                skyBackground
                elevationRange {
                  airMass {
                    min
                    max
                  }
                }
              }
            }
          }
        }
      """,
    errors = List(
      "'min' out of range: must be 1.0 <= min <= 3.0"
    ),
    variables = Some(json"""
      {
        "bulkEditConstraints": {
          "SET": {
            "constraintSet": {
              "skyBackground": "GRAY",
              "elevationRange": {
                "airMass": {
                  "min": 0.1,
                  "max": 2.0
                }
              }
            }
          },
          "WHERE": {
            "id": { "IN": [ "o-3", "o-4" ] }
          }
        }
      }
      """)
  )

  queryTest(
    query = """
      mutation EditMiscProperties($editObservationInput: UpdateObservationsInput!) {
        updateObservations(input: $editObservationInput) {
          observations {
            id
            visualizationTime
            posAngleConstraint {
              constraint
              angle { degrees }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "updateObservations" : {
          "observations": [
            {
              "id": "o-3",
              "visualizationTime": "2017-02-16T20:30:00Z",
              "posAngleConstraint": {
                "constraint": "ALLOW_FLIP",
                "angle": {
                  "degrees": 123.45
                }
              }
            }
          ]
        }
      }
    """,
    variables = Some(json"""
      {
        "editObservationInput": {
          "SET": {
            "visualizationTime": "2017-02-16T20:30:00Z",
            "posAngleConstraint": {
              "constraint": "ALLOW_FLIP",
              "angle": {
                "degrees": 123.45
              }
            }
          },
          "WHERE": {
            "id": { "EQ": "o-3" }
          }
        }
      }
    """)
  )

  queryTest(
    query = """
      mutation RemovePosAngleConstraint($editObservationInput: UpdateObservationsInput!) {
        updateObservations(input: $editObservationInput) {
          observations {
            id
            posAngleConstraint {
              constraint
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "updateObservations" : {
          "observations": [
            {
              "id": "o-3",
              "posAngleConstraint": null
            }
          ]
        }
      }
    """,
    variables = Some(json"""
      {
        "editObservationInput": {
          "SET": {
            "posAngleConstraint": null
          },
          "WHERE": {
            "id": { "EQ": "o-3" }
          }
        }
      }
    """)
  )

  queryTest(
    query = """
      mutation ToAverageParallactic($editObservationInput: UpdateObservationsInput!) {
        updateObservations(input: $editObservationInput) {
          observations {
            id
            posAngleConstraint {
              constraint
              angle {
                degrees
              }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "updateObservations" : {
          "observations": [
            {
              "id": "o-3",
              "posAngleConstraint": {
                "constraint": "AVERAGE_PARALLACTIC",
                "angle": null
              }
            }
          ]
        }
      }
    """,
    variables = Some(json"""
      {
        "editObservationInput": {
          "SET": {
            "posAngleConstraint": {
              "constraint": "AVERAGE_PARALLACTIC",
              "angle": null
            }
          },
          "WHERE": {
            "id": { "EQ": "o-3" }
          }
        }
      }
    """)
  )

  // Attempts to edit the pos angle constraint  but it fails because we switch
  // to fixed w/o setting an angle
  queryTestFailure(
    query =
      """
        mutation InvalidPosAngleConstraint($editObservationInput: UpdateObservationsInput!) {
          updateObservations(input: $editObservationInput) {
            observations {
              id
              posAngleConstraint {
                constraint
                angle {
                  degrees
                }
              }
            }
          }
        }
      """,
    errors = List(
      "FIXED constraints require an associated `angle` value"
    ),
    variables = Some(json"""
      {
        "editObservationInput": {
          "SET": {
            "posAngleConstraint": {
              "constraint": "FIXED",
              "angle": null
            }
          },
          "WHERE": {
            "id": { "EQ": "o-3" }
          }
        }
     }
      """)
  )

  queryTest(
    query = """
      mutation CloneObservation($cloneObservationInput: CloneObservationInput!) {
        cloneObservation(input: $cloneObservationInput) {
          originalObservation {
            posAngleConstraint {
              constraint
              angle {
                degrees
              }
            }
          }
          newObservation {
            posAngleConstraint {
              constraint
              angle {
                degrees
              }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "cloneObservation" : {
          "originalObservation": {
            "posAngleConstraint": {
              "constraint": "AVERAGE_PARALLACTIC",
              "angle": null
            }
          },
          "newObservation": {
            "posAngleConstraint": {
              "constraint": "FIXED",
              "angle": {
                "degrees": 45
              }
            }
          }
        }
      }
    """,
    variables = Some(json"""
      {
        "cloneObservationInput": {
          "observationId": "o-3",
          "SET": {
            "posAngleConstraint": {
              "constraint": "FIXED",
              "angle": {
                "degrees": 45
              }
            }
          }
        }
      }
    """)
  )

}