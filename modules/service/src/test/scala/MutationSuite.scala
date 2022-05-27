// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import io.circe.literal._

class MutationSuite extends OdbSuite {

  queryTest(
    query = """
      mutation BulkEditConstraints($bulkEditConstraints: EditObservationInput!) {
        editObservation(input: $bulkEditConstraints) {
          id
          constraintSet {
            skyBackground
          }
        }
      }
    """,
    expected = json"""
      {
        "editObservation" : [
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
    """,
    variables = Some(json"""
      {
        "bulkEditConstraints": {
          "select": {
            "observationIds": [ "o-3", "o-4" ]
          },
          "patch": {
            "constraintSet": {
              "skyBackground": "GRAY"
            }
          }
        }
      }
    """)
  )

  queryTest(
    query = """
      mutation BulkEditScienceMode($bulkEditScienceMode: EditObservationInput!) {
        editObservation(input: $bulkEditScienceMode) {
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
    """,
    expected = json"""
      {
        "editObservation" : [
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
    """,
    variables = Some(json"""
      {
        "bulkEditScienceMode": {
          "select": {
            "observationIds": [ "o-3", "o-4" ]
          },
          "patch": {
            "scienceMode": {
              "gmosSouthLongSlit": {
                "advanced": {
                  "overrideGrating": "R600_G5324"
                }
              }
            }
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
          nodes {
            scienceMode {
              gmosSouthLongSlit {
                advanced {
                  overrideGrating
                }
              }
            }
            observationIds
            observations(first: 10) {
              nodes {
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
          "nodes" : [
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
                "nodes" : [
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
                "nodes" : [
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
        mutation BulkEditConstraints($bulkEditConstraints: EditObservationInput!) {
          editObservation(input: $bulkEditConstraints) {
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
      """,
    errors = List(
      "'min' out of range: must be 1.0 <= min <= 3.0"
    ),
    variables = Some(json"""
      {
        "bulkEditConstraints": {
          "select": {
            "observationIds": [ "o-3", "o-4" ]
          },
          "patch": {
            "constraintSet": {
              "skyBackground": "GRAY",
              "elevationRange": {
                "airMass": {
                  "min": 0.1,
                  "max": 2.0
                }
              }
            }
          }
        }
      }
      """)
  )

  queryTest(
    query = """
      mutation EditMiscProperties($editObservationInput: EditObservationInput!) {
        editObservation(input: $editObservationInput) {
          id
          visualizationTime
        }
      }
    """,
    expected = json"""
      {
        "editObservation" : [
          {
            "id": "o-3",
            "visualizationTime": "2017-02-16T20:30:00Z"
          }
        ]
      }
    """,
    variables = Some(json"""
      {
        "editObservationInput": {
          "select": {
            "observationIds": [ "o-3" ]
          },
          "patch": {
            "visualizationTime": "2017-02-16T20:30:00Z"
          }
        }
      }
    """)
  )
}