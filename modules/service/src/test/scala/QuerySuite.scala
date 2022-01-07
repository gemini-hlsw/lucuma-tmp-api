// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import io.circe.literal._

class QuerySuite extends OdbSuite {

  queryTest(
    query = """
      query Programs {
        programs(programIds: ["p-2", "p-3", "p-4"]) {
          nodes {
            id
            name
          }
        }
      }
    """,
    expected = json"""
      {
        "programs" : {
          "nodes" : [
            {
              "id" : "p-2",
              "name" : "The real dark matter was the friends we made along the way"
            },
            {
              "id" : "p-3",
              "name" : "An Empty Placeholder Program"
            }
          ]
        }
      }
    """
  )

  queryTest(
    query = """
      query Observations {
        observations(programId: "p-2") {
          nodes {
            id
            constraintSet {
              cloudExtinction
              imageQuality
              skyBackground
              waterVapor
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "observations" : {
          "nodes" : [
            {
              "id" : "o-2",
              "constraintSet" : {
                "cloudExtinction" : "POINT_THREE",
                "imageQuality" : "POINT_EIGHT",
                "skyBackground" : "BRIGHT",
                "waterVapor" : "WET"
              }
            },
            {
              "id" : "o-3",
              "constraintSet" : {
                "cloudExtinction" : "POINT_THREE",
                "imageQuality" : "POINT_EIGHT",
                "skyBackground" : "BRIGHT",
                "waterVapor" : "WET"
              }
            },
            {
              "id" : "o-4",
              "constraintSet" : {
                "cloudExtinction" : "POINT_THREE",
                "imageQuality" : "POINT_EIGHT",
                "skyBackground" : "BRIGHT",
                "waterVapor" : "WET"
              }
            },
            {
              "id" : "o-5",
              "constraintSet" : {
                "cloudExtinction" : "POINT_THREE",
                "imageQuality" : "POINT_EIGHT",
                "skyBackground" : "BRIGHT",
                "waterVapor" : "WET"
              }
            },
            {
              "id" : "o-6",
              "constraintSet" : {
                "cloudExtinction" : "POINT_THREE",
                "imageQuality" : "POINT_EIGHT",
                "skyBackground" : "BRIGHT",
                "waterVapor" : "WET"
              }
            },
            {
              "id" : "o-7",
              "constraintSet" : {
                "cloudExtinction" : "POINT_THREE",
                "imageQuality" : "POINT_EIGHT",
                "skyBackground" : "BRIGHT",
                "waterVapor" : "WET"
              }
            }
          ]
        }
      }
    """
  )

  queryTest(
    query = """
      query ObservationsByConstraintSet {
        constraintSetGroup(programId:"p-2") {
          nodes {
            constraintSet {
              cloudExtinction
              imageQuality
              skyBackground
              waterVapor
            }
            observationIds
            observations(first: 10) {
              nodes {
                id
                name
              }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "constraintSetGroup" : {
          "nodes" : [
            {
              "constraintSet" : {
                "cloudExtinction" : "POINT_THREE",
                "imageQuality" : "POINT_EIGHT",
                "skyBackground" : "BRIGHT",
                "waterVapor" : "WET"
              },
              "observationIds" : [
                "o-2",
                "o-3",
                "o-4",
                "o-5",
                "o-6",
                "o-7"
              ],
              "observations" : {
                "nodes" : [
                  {
                    "id" : "o-2",
                    "name" : "NGC 5949"
                  },
                  {
                    "id" : "o-3",
                    "name" : "NGC 3312"
                  },
                  {
                    "id" : "o-4",
                    "name" : "NGC 3312"
                  },
                  {
                    "id" : "o-5",
                    "name" : "NGC 3312"
                  },
                  {
                    "id" : "o-6",
                    "name" : "NGC 5949"
                  },
                  {
                    "id" : "o-7",
                    "name" : "Observation"
                  }
                ]
              }
            }
          ]
        }
      }
    """
  )

  queryTest(
    query = """
      query ObservationsByScienceRequirements {
        scienceRequirementsGroup(programId:"p-2") {
          nodes {
            scienceRequirements {
              mode
              spectroscopyRequirements {
                wavelength { nanometers }
              }
            }
            observationIds
            observations(first: 10) {
              nodes {
                id
                name
              }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "scienceRequirementsGroup" : {
          "nodes" : [
            {
              "scienceRequirements" : {
                "mode" : "SPECTROSCOPY",
                "spectroscopyRequirements" : {
                  "wavelength" : null
                }
              },
              "observationIds" : [
                "o-2",
                "o-3",
                "o-4",
                "o-5",
                "o-6",
                "o-7"
              ],
              "observations" : {
                "nodes" : [
                  {
                    "id" : "o-2",
                    "name" : "NGC 5949"
                  },
                  {
                    "id" : "o-3",
                    "name" : "NGC 3312"
                  },
                  {
                    "id" : "o-4",
                    "name" : "NGC 3312"
                  },
                  {
                    "id" : "o-5",
                    "name" : "NGC 3312"
                  },
                  {
                    "id" : "o-6",
                    "name" : "NGC 5949"
                  },
                  {
                    "id" : "o-7",
                    "name" : "Observation"
                  }
                ]
              }
            }
          ]
        }
      }
    """
  )

}