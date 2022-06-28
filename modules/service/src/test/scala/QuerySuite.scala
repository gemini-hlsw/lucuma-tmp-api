// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import io.circe.literal._

class QuerySuite extends OdbSuite {

  queryTest(
    query = """
      query Programs {
        programs(WHERE: { id: { IN: ["p-2", "p-3", "p-4"] } } ) {
          matches {
            id
            name
            proposal {
              title
              proposalClass {
                __typename
                minPercentTime
                ... on LargeProgram {
                  minPercentTotalTime
                  totalTime {
                    seconds
                  }
                }
                ... on Intensive {
                  minPercentTotalTime
                  totalTime {
                    seconds
                  }
                }
              }
              category
              toOActivation
              abstract
              partnerSplits {
                partner
                percent
              }
            }
          }
        }
      }

    """,
    expected = json"""
      {
        "programs" : {
          "matches": [
            {
              "id" : "p-2",
              "name" : "The real dark matter was the friends we made along the way",
              "proposal": {
                "title": "Proposal title",
                "proposalClass": {
                  "__typename": "Classical",
                  "minPercentTime": 80
                },
                "category": "SMALL_BODIES",
                "toOActivation": "NONE",
                "abstract": "Totally abstract",
                "partnerSplits": [
                  {
                    "partner": "CL",
                    "percent": 60
                  },
                  {
                    "partner": "UH",
                    "percent": 40
                  }
                ]
              }
            },
            {
              "id" : "p-3",
              "name" : "An Empty Placeholder Program",
              "proposal": null
            }
          ]
        }
      }
    """
  )

  queryTest(
    query = """
      query Observations {
        observations(WHERE: { programId: { EQ: "p-2" } }) {
          matches {
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
          "matches" : [
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
          matches {
            constraintSet {
              cloudExtinction
              imageQuality
              skyBackground
              waterVapor
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
        "constraintSetGroup" : {
          "matches" : [
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
                "matches" : [
                  {
                    "id" : "o-2",
                    "title" : "NGC 5949"
                  },
                  {
                    "id" : "o-3",
                    "title" : "NGC 3312"
                  },
                  {
                    "id" : "o-4",
                    "title" : "NGC 3312"
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
          matches {
            scienceRequirements {
              mode
              spectroscopy {
                wavelength { nanometers }
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
        "scienceRequirementsGroup" : {
          "matches" : [
            {
              "scienceRequirements" : {
                "mode" : "SPECTROSCOPY",
                "spectroscopy" : {
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
                "matches" : [
                  {
                    "id" : "o-2",
                    "title" : "NGC 5949"
                  },
                  {
                    "id" : "o-3",
                    "title" : "NGC 3312"
                  },
                  {
                    "id" : "o-4",
                    "title" : "NGC 3312"
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
            }
          ]
        }
      }
    """
  )

}
