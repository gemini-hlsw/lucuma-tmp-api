// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class ScienceTargetsMutationSuite extends OdbSuite {

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

  // Replace all targets in o-3
  queryTest(
    query ="""
      mutation ReplaceScienceTargets($listEdit: BulkReplaceTargetListInput!) {
        replaceScienceTargetList(input: $listEdit) {
          edits {
            op
            target {
              name
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "replaceScienceTargetList": [
          {
            "edits": [
              {
                "op": "DELETE",
                "target": {
                  "name": "NGC 3312"
                }
              },
              {
                "op": "CREATE",
                "target": {
                  "name": "NGC 1704"
                }
              },
              {
                "op": "CREATE",
                "target": {
                  "name": "NGC 1705"
                }
              }
            ]
          }
        ]
      }
    """,
    variables =json"""
      {
        "listEdit": {
          "select": {
            "observations": [ "o-3" ]
          },
          "replace": [
            {
              "sidereal": {
                "name": "NGC 1704",
                "ra": {
                  "hms": "04:49:54.960"
                },
                "dec": {
                  "dms": "-69:45:18.00"
                }
              }
            },
            {
              "sidereal": {
                "name": "NGC 1705",
                "ra": {
                  "hms": "04:54:13.500"
                },
                "dec": {
                  "dms": "-53:21:39.82"
                },
                "radialVelocity": {
                  "kilometersPerSecond": 632.493
                }
              }
            }
          ]
        }
      }
    """.some,
    clients = List(ClientOption.Http)  // cannot run this test twice since it changes required state

  )

  // Delete NGC 3312 then add NGC 1704 (could also have been a `replaceList`).
  queryTest(
    query ="""
      mutation UpdateScienceTargets($listEdit: BulkEditTargetListInput!) {
        updateScienceTargetList(input: $listEdit) {
          edits {
            op
            target {
              name
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "updateScienceTargetList": [
          {
            "edits": [
              {
                "op": "DELETE",
                "target": {
                  "name": "NGC 3312"
                }
              },
              {
                "op": "CREATE",
                "target": {
                  "name": "NGC 1704"
                }
              }
            ]
          }
        ]
      }
    """,
    variables =json"""
      {
        "listEdit": {
          "select": {
            "observations": [ "o-5" ]
          },
          "edits": [
            {
              "delete": {
                "names": [ "NGC 3312" ]
              }
            },
            {
              "addSidereal": {
                "name": "NGC 1704",
                "ra": {
                  "hms": "04:49:54.960"
                },
                "dec": {
                  "dms": "-69:45:18.00"
                }
              }
            }
          ]
        }
      }
    """.some,
    clients = List(ClientOption.Http)

  )

  // Edit all targets in o-6
  queryTest(
    query ="""
      mutation UpdateScienceTargets($listEdit: BulkEditTargetListInput!) {
        updateScienceTargetList(input: $listEdit) {
          edits {
            op
            target {
              name
              tracking {
                ... on Sidereal {
                  coordinates {
                    ra {
                      hms
                    }
                  }
                }
              }
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "updateScienceTargetList": [
          {
            "edits": [
              {
                "op": "EDIT",
                "target": {
                  "name": "NGC 5949",
                  "tracking": {
                    "coordinates": {
                      "ra": {
                        "hms": "01:00:00.000000"
                      }
                    }
                  }
                }
              },
              {
                "op": "EDIT",
                "target": {
                  "name": "NGC 3269",
                  "tracking": {
                    "coordinates": {
                      "ra": {
                        "hms": "02:00:00.000000"
                      }
                    }
                  }
                }
              },
              {
                "op": "EDIT",
                "target": {
                  "name": "NGC 3312",
                  "tracking": {
                    "coordinates": {
                      "ra": {
                        "hms": "03:00:00.000000"
                      }
                    }
                  }
                }
              }
            ]
          }
        ]
      }
    """,
    variables =json"""
      {
        "listEdit": {
          "select": {
            "observations": [ "o-6" ]
          },
          "edits": [
            {
              "editSidereal": {
                "select": {
                  "names": [ "NGC 5949"]
                },
                "ra": {
                  "hms": "01:00:00.00"
                }
              }
            },
            {
              "editSidereal": {
                "select": {
                  "names": [ "NGC 3269" ]
                },
                "ra": {
                  "hms": "02:00:00.00"
                }
              }
            },
            {
              "editSidereal": {
                "select": {
                  "names": [ "NGC 3312" ]
                },
                "ra": {
                  "hms": "03:00:00.00"
                }
              }
            }
          ]
        }
      }
    """.some

  )

}
