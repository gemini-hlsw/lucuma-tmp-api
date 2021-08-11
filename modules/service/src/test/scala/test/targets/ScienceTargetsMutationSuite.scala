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
      mutation UpdateScienceTargets($listEdit: BulkEditScienceTargetsInput!) {
        updateScienceTargets(input: $listEdit) {
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
        "updateScienceTargets": [
          {
            "id": "o-3",
            "targets": {
              "science": [
                {
                  "name": "NGC 1704"
                },
                {
                  "name": "NGC 1705"
                }
              ]
            }
          }
        ]
      }
    """,
    variables =json"""
      {
        "listEdit": {
          "selectObservations": [ "o-3" ],
          "edit": {
            "replaceList": [
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
      }
    """.some

  )

  // Same name twice in replace list
  queryTestFailure(
    query ="""
      mutation UpdateScienceTargets($listEdit: BulkEditScienceTargetsInput!) {
        updateScienceTargets(input: $listEdit) {
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
      "Cannot create a new science target with name 'NGC 1704' because one already exists in observation o-4"
    ),
    variables =json"""
      {
        "listEdit": {
          "selectObservations": [ "o-4" ],
          "edit": {
            "replaceList": [
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
      }
    """.some
  )

  // Delete NGC 3312 then add NGC 1704 (could also have been a `replaceList`)
  queryTest(
    query ="""
      mutation UpdateScienceTargets($listEdit: BulkEditScienceTargetsInput!) {
        updateScienceTargets(input: $listEdit) {
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
        "updateScienceTargets": [
          {
            "id": "o-5",
            "targets": {
              "science": [
                {
                  "name": "NGC 1704"
                }
              ]
            }
          }
        ]
      }
    """,
    variables =json"""
      {
        "listEdit": {
          "selectObservations": [ "o-5" ],
          "edit": {
            "editList": [
              {
                "delete": "NGC 3312"
              },
              {
                "add": {
                  "sidereal": {
                    "name": "NGC 1704",
                    "ra": {
                      "hms": "04:49:54.960"
                    },
                    "dec": {
                      "dms": "-69:45:18.00"
                    }
                  }
                }
              }
            ]
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)

  )

  // Edit all targets in o-6
  queryTest(
    query ="""
      mutation UpdateScienceTargets($listEdit: BulkEditScienceTargetsInput!) {
        updateScienceTargets(input: $listEdit) {
          id
          targets {
            science {
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
        "updateScienceTargets": [
          {
            "id": "o-6",
            "targets": {
              "science": [
                {
                  "name": "NGC 3269",
                  "tracking": {
                    "coordinates": {
                      "ra": {
                        "hms": "01:00:00.000000"
                      }
                    }
                  }
                },
                {
                  "name": "NGC 3312",
                  "tracking": {
                    "coordinates": {
                      "ra": {
                        "hms": "02:00:00.000000"
                      }
                    }
                  }
                },
                {
                  "name": "NGC 5949",
                  "tracking": {
                    "coordinates": {
                      "ra": {
                        "hms": "03:00:00.000000"
                      }
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
        "listEdit": {
          "selectObservations": [ "o-6" ],
          "edit": {
            "editList": [
              {
                "edit": {
                  "selectTarget": "NGC 5949",
                  "sidereal": {
                    "ra": {
                      "hms": "03:00:00.00"
                    }
                  }
                }
              },
              {
                "edit": {
                  "selectTarget": "NGC 3312",
                  "sidereal": {
                    "ra": {
                      "hms": "02:00:00.00"
                    }
                  }
                }
              },
              {
                "edit": {
                  "selectTarget": "NGC 3269",
                  "sidereal": {
                    "ra": {
                      "hms": "01:00:00.00"
                    }
                  }
                }
              }
            ]
          }
        }
      }
    """.some

  )


}
