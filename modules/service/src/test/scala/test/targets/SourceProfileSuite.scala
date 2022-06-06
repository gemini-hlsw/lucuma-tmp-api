// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class SourceProfileSuite extends OdbSuite {

  // J value set to 42
  // H error set to null
  // Add new U entry
  queryTest(
    query     = """
      mutation EditMagnitude($targetEdit: EditTargetInput!) {
        editTargets(input: $targetEdit) {
          targets {
            id
            name
            sourceProfile {
              point {
                bandNormalized {
                  brightnesses {
                    band
                    value
                    units
                    error
                  }
                }
              }
            }
          }
        }
      }
    """,
    expected  = json"""
      {
        "editTargets": {
          "targets": [
            {
              "id": "t-3",
              "name": "NGC 3269",
              "sourceProfile": {
                "point": {
                  "bandNormalized": {
                    "brightnesses": [
                      {
                        "band": "U",
                        "value": 10,
                        "units": "VEGA_MAGNITUDE",
                        "error": null
                      },
                      {
                        "band": "B",
                        "value": 13.240,
                        "units": "VEGA_MAGNITUDE",
                        "error": null
                      },
                      {
                        "band": "V",
                        "value": 13.510,
                        "units": "VEGA_MAGNITUDE",
                        "error": null
                      },
                      {
                        "band": "R",
                        "value": 11.730,
                        "units": "VEGA_MAGNITUDE",
                        "error": null
                      },
                      {
                        "band": "J",
                        "value": 42,
                        "units": "VEGA_MAGNITUDE",
                        "error": 0.018
                      },
                      {
                        "band": "H",
                        "value": 9.387,
                        "units": "VEGA_MAGNITUDE",
                        "error": null
                      },
                      {
                        "band": "K",
                        "value": 9.055,
                        "units": "VEGA_MAGNITUDE",
                        "error": 0.031
                      }
                    ]
                  }
                }
              }
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "targetEdit": {
          "select": {
            "targetIds": [ "t-3" ]
          },
          "patch": {
            "sourceProfile": {
              "point": {
                "bandNormalized": {
                  "editBrightnesses": [
                    {
                      "band": "J",
                      "value": 42.0
                    },
                    {
                      "band": "H",
                      "error": null
                    },
                    {
                      "band": "U",
                      "value": 10.0,
                      "units": "VEGA_MAGNITUDE"
                    }
                  ]
                }
              }
            }
          }
        }
      }
    """.some,
    clients   = List(ClientOption.Http)
  )

  // Replace all magnitudes with one U entry
  queryTest(
    query     = """
      mutation EditMagnitude($targetEdit: EditTargetInput!) {
        editTargets(input: $targetEdit) {
          targets {
            id
            name
            sourceProfile {
              point {
                bandNormalized {
                  brightnesses {
                    band
                    value
                    units
                    error
                  }
                }
              }
            }
          }
        }
      }
    """,
    expected  = json"""
      {
        "editTargets": {
          "targets": [
            {
              "id": "t-2",
              "name": "NGC 5949",
              "sourceProfile": {
                "point": {
                  "bandNormalized": {
                    "brightnesses": [
                      {
                        "band": "U",
                        "value": 10,
                        "units": "VEGA_MAGNITUDE",
                        "error": null
                      }
                    ]
                  }
                }
              }
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "targetEdit": {
          "select": {
            "targetIds": [ "t-2" ]
          },
          "patch": {
            "sourceProfile": {
              "point": {
                "bandNormalized": {
                  "brightnesses": [
                    {
                      "band": "U",
                      "value": 10.0,
                      "units": "VEGA_MAGNITUDE"
                    }
                  ]
                }
              }
            }
          }
        }
      }
    """.some,
    clients   = List(ClientOption.Http)
  )

  // Delete all but V, set its error to 10
  queryTest(
    query     = """
      mutation EditMagnitude($targetEdit: EditTargetInput!) {
        editTargets(input: $targetEdit) {
          targets {
            id
            name
            sourceProfile {
              point {
                bandNormalized {
                  brightnesses {
                    band
                    value
                    units
                    error
                  }
                }
              }
            }
          }
        }
      }
    """,
    expected  = json"""
      {
        "editTargets": {
          "targets": [
            {
              "id": "t-4",
              "name": "NGC 3312",
              "sourceProfile": {
                "point": {
                  "bandNormalized": {
                    "brightnesses": [
                      {
                        "band": "V",
                        "value": 13.960,
                        "units": "VEGA_MAGNITUDE",
                        "error": 10
                      }
                    ]
                  }
                }
              }
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "targetEdit": {
          "select": {
            "targetIds": [ "t-4" ]
          },
          "patch": {
            "sourceProfile": {
              "point": {
                "bandNormalized": {
                  "editBrightnesses": [
                    {
                      "band": "V",
                      "error": 10.0
                    }
                  ],
                  "deleteBrightnesses": [ "B", "J", "H", "K" ]
                }
              }
            }
          }
        }
      }
    """.some,
    clients   = List(ClientOption.Http)
  )

}
