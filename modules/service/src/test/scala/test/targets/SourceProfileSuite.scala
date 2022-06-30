// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class SourceProfileSuite extends OdbSuite {

  // Replace all magnitudes with one U entry
  queryTest(
    query     = """
      mutation EditMagnitude($targetsUpdate: UpdateTargetsInput!) {
        updateTargets(input: $targetsUpdate) {
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
        "updateTargets": {
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
        "targetsUpdate": {
          "SET": {
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
          },
          "WHERE": {
            "id": { "EQ": "t-2" }
          }
        }
      }
    """.some,
    clients   = List(ClientOption.Http)
  )

}
