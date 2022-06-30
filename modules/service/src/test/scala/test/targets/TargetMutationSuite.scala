// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class TargetMutationSuite extends OdbSuite {

  // Edit NGC 3312 to remove parallax altogether.
  queryTest(
    query = """
      mutation UpdateScienceTarget($updateTargets: UpdateTargetsInput!) {
        updateTargets(input: $updateTargets) {
          targets {
            id
            name
            sidereal {
              parallax { microarcseconds }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "updateTargets": {
          "targets": [
            {
              "id": "t-4",
              "name": "NGC 3312",
              "sidereal": {
                "parallax": null
              }
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "updateTargets": {
          "SET": {
            "sidereal": {
              "parallax": null
            }
          },
          "WHERE": {
            "id": { "EQ": "t-4" }
          }
        }
      }
    """.some
  )

  // Delete a target by id.  No need to specify a target environment.
  queryTest(
    query = """
      mutation DeleteTarget($deleteTargetInput: DeleteTargetsInput!) {
        deleteTargets(input: $deleteTargetInput) {
          targets {
            id
            name
            existence
          }
        }
      }
    """,
    expected = json"""
      {
        "deleteTargets": {
          "targets": [
            {
              "id": "t-4",
              "name": "NGC 3312",
              "existence": "DELETED"
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "deleteTargetInput": {
          "WHERE": {
            "id": { "EQ": "t-4" }
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)  // cannot run this test twice since it changes required state
  )

  // Clone an existing deleted, target.
  queryTest(
    query ="""
      mutation CloneTarget($cloneInput: CloneTargetInput!) {
        cloneTarget(input: $cloneInput) {
          newTarget {
            id
            name
            existence
          }
        }
      }
    """,
    expected =json"""
      {
        "cloneTarget": {
          "newTarget": {
            "id": "t-7",
            "name": "NGC 3312",
            "existence": "PRESENT"
          }
        }
      }
    """,
    variables = json"""
      {
        "cloneInput": {
          "targetId": "t-4"
        }
      }
    """.some,
    clients = List(ClientOption.Http)
  )

  // Create a new non-sidereal target.
  queryTest(
    query = """
      mutation CreateTarget($createTargetInput: CreateTargetInput!) {
        createTarget(input: $createTargetInput) {
          target {
            id
          }
        }
      }
    """,
    expected = json"""
      {
        "createTarget": {
          "target": {
            "id": "t-8"
          }
        }
      }
    """,
    variables = json"""
      {
        "createTargetInput": {
          "programId": "p-2",
          "SET": {
            "name": "Mars",
            "nonsidereal": {
              "keyType": "MAJOR_BODY",
              "des": "499"
            },
            "sourceProfile": {
              "point": {
                "bandNormalized": {
                  "sed": {
                    "planet": "MARS"
                  },
                  "brightnesses": [
                    {
                      "band": "V",
                      "value": 10,
                      "units": "VEGA_MAGNITUDE"
                    }
                  ]
                }
              }
            }
          }
        }
      }
    """.some
  )

  // Now do an multi-target edit which should fail for the nonsidereal target
  queryTestFailure(
    query = """
      mutation MultiTargetEdit($updateTargetsInput: UpdateTargetsInput!) {
        updateTargets(input: $updateTargetsInput) {
          hasMore
        }
      }
    """,
    errors = List(
      "target t-8: No sidereal 'ra' definition provided",
      "target t-8: No sidereal 'dec' definition provided"
    ),
    variables = json"""
      {
        "updateTargetsInput": {
          "SET": {
            "sidereal": {
              "parallax": {
                "microarcseconds": 1
              }
            }
          },
          "WHERE": {
            "id": { "IN": [ "t-7", "t-8" ] }
          }
        }
      }
    """.some
  )

  // Target t-7 wasn't modified.
  queryTest(
    query = """
      query ParallaxNotChanged {
        target(targetId: "t-7") {
          sidereal {
            parallax {
              microarcseconds
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "target": {
          "sidereal": {
            "parallax": null
          }
        }
      }
    """
  )

}
