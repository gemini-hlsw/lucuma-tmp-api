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
}
