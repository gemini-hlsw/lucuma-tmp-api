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
      mutation UpdateScienceTarget($targetEdit: EditTargetInput!) {
        editTarget(input: $targetEdit) {
          id
          name
          sidereal {
            parallax { microarcseconds }
          }
        }
      }
    """,
    expected = json"""
      {
        "editTarget": [
          {
            "id": "t-4",
            "name": "NGC 3312",
            "sidereal": {
              "parallax": null
            }
          }
        ]
      }
    """,
    variables = json"""
      {
        "targetEdit": {
          "select": {
            "targetIds": [ "t-4" ]
          },
          "patch": {
            "sidereal": {
              "parallax": null
            }
          }
        }
      }
    """.some
  )

  // Delete a target by id.  No need to specify a target environment.
  queryTest(
    query = """
      mutation DeleteTarget($deleteTargetInput: DeleteTargetInput!) {
        deleteTarget(input: $deleteTargetInput) {
          id
          name
          existence
        }
      }
    """,
    expected = json"""
      {
        "deleteTarget": [
          {
            "id": "t-4",
            "name": "NGC 3312",
            "existence": "DELETED"
          }
        ]
      }
    """,
    variables = json"""
      {
        "deleteTargetInput": {
          "select": {
            "targetIds": [ "t-4" ]
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
