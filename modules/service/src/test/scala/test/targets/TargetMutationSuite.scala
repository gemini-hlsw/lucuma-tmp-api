// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
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
        updateTarget(input: $targetEdit) {
          id
          name
          tracking {
            ... on Sidereal {
              parallax { microarcseconds }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "updateTarget": {
          "id": "t-4",
          "name": "NGC 3312",
          "tracking": {
            "parallax": null
          }
        }
      }
    """,
    variables = json"""
      {
        "targetEdit": {
          "targetId": "t-4",
          "sidereal": {
            "parallax": null
          }
        }
      }
    """.some
  )

  // Delete a target by id.  No need to specify a target environment.
  queryTest(
    query = """
      mutation DeleteTarget {
        deleteTarget(targetId: "t-4") {
          id
          name
          existence
        }
      }
    """,
    expected = json"""
      {
        "deleteTarget": {
          "id": "t-4",
          "name": "NGC 3312",
          "existence": "DELETED"
        }
      }
    """,
    None,
    clients = List(ClientOption.Http)  // cannot run this test twice since it changes required state
  )

  // Clone an existing deleted, target.
  queryTest(
    query ="""
      mutation CloneTarget {
        cloneTarget(existingTargetId: "t-4", suggestedCloneId: "t-abc") {
          id
          name
          existence
        }
      }
    """,
    expected =json"""
      {
        "cloneTarget": {
          "id": "t-abc",
          "name": "NGC 3312",
          "existence": "PRESENT"
        }
      }
    """,
    None,
    clients = List(ClientOption.Http)
  )
}
