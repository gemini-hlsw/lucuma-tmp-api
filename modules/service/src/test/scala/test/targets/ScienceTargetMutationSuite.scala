// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class ScienceTargetMutationSuite extends OdbSuite {

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

  // Bulk edit NGC 3312 to remove parallax altogether.
  queryTest(
    query = """
      mutation UpdateScienceTarget($targetEdit: BulkEditTargetInput!) {
        updateScienceTarget(input: $targetEdit) {
          observation {
            id
          }
          edits {
            op
            target {
              name
              tracking {
                ... on Sidereal {
                  parallax { microarcseconds }
                }
              }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "updateScienceTarget": [
          {
            "observation": {
              "id": "o-3"
            },
            "edits": [
              {
                "op": "EDIT",
                "target": {
                  "name": "NGC 3312",
                  "tracking": {
                    "parallax": null
                  }
                }
              }
            ]
          },
          {
            "observation": {
              "id": "o-4"
            },
            "edits": [
              {
                "op": "EDIT",
                "target": {
                  "name": "NGC 3312",
                  "tracking": {
                    "parallax": null
                  }
                }
              }
            ]
          },
          {
            "observation": {
              "id": "o-5"
            },
            "edits": [
              {
                "op": "EDIT",
                "target": {
                  "name": "NGC 3312",
                  "tracking": {
                    "parallax": null
                  }
                }
              }
            ]
          },
          {
            "observation": {
              "id": "o-6"
            },
            "edits": [
              {
                "op": "EDIT",
                "target": {
                  "name": "NGC 3312",
                  "tracking": {
                    "parallax": null
                  }
                }
              }
            ]
          }
        ]
      }
    """,
    variables = json"""
      {
        "targetEdit": {
          "select": {
            "observations": [ "o-3", "o-4", "o-5", "o-6" ]
          },
          "editSidereal": {
            "select": {
              "names": [ "NGC 3312" ]
            },
            "parallax": null
          }
        }
      }
    """.some
  )

  // Attempt to edit a target by name without specifying the environment
  queryTestFailure(
    query = """
      mutation UpdateScienceTarget($targetEdit: BulkEditTargetInput!) {
        updateScienceTarget(input: $targetEdit) {
          edits {
            op
            target {
              name
              tracking {
                ... on Sidereal {
                  parallax { microarcseconds }
                }
              }
            }
          }
        }
      }
    """,
    errors = List(
      "No target environment was selected: specify a target environment when identifying targets by name."
    ),
    variables = json"""
      {
        "targetEdit": {
          "editSidereal": {
            "select": {
               "names": [ "NGC 3312" ]
            },
            "parallax": null
          }
        }
      }
    """.some
  )

  // Delete a target by id.  No need to specify a target environment.
  queryTest(
    query = """
      mutation UpdateScienceTarget($targetEdit: BulkEditTargetInput!) {
        updateScienceTarget(input: $targetEdit) {
          observation {
            id
          }
          edits {
            op
            target {
              name
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "updateScienceTarget": [
          {
            "observation": {
              "id": "o-3"
            },
            "edits": [
              {
                "op": "DELETE",
                "target": {
                  "name": "NGC 3312"
                }
              }
            ]
          }
        ]
      }
    """,
    variables = json"""
      {
        "targetEdit": {
          "delete": {
            "targetIds": [ "t-3" ]
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)  // cannot run this test twice since it changes required state

  )

}
