// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class TargetUndeleteSuite extends OdbSuite {

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

  // Delete NGC 3269 (t-3)
  queryTest(
    query ="""
      mutation DeleteTarget($deleteTargetInput: DeleteTargetsInput!) {
        deleteTargets(input: $deleteTargetInput) {
          targets {
            id
          }
        }
      }
    """,
    expected = json"""
      {
        "deleteTargets": {
          "targets": [
            {
              "id": "t-3"
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "deleteTargetInput": {
          "WHERE": {
            "id": { "EQ": "t-3" }
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)
  )

  // Fetch all science targets.  There is a t-6 but for p-3 so we skip it.
  // t-3 is not included by default in the results because it was deleted.
  queryTest(
    query ="""
      query AllTargets {
        targets(WHERE: { programId: { EQ: "p-2" } }) {
          matches {
            id
            name
          }
        }
      }
    """,
    expected = json"""
      {
        "targets": {
          "matches": [
            {
              "id": "t-2",
              "name": "NGC 5949"
            },
            {
              "id": "t-4",
              "name": "NGC 3312"
            },
            {
              "id": "t-5",
              "name": "NGC 4749"
            }
          ]
        }
      }
    """
  )

  // Fetch all science targets again, but make a point of including deleted
  // targets.
  queryTest(
    query ="""
      query AllTargetsIncludeDeleted {
        targets(WHERE: { programId: { EQ: "p-2" } }, includeDeleted: true) {
          matches {
            id
            name
          }
        }
      }
    """,
    expected = json"""
      {
        "targets": {
          "matches": [
            {
              "id": "t-2",
              "name": "NGC 5949"
            },
            {
              "id": "t-3",
              "name": "NGC 3269"
            },
            {
              "id": "t-4",
              "name": "NGC 3312"
            },
            {
              "id": "t-5",
              "name": "NGC 4749"
            }
          ]
        }
      }
    """
  )

  // Undelete NGC 3269 (t-3)
  queryTest(
    query ="""
      mutation UndeleteTargets($undeleteTargetsInput: UndeleteTargetsInput!) {
        undeleteTargets(input: $undeleteTargetsInput) {
          targets {
            id
          }
        }
      }
    """,
    expected = json"""
      {
        "undeleteTargets": {
          "targets": [
            {
              "id": "t-3"
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "undeleteTargetsInput": {
          "WHERE": {
            "id": { "EQ": "t-3" }
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)
  )

  // Fetch all science targets.  t-3 appears again.
  queryTest(
    query ="""
      query AllTargets {
        targets(WHERE: { programId: { EQ: "p-2" } }) {
          matches {
            id
            name
          }
        }
      }
    """,
    expected = json"""
      {
        "targets": {
          "matches": [
            {
              "id": "t-2",
              "name": "NGC 5949"
            },
            {
              "id": "t-3",
              "name": "NGC 3269"
            },
            {
              "id": "t-4",
              "name": "NGC 3312"
            },
            {
              "id": "t-5",
              "name": "NGC 4749"
            }
          ]
        }
      }
    """
  )
}
