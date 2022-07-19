// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import cats.syntax.option._
import io.circe.literal._

class ObservationUndeleteSuite extends OdbSuite {

  // Delete o-6
  queryTest(
    query ="""
      mutation DeleteObservations($deleteObservationsInput: DeleteObservationsInput!) {
        deleteObservations(input: $deleteObservationsInput) {
          observations {
            id
          }
        }
      }
    """,
    expected = json"""
      {
        "deleteObservations": {
          "observations": [
            {
              "id": "o-6"
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "deleteObservationsInput": {
          "WHERE": {
            "id": { "EQ": "o-6" }
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)
  )

  // Fetch all observations. o-6 is not included by default in the results
  // because it was deleted.
  queryTest(
    query ="""
      query AllObservations {
        observations(WHERE: { programId: { EQ: "p-2" } }) {
          matches {
            id
          }
        }
      }
    """,
    expected = json"""
      {
        "observations": {
          "matches": [
            {
              "id": "o-2"
            },
            {
              "id": "o-3"
            },
            {
              "id": "o-4"
            },
            {
              "id": "o-5"
            },
            {
              "id": "o-7"
            }
          ]
        }
      }
    """
  )

  // Fetch all observations again, but including deleted
  queryTest(
    query ="""
      query AllObservationsIncludeDeleted {
        observations(WHERE: { programId: { EQ: "p-2" } }, includeDeleted: true) {
          matches {
            id
          }
        }
      }
    """,
    expected = json"""
      {
        "observations": {
          "matches": [
            {
              "id": "o-2"
            },
            {
              "id": "o-3"
            },
            {
              "id": "o-4"
            },
            {
              "id": "o-5"
            },
            {
              "id": "o-6"
            },
            {
              "id": "o-7"
            }
          ]
        }
      }
    """
  )

  // undelete o-6
  queryTest(
    query ="""
      mutation UndeleteObservations($undeleteObservationsInput: UndeleteObservationsInput!) {
        undeleteObservations(input: $undeleteObservationsInput) {
          observations {
            id
          }
        }
      }
    """,
    expected = json"""
      {
        "undeleteObservations": {
          "observations": [
            {
              "id": "o-6"
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "undeleteObservationsInput": {
          "WHERE": {
            "id": { "EQ": "o-6" }
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)
  )

  // Fetch all observations and o-6 appears again.
  queryTest(
    query ="""
      query AllObservationsAgain {
        observations(WHERE: { programId: { EQ: "p-2" } }) {
          matches {
            id
          }
        }
      }
    """,
    expected = json"""
      {
        "observations": {
          "matches": [
            {
              "id": "o-2"
            },
            {
              "id": "o-3"
            },
            {
              "id": "o-4"
            },
            {
              "id": "o-5"
            },
            {
              "id": "o-6"
            },
            {
              "id": "o-7"
            }
          ]
        }
      }
    """
  )


}
