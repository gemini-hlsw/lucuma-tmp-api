// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import cats.syntax.option._
import io.circe.literal._

class GroupIncludeDeletedSuite extends OdbSuite {

  queryTest(
    query ="""
      mutation DeleteObservation($deleteObservationInput: DeleteObservationInput!) {
        deleteObservations(input: $deleteObservationInput) {
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
              "id": "o-5"
            }
          ]
        }
      }
    """,
    variables =json"""
      {
        "deleteObservationInput": {
          "select": {
            "observationIds": [ "o-5" ]
          }
        }
      }
    """.some
  )

  // After deletion of "o-5", the grouping will not contain it
  queryTest(
    query = """
      query ObservationsByConstraintSet {
        constraintSetGroup(programId:"p-2", includeDeleted: false) {
          nodes {
            observationIds
          }
        }
      }
    """,
    expected = json"""
      {
        "constraintSetGroup" : {
          "nodes" : [
            {
              "observationIds" : [
                "o-2",
                "o-3",
                "o-4",
                "o-6",
                "o-7"
              ]
            }
          ]
        }
      }
    """
  )

  // After deletion of "o-5", the grouping will contain it if includeDeleted is true
  queryTest(
    query = """
      query ObservationsByConstraintSet {
        constraintSetGroup(programId:"p-2", includeDeleted: true) {
          nodes {
            observationIds
          }
        }
      }
    """,
    expected = json"""
      {
        "constraintSetGroup" : {
          "nodes" : [
            {
              "observationIds" : [
                "o-2",
                "o-3",
                "o-4",
                "o-5",
                "o-6",
                "o-7"
              ]
            }
          ]
        }
      }
    """
  )
}
