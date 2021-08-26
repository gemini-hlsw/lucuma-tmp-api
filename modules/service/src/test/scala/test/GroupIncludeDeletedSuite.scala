// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import io.circe.literal._

class GroupIncludeDeletedSuite extends OdbSuite {

  queryTest(
    query ="""
      mutation DeleteObservation {
        deleteObservation(observationId: "o-5") {
          id
        }
      }
    """,
    expected = json"""
      {
        "deleteObservation": {
          "id": "o-5"
        }
      }
    """
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
