// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import io.circe.Json
import io.circe.literal._

class ObservationEditSubscriptionSuite extends OdbSuite {

  // Subscribe to observation updates
  val query: String = """
    subscription ObservationEdit {
      observationEdit(programId: "p-2") {
        value { id subtitle }
      }
    }
  """

  // A parameterized mutation for observations
  val mutation: String = """
    mutation UpdateObservation($editObservation: UpdateObservationsInput!) {
      updateObservations(input: $editObservation) { observations { id } }
    }
  """

  // When we mutate we will change the target name three times
  val newNames: List[String] =
    List("foo", "bar", "baz")

  // Given a name, construct an EditObservationInput that updates observation `o-2`'s name
  def arg(newName: String): Json =
    json"""
      {
        "editObservation": {
          "SET": {
            "subtitle": $newName
          },
          "WHERE": {
            "id": { "EQ": "o-2" }
          }
        }
      }
    """

  // Our mutations are pairs of (query, optional args)
  val mutations: List[(String, Option[Json])] =
    newNames.map { s => (mutation, Some(arg(s))) }

  // An expected event we should get back after changing `t-2`'s name
  def expect(newName: String) =
    json"""
      {
        "observationEdit" : {
          "value" : {
            "id" : "o-2",
            "subtitle" : $newName
          }
        }
      }
    """

  // Our expected events, in order.
  def expectedEvents: List[Json] =
    newNames.map(expect)

  // Run the test!
  subscriptionTest(
    query,
    Left(mutations),
    expectedEvents
  )

}