// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import io.circe.Json
import io.circe.literal._

class ProgramTargetEditSubscriptionSuite extends OdbSuite {

  // Subscribe to target updates
  val query: String = """
    subscription ProgramTargetEdit {
      targetEdit(programId: "p-2") {
        value { id name }
      }
    }
  """

  // A parameterized mutation for targets
  val mutation: String = """
    mutation UpdateSiderealTarget($editSidereal: EditSiderealInput!) {
      updateSiderealTarget(input: $editSidereal) { id }
    }
  """

  // When we mutate we will change the target name three times
  val newNames: List[String] =
    List("foo", "bar", "baz")

  // Given a name, construct an EditSiderealInput that updates target `t-2`'s name
  def arg(newName: String): Json =
    json"""
      {
        "editSidereal": {
          "targetId": "t-2",
          "name": $newName
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
        "targetEdit" : {
          "value" : {
            "id" : "t-2",
            "name" : $newName
          }
        }
      }
    """

  // Our expectated events, in order.
  def expectedEvents: List[Json] =
    newNames.map(expect)

  // Run the test!
  subscriptionTest(
    query,
    Left(mutations),
    expectedEvents
  )

}