// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.tests.CatsSuite
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import org.scalatest.Assertion

final class WhereStringSuite extends CatsSuite  {

  val s: String =
    raw"Call no council of war. _ %([^+?*])% It is proverbial that councils of war never fight."

  private def LIKE(s: NonEmptyString): WhereStringInput =
    WhereStringInput.LIKE(s)

  private def NLIKE(s: NonEmptyString): WhereStringInput =
    WhereStringInput.NLIKE(s)

  private def matches(where: WhereStringInput): Assertion =
    assert(where.matches(s))

  test("LIKE % at start") {
    matches(LIKE("%Call no council%"))
  }

  test("LIKE % in middle") {
    matches(LIKE("%It is proverbial%"))
  }

  test("LIKE % at end") {
    matches(LIKE("%never fight.%"))
  }

  test("LIKE case insensitive") {
    matches(LIKE("%NEVER fight%").ignoreCase)
  }

  test("LIKE _ at start") {
    matches(LIKE("_all no council%"))
  }

  test("LIKE _ in middle") {
    matches(LIKE("%It __ proverbial%"))
  }

  test("LIKE _ at end") {
    matches(LIKE("%never fight_"))
  }

  test("NLIKE") {
    matches(NLIKE("PROVERBIAL"))
  }

  test("NLIKE - but for case") {
    matches(NLIKE("%PROVERBIAL%"))
  }

  test("LIKE - match regex symbols literally") {
    matches(LIKE("%^+?%"))
  }

  test("LIKE - escape % symbols") {
    matches(LIKE(NonEmptyString.unsafeFrom(raw"%\%(%")))
  }

  test("LIKE - escape all wildcard symbols") {
    matches(LIKE(NonEmptyString.unsafeFrom(raw"Call no council of war. \_ \%([^+?*])\% It is proverbial that councils of war never fight.")))
  }

  test("IN") {
    assert(WhereStringInput.IN("ox", "dogs", "gore", "kick").matches("gore"))
  }

  test("IN - empty") {
    assert(!WhereStringInput.IN().matches("gore"))
  }

  test("IN - case insensitive") {
    assert(WhereStringInput.IN("ox", "dogs", "gore", "kick").ignoreCase.matches("GORE"))
  }

  test("NIN") {
    assert(WhereStringInput.NIN("ox", "dogs", "gore", "kick").matches("fence"))
  }

  test("NIN - empty") {
    assert(WhereStringInput.NIN().matches("gore"))
  }

  test("EQ") {
    assert(WhereStringInput.EQ("ox").matches("ox"))
  }

  test("EQ - case insensitive") {
    assert(WhereStringInput.EQ("ox").ignoreCase.matches("oX"))
  }

  test("NEQ") {
    assert(WhereStringInput.NEQ("ox").matches("dogs"))
  }

  test("NEQ - case insensitive") {
    assert(!WhereStringInput.NEQ("ox").ignoreCase.matches("oX"))
  }
}
