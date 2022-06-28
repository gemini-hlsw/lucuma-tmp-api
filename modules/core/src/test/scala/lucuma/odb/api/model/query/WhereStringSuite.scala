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

  import WhereStringInput.{EQ, NEQ, IN, NIN, LIKE, NLIKE}

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
    assert(IN("ox", "dogs", "gore", "kick").matches("gore"))
  }

  test("IN - empty") {
    assert(!IN().matches("gore"))
  }

  test("IN - case insensitive") {
    assert(IN("ox", "dogs", "gore", "kick").ignoreCase.matches("GORE"))
  }

  test("NIN") {
    assert(NIN("ox", "dogs", "gore", "kick").matches("fence"))
  }

  test("NIN - empty") {
    assert(NIN().matches("gore"))
  }

  test("EQ") {
    assert(EQ("ox").matches("ox"))
  }

  test("EQ - case insensitive") {
    assert(EQ("ox").ignoreCase.matches("oX"))
  }

  test("NEQ") {
    assert(NEQ("ox").matches("dogs"))
  }

  test("NEQ - case insensitive") {
    assert(!NEQ("ox").ignoreCase.matches("oX"))
  }
}
