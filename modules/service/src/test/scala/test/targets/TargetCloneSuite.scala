// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import io.circe.literal._

class TargetCloneSuite extends OdbSuite {

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

  // Clone an existing target and replace it in observations 3 and 4
  queryTest(
    query ="""
      mutation CloneAndReplaceTarget {
        cloneTarget(existingTargetId: "t-4", suggestedCloneId: "t-a", observationIds: [ "o-3", "o-4" ]) {
          id
          name
        }
      }
    """,
    expected =json"""
      {
        "cloneTarget": {
          "id": "t-a",
          "name": "NGC 3312"
        }
      }
    """,
    None,
    clients = List(ClientOption.Http)
  )
}
