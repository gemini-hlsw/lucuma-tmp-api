// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import cats.syntax.option._
import io.circe.literal._

class ProgramMutationSuite extends OdbSuite {
  queryTest(
    query = """
      mutation CreateEmptyProgram($programCreate: CreateProgramInput!) {
        createProgram(input: $programCreate)
    """ + programQuery("program") + "}",
    expected = json"""
      {
        "createProgram": {
          "program": {
            "id": "p-4",
            "name": null,
            "existence": "PRESENT",
            "proposal": null
          }
        }
      }
    """,
    variables = json"""
      {
        "programCreate": {
          "properties": {}
        }
      }
    """.some
  )

  queryTestFailure(
    query = """
      mutation UpdateProgramNewProposalError($updatePrograms: UpdateProgramsInput!) {
        updatePrograms(input: $updatePrograms)
    """ + programQuery("programs") + "}",
    errors = List(
      "No minPercentTime definition provided",
      "No totalTime definition provided",
      "No toOActivation definition provided",
      "No partnerSplits definition provided"
    ),
    variables = json"""
      {
        "updatePrograms": {
          "SET": {
            "name": "Jack",
            "proposal": {
              "title": "Classy Proposal",
              "proposalClass": {
                "largeProgram": {
                  "minPercentTotalTime": 88
                }
              }
            }
          },
          "WHERE": { "id": { "EQ": "p-3" } }
        }
      }
    """.some
  )

  queryTest(
    query = """
      mutation UpdateProgramNewProposal($updatePrograms: UpdateProgramsInput!) {
        updatePrograms(input: $updatePrograms)
    """ + programQuery("programs") + "}",
    expected = json"""
      {
        "updatePrograms": {
          "programs": [
            {
              "id": "p-3",
              "name": "Jack",
              "existence": "PRESENT",
              "proposal": {
                "title": "Classy Proposal",
                "proposalClass": {
                  "__typename": "LargeProgram",
                  "minPercentTime": 77,
                  "minPercentTotalTime": 88,
                  "totalTime": {
                    "seconds": 660.000000
                  }
                },
                "category": null,
                "toOActivation": "STANDARD",
                "abstract": null,
                "partnerSplits": []
              }
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "updatePrograms": {
          "SET": {
            "name": "Jack",
            "proposal": {
              "title": "Classy Proposal",
              "proposalClass": {
                "largeProgram": {
                  "minPercentTime": 77,
                  "minPercentTotalTime": 88,
                  "totalTime": { "minutes": 11 }
                }
              },
              "toOActivation": "STANDARD",
              "partnerSplits": []
            }
          },
          "WHERE": { "id": { "EQ": "p-3" } }
        }
      }
    """.some
  )

  queryTest(
    query = """
      mutation UpdateProgramExisting($updatePrograms: UpdateProgramsInput!) {
        updatePrograms(input: $updatePrograms)
    """ + programQuery("programs") + "}",
    expected = json"""
      {
        "updatePrograms": {
          "programs": [
            {
              "id": "p-3",
              "name": "Jack",
              "existence": "PRESENT",
              "proposal": {
                "title": "Classy Proposal",
                "proposalClass": {
                  "__typename": "LargeProgram",
                  "minPercentTime": 77,
                  "minPercentTotalTime": 96,
                  "totalTime": {
                    "seconds": 660.000000
                  }
                },
                "category": null,
                "toOActivation": "STANDARD",
                "abstract": null,
                "partnerSplits": [
                  {
                    "partner": "CL",
                    "percent": 60
                  },
                  {
                    "partner": "UH",
                    "percent": 40
                  }
                ]
              }
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "updatePrograms": {
          "SET": {
            "proposal": {
              "proposalClass": {
                "largeProgram": {
                  "minPercentTotalTime": 96
                }
              },
              "partnerSplits": [
                {
                  "partner": "CL",
                  "percent": 60
                },
                {
                  "partner": "UH",
                  "percent": 40
                }
              ]
            }
          },
          "WHERE": { "id": { "EQ": "p-3" } }
        }
      }
    """.some
  )

  private def programQuery(name: String) = s"""
  {
    $name {
      id
      name
      existence
      proposal {
        title
        proposalClass {
          __typename
          minPercentTime
          ... on LargeProgram {
            minPercentTotalTime
            totalTime {
              seconds
            }
          }
          ... on Intensive {
            minPercentTotalTime
            totalTime {
              seconds
            }
          }
        }
        category
        toOActivation
        abstract
        partnerSplits {
          partner
          percent
        }
      }
    }
  }
  """
}
