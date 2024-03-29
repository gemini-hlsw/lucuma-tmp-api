// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package seqexec

import cats.syntax.option._
import io.circe.literal._
import lucuma.odb.api.model.{Step, Visit}

class VisitSuite extends OdbSuite {

  val vid: Visit.Id =
    Visit.Id.parse("v-f07560e6-537f-4002-9188-58cff2a70565").get

  val sid: Step.Id =
    Step.Id.parse("s-7afec527-862f-40f2-85d8-3bd4741c336c").get

  // Record a visit.
  queryTest(
    query = s"""
      mutation RecordGmosSouthVisit($$recordInput: RecordGmosSouthVisitInput!) {
        recordGmosSouthVisit(input: $$recordInput, visitId: "$vid") {
          visitRecord {
            id
          }
        }
      }
    """,
    expected =json"""
      {
        "recordGmosSouthVisit": {
          "visitRecord": {
            "id": ${vid.toString}
          }
        }
      }
    """,
    variables = json"""
      {
        "recordInput": {
          "observationId": "o-2",
          "static": {
            "detector": "HAMAMATSU",
            "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING",
            "stageMode": "FOLLOW_XY"
          }
        }
      }
    """.some,
    List(ClientOption.Http)
  )

  // Add a sequence event
  queryTest(
    query = s"""
      mutation AddSequenceEvent($$eventInput: AddSequenceEventInput!) {
        addSequenceEvent(input: $$eventInput) {
          event {
            payload {
              command
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "addSequenceEvent": {
          "event": {
            "payload": {
              "command": "START"
            }
          }
        }
      }
    """,
    variables = json"""
      {
        "eventInput": {
          "visitId": ${vid.toString},
          "location": {
            "observationId": "o-2"
          },
          "payload": {
            "command": "START"
          }
        }
      }
    """.some,
    List(ClientOption.Http)
  )

  // Record a step.
  queryTest(
    query = s"""
      mutation RecordGmosSouthStep($$recordInput: RecordGmosSouthStepInput!) {
        recordGmosSouthStep(input: $$recordInput, stepId: "$sid") {
          stepRecord {
            id
          }
        }
      }
    """,
    expected =json"""
      {
        "recordGmosSouthStep": {
          "stepRecord": {
            "id": ${sid.toString}
          }
        }
      }
    """,
    variables = json"""
      {
        "recordInput": {
          "observationId": "o-2",
          "visitId": ${vid.toString},
          "stepConfig": {
            "bias": {
              "config": {
                "exposure": {
                  "seconds": 0.0
                },
                "readout": {
                  "xBin": "ONE",
                  "yBin": "ONE",
                  "ampCount": "TWELVE",
                  "ampGain": "LOW",
                  "ampRead": "SLOW"
                },
                "dtax": "ONE",
                "roi": "FULL_FRAME",
                "gratingConfig": {
                  "grating":  "B1200_G5321",
                  "order": "ONE",
                  "wavelength": {
                    "nanometers": 500.0
                  }
                }
              }
            }
          }
        }
      }
    """.some,
    List(ClientOption.Http)
  )

  // Add a step event
  queryTest(
    query = s"""
      mutation AddStepEvent($$eventInput: AddStepEventInput!) {
        addStepEvent(input: $$eventInput) {
          event {
            payload {
              stepStage
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "addStepEvent": {
          "event": {
            "payload": {
              "stepStage": "START_STEP"
            }
          }
        }
      }
    """,
    variables = json"""
      {
        "eventInput": {
          "visitId": ${vid.toString},
          "location": {
            "observationId": "o-2",
            "stepId": ${sid.toString}
          },
          "payload": {
             "sequenceType": "SCIENCE",
             "stepStage": "START_STEP"
          }
        }
      }
    """.some,
    List(ClientOption.Http)
  )

  // Add a dataset event
  queryTest(
    query = s"""
      mutation AddDatasetEvent($$eventInput: AddDatasetEventInput!) {
        addDatasetEvent(input: $$eventInput) {
          event {
            payload {
              datasetStage
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "addDatasetEvent": {
          "event": {
            "payload": {
               "datasetStage": "START_OBSERVE"
             }
           }
        }
      }
    """,
    variables = json"""
      {
        "eventInput": {
          "visitId": ${vid.toString},
          "location": {
            "observationId": "o-2",
            "stepId":         ${sid.toString},
            "index":          1
          },
          "payload": {
            "datasetStage": "START_OBSERVE",
            "filename": "S20220504S0001.fits"
          }
        }
      }
    """.some,
    List(ClientOption.Http)
  )

  // List the dataset
  queryTest(
    query = s"""
      query ListDatasets {
        datasets(WHERE: { observationId: { EQ: "o-2" } } ) {
          matches {
            id {
              observationId
              stepId
              index
            }
            filename
            qaState
          }
        }
      }
    """,
    expected =json"""
      {
        "datasets": {
          "matches": [
            {
              "id": {
                "observationId": "o-2",
                "stepId": ${sid.toString},
                "index": 1
              },
              "filename": "S20220504S0001.fits",
              "qaState": null
            }
          ]
        }
      }
    """,
    variables = None,
    List(ClientOption.Http)
  )

  // Set the QA State
  queryTest(
    query = """
      mutation EditDataset($editDatasetsInput: UpdateDatasetsInput!) {
        updateDatasets(input: $editDatasetsInput) {
          datasets {
            id {
              observationId
              stepId
              index
            }
            filename
            qaState
          }
        }
      }
    """,
    expected =json"""
      {
        "updateDatasets": {
          "datasets": [
            {
              "id": {
                "observationId": "o-2",
                "stepId": ${sid.toString},
                "index": 1
              },
              "filename": "S20220504S0001.fits",
              "qaState": "PASS"
            }
          ]
        }
      }
    """,
    variables =json"""
      {
        "editDatasetsInput": {
          "SET": {
            "qaState": "PASS"
          },
          "WHERE": {
            "observationId": { "EQ": "o-2" },
            "stepId": { "EQ": ${sid.toString} }
          }
        }
      }
    """.some,
    List(ClientOption.Http)
  )

  // List the events
  queryTest(
    query = s"""
      query ListEvents {
        executionEvents {
          matches {
            id
            observation {
              id
            }
            visitId
            __typename
            ... on SequenceEvent {
              payload {
                command
              }
            }
            ... on StepEvent {
              payload {
                stepStage
              }
            }
            ... on DatasetEvent {
              payload {
                filename
                datasetStage
              }
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "executionEvents": {
          "matches": [
            {
              "id": "e-2",
              "observation": {
                "id": "o-2"
              },
              "visitId": ${vid.toString},
              "__typename" : "SequenceEvent",
              "payload": {
                "command": "START"
              }
            },
            {
              "id": "e-3",
              "observation": {
                "id": "o-2"
              },
              "visitId": ${vid.toString},
              "__typename" : "StepEvent",
              "payload": {
                "stepStage": "START_STEP"
              }
            },
            {
              "id": "e-4",
              "observation": {
                "id": "o-2"
              },
              "visitId": ${vid.toString},
              "__typename" : "DatasetEvent",
              "payload": {
                "filename": "S20220504S0001.fits",
                "datasetStage": "START_OBSERVE"
              }
            }
          ]
        }
      }
    """,
    variables = None,
    List(ClientOption.Http)
  )

}
