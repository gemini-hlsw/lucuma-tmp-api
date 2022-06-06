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
          id
        }
      }
    """,
    expected =json"""
      {
        "recordGmosSouthVisit": {
          "id": ${vid.toString}
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
          newEvent {
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
          "newEvent": {
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
          id
        }
      }
    """,
    expected =json"""
      {
        "recordGmosSouthStep": {
          "id": ${sid.toString}
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
          newEvent {
            payload {
              stage
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "addStepEvent": {
          "newEvent": {
            "payload": {
              "stage": "START_STEP"
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
             "stage": "START_STEP"
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
          newEvent {
            payload {
              stage
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "addDatasetEvent": {
          "newEvent": {
            "payload": {
               "stage": "START_OBSERVE"
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
            "stage": "START_OBSERVE",
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
        datasets(observationId: "o-2") {
          nodes {
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
          "nodes": [
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
      mutation EditDataset($editDatasetInput: EditDatasetInput!) {
        editDataset(input: $editDatasetInput) {
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
        "editDataset": {
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
        "editDatasetInput": {
          "select": {
            "observationId": "o-2",
            "stepId": ${sid.toString}
          },
          "patch": {
            "qaState": "PASS"
          }
        }
      }
    """.some,
    List(ClientOption.Http)
  )


}
