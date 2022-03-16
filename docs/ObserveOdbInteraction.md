# Observe / ODB Interaction

This document briefly sketches the information flow and ODB API calls required by Observe to execute an observation according to the latest proposal.

## Fetching the Sequence(s)

Observe uses an observation's `execution` field to retrieve the static (unchanging) configuration along with acquisition and science sequences.  In general, Observe will query for the complete configuration of each step and will have to be prepared to accept any instrument.  In the example below though, a small subset of the complete configuration information available (and needed) for a single instrument is shown.


```
fragment GmosSouthAtomFields on GmosSouthAtom {
  steps {
    instrumentConfig {
      exposure { seconds }
      filter
      fpu {
        builtin
      }
    }
    
    stepConfig {
      ... on Gcal {
        arcs
      }
      ... on Science {
        offset {
          p { arcseconds }
          q { arcseconds }
        }
      }
    }
  }
}
query Observe {
  observation(observationId: "o-2") {
    execution {
      executionConfig {
        ... on GmosSouthExecutionConfig {
          static {
            stageMode
            detector
            mosPreImaging
          }
          acquisition {
            nextAtom {
              ...GmosSouthAtomFields
            }
            possibleFuture {
              ...GmosSouthAtomFields
            }
          }
          science {
            nextAtom {
              ...GmosSouthAtomFields
            }
            possibleFuture {
              ...GmosSouthAtomFields
            }
          }
        }
      }
    }
  }
}
```

Notice that the `static` configuration is separated from the steps themselves and that the `acquisition` and `science` steps are segregated.  Observe would start by executing the acquisition steps and once accomplished switch to the science steps.  Within a sequence, the `nextAtom` (if any) is the one that Observe should execute while the `possibleFuture` shows what we expect to happen for the remainder of the sequence.  Because the possible future is uncertain, after finishing the `nextAtom`, Observe would repeat the query to obtain the latest `nextAtom` and latest `possibleFuture`.  This would continue until no `nextAtom` is forthcoming or until the observer decides to stop the observation.

## Starting a Visit

Before executing an observation, Observe would record the start of a "visit".  This produces a `VisitRecord` in the database.  In theory the `static` configuration in the query above can be modified or ignored altogether so part of recording a visit is to pass the actual static configuration that is used.

```
mutation RecordVisit($visitInput: GmosSouthVisitRecordInput!) {
  recordGmosSouthVisit(input: $visitInput) {
    id
  }
}

{  
  "visitInput": {
    "observationId": "o-2",
    "static": {
      "detector":      "HAMAMATSU",
      "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING",
      "nodAndShuffle": null,
      "stageMode":     "FOLLOW_XY"
    }
  }
}
```

This produces an initial `GmosSouthVisitRecord` which contains an `id` for the visit.

```
{
  "data": {
    "recordGmosSouthVisit": {
      "id": "v-4faed9d9-1357-4140-b808-928a2fe4ca6a"
    }
  }
}
```

The ID is required for sending events.

## Sequence Events

As the acquisition and science sequences are executed, Observe generates events that are sent to the ODB.

```
mutation AddSequenceEvent($seqEventInput: AddSequenceEventInput!) {
  addSequenceEvent(input:$seqEventInput) {
    id
  }
}

{
  "seqEventInput": {
    "observationId": "o-2",
    "visitId":       "v-4faed9d9-1357-4140-b808-928a2fe4ca6a",
    "command":       "START"
  }
}
```

In addition to `START`, the `ABORT`, `CONTINUE`, `PAUSE`, `SLEW`, and `STOP` sequence commands can also be recorded.


## Executing a Step

Before a step is executed, Observe records the configuration in the ODB, producing a `StepRecord`.  As for the visit record case above, the configuration that is actually executed should be sent to the ODB and a new step record is returned.

```
mutation RecordStep($stepInput: GmosSouthStepRecordInput!) {
  recordGmosSouthStep(input: $stepInput) {
    id
  }
}

"stepInput": {
  "observationId": "o-2",
  "visitId": "v-4faed9d9-1357-4140-b808-928a2fe4ca6a",
  "stepConfig": {
    "science": {
      "offset": {
        "p": { "arcseconds": 0 },
        "q": { "arcseconds": 0 }
      },
      "config": {
        "exposure": {
          "seconds": 10
        },
        "readout": {
          "xBin": "ONE",
          "yBin": "ONE",
          "ampGain": "LOW",
          "ampCount": "TWELVE",
          "ampRead": "SLOW"
        },
        "dtax": "ONE",
        "roi": "FULL_FRAME",
        "grating": {
          "disperser": "B1200_G5321",
          "order": "ONE",
          "wavelength": { "nanometers": 520 }
        },
        "fpu": {
          "builtin": "LONG_SLIT_1_00"
        }
      }
    }
  }
}
```

The step ID in the record is needed for sending step and dataset events.

```
{
  "data": {
    "recordGmosSouthStep": {
      "id": "s-cf272424-0d0c-4592-a99c-45e5e95a6e87"
    }
  }
}
```

## Sending Step and Dataset Events

A step or dataset event requires the observation, visit, and step ids.

```
mutation AddStepEvent($stepEventInput: AddStepEventInput!) {
  addStepEvent(input: $stepEventInput) {
    id
  }
}

{
  "stepEventInput": {
    "observationId": "o-2",
    "visitId":       "v-280fd080-0176-4ce3-a755-f824c0b5a84d",
    "stepId":        "s-cf272424-0d0c-4592-a99c-45e5e95a6e87",
    "sequenceType":  "SCIENCE",
    "stage":         "START_STEP"
  }
}
```

Step events include `START_STEP`, `START_CONFIGURE`, `END_CONFIGURE`, `START_OBSERVE`, `END_OBSERVE`, and `END_STEP`.

Dataset events are similar but at the dataset level:

```
type AddDatasetEventInput {
  observationId: ObservationId!
  visitId:       VisitId!
  stepId:        StepId!
  datasetIndex:  Int!
  filename:      DatasetFilename
  stageType:     DatasetStage!
}

enum DatasetStage {
  START_OBSERVE
  START_READOUT
  START_WRITE
  END_OBSERVE
  END_READOUT
  END_WRITE
}
```

Events are used to determine when steps and atoms have completed. Their presence thereby potentially changes the results of subsequent `nextAtom` calls.  Events must refer to valid visits and steps that have been previously recorded via `recordGmosSouthVisit` (for example) and `recordGmosSouthStep` or they will be rejected.

