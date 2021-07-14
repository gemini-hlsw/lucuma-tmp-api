# Observe API Notes

This document sketches out a few ideas for the Observe execution API.  It focuses on the execution
of a single observation, ignoring for now scheduling and timeline features, daytime calibration 
queues, weather, suggesting where to split observations, etc.  For the initial step at transitioning
Web Seqexec to GPP, the user should simply enter an observation id to load and Observe will execute
it.

Following GraphQL itself, there are query, mutation and subscription aspects to the API.


## Query API

Beginning with the query API, we'll introduce an `Execution` type to house runtime and execution
related data for an observation (suitably access controlled).  The client accesses  it via an 
observation id. Since the given observation may not exist, the result is optional.


```
type Query {
  …
  execution(
    observationId: ObservationId!
  ): Execution
  …
}
```

`Execution` can also (or perhaps alternatively) be returned from a field in the `Observation` type 
itself.

The first challenge for the API is that each instrument has its own configuration details and 
features.  This implies that the API will have a combination of generic parts that apply to any 
instrument along with instrument-specific parts.

```
type Execution {

  # Generic, any instrument, section
  executionState: ExecutionState!
  timeAccounting: TimeAccounting!
  remainingTime:  PlannedTime! 
  
  # Mix of generic and instrument-specific information
  events: ExecutionEvents! 

  # Instrument-specific
  executionConfig: ExecutionConfiguration!
    
}
```

To this we can eventually add convenience methods to query datasets, etc. as needed.

### `ExecutionState`

The `executionState` is only hinted at here, but the idea is to describe the runtime state of an
observation, to be shared across all services and tools.  Picture a state-machine describing the
possible execution states with transitions according to sequence commands and step/dataset
completion.  Subscriptions would be offered to stay informed of progress.  It is expected that this
would be at a higher level than the state maintained by Observe itself, but would provide a
centralized, shared concept of the current state of execution to all interested parties.  Instead
of "Observe tells Engage" that an acquisition is complete for example, what would actually 
happen in the implementation would be that Engage monitors the observation execution state.

Since it won't be needed by Observe, at least initially, we'll leave this for the future.


### `TimeAccounting`

`TimeAccounting` calculates actual executed time and categorizes it into program, partner, and
non-charged categories. Detailed step-by-step, dataset-by-dataset time data can be found in the
execution events discussed later.  Time accounting calculations will require observation events 
along with weather and fault information.  Most of this is of little interest to Observe, though it
will want to update the progress bar on individual steps and on the observation as a whole.

_The following SDL sketches out the general idea but specific details are likely to change._

```
# Equivalent time amount in several unit options (e.g., 120 seconds or 2 minutes)
type Duration {
  # Duration in µs
  microseconds: Long!

  # Duration in ms
  milliseconds: BigDecimal!

  # Duration in seconds
  seconds: BigDecimal!

  # Duration in minutes
  minutes: BigDecimal!

  # Duration in hours
  hours: BigDecimal!
}

# Summary of the time spent on an observation and how it is divided
type TimeAccounting {

   # Total execution time including program, partner and non-charged times
   total:       Duration!

   # Time subtracted from the program's time budget.
   programTime: Duration!

   # Time associated with the program's partners.
   partnerTime: Duration!

   # Time that is not charged to the program or partners (e.g., spent on
   # datasets that do not meet quality standards.)
   nonCharged:  Duration!

}
```

### `PlannedTime`

Remaining time, as opposed to `TimeAccounting`, deals with an estimation of _future_ execution time.
Because the corresponding steps have not been executed, this is subject to change over time but 
important for the scheduler and planning in general.  Individual step and atom time estimates, which
are of interest not only to Explore but also the scheduler, are available in the execution
configuration atoms and steps as shown later.  Planned time at this level would summarize setup, 
acquisition sequence and science sequence time estimates. 

_The following SDL sketches out the general idea but specific details are likely to change._

```

# Individual step time can be divided into categories.  Time spent in each 
# category will be grouped. 
type StepTime {

  # Time spent executing configuration changes (e.g., swapping filters)
  configChange: Duration!
  
  # Time spent collecting data
  exposure: Duration!
  
  # Time spent reading out the detector
  readout: Duration!
  
  # Time spent writing datasets
  write: Duration!

  # Total of all categories  
  total: Duration!

}

# Time estimates
type PlannedTime {

  # Estimated setup time
  setup: Duration!
  
  # Total acquisition time estimate.  Sum of all acquisition steps.
  acquisition: StepTime!
  
  # Total science time estimate.  Sum of all science steps.
  science: StepTime!
  
  # Total planned time across setup, acquisition, and science.
  total: Duration!
  
}

```

### `ExecutionEvents`

`ExecutionEvents` houses a complete record of executed steps and makes available the instrument
configuration at each step, dataset details, etc.   Execution events should be queryable by time
range.

```
# Execution events associated with an observation
type ExecutionEvents {

  all: [ExecutionEvent!]!
  
  forNight(night: ObservingNight!): [ExecutionEvent!]!
  
  forRange(startTime: Timestamp!, endTime: Timestamp!): [ExecutionEvent!]!

}

# All execution events implement the ExecutionEvent interface
interface ExecutionEvent {

  # The time provided by Observe noting when the event was generated
  generated:     Timestamp!
  
  # The time that the event was actually received according to the database
  received:      Timestamp!
  
  observationId: ObservationId!
  
}

```

where `Timestamp` is some custom scalar identifying a date and time. The caller (typically Observe
or Engage) supplies the `generated` timestamp when it triggers the event.  The database server 
records the `received` timestamp when it gets the request.  While this distinction may not be
important, it would permit the caller to hold and batch events without losing information about when
they occurred.  At any rate, if the caller is supplying a timestamp then in theory it could be
anything so it seems useful to also record the reception timestamp.

Observe is the source of most event information. It utilizes the mutation API to inform the 
database of an observation's progress, which creates events and dutifully notifies subscription 
clients.

We envision multiple levels of event information, from sequence events at the highest level, then
step events in the middle, and finally detailed dataset events.


#### Sequence Events

Sequence events cover sequence control as a whole: `START`, `PAUSE`, etc.  These are the same for
all instruments and should be familiar from current operations.

```
enum SequenceCommandType {

  ABORT

  CONTINUE

  PAUSE

  SLEW
  
  START

  STOP

}

type SequenceEvent implements ExecutionEvent {

  <ExecutionEvent fields here, not repeated to focus on what is new>
   
  command: SequenceCommandType!

}
```

#### Step Events

Step events trace the stages of execution of a single step. They have a common
part that is useful in its own right, but also contain the step configuration
itself, which is instrument-specific.

```
enum StepStageType {

  END_CONFIGURE

  END_OBSERVE

  END_STEP

  START_CONFIGURE

  START_OBSERVE

  START_STEP

}

# The sequence is either acquisition or science, and the two are maintained
# separately in the API and Observe UI.
enum SequenceType {

  ACQUISITION
  
  SCIENCE
  
}


interface StepEvent implements ExecutionEvent {

  <ExecutionEvent fields here, not repeated to focus on what is new>

  stage:         StepStageType!
  
  sequenceType:  SequenceType!

  # Monotonically increasing (when combined with sequenceType)
  stepNumber:    PositiveInteger!
  
  # Reference to the atom containting the step
  atomId:        AtomId!
  
  # Reference to the step itself
  stepId:        StepId!
  
}

type GmosNorthStepEvent implements StepEvent {

  <StepEvent fields here, not repeated to focus on what is new>
  
  # Static configuration in use during this step
  static:        GmosNorthStatic!
  
  # The step itself
  step:          GmosNorthStep!        

} 
 
```

There are corresponding step event types for each instrument, complete with their own static (e.g.,
`GmosNorthStatic`) and step (e.g., `GmosNorthStep`) types.  The instrument-specific static and step 
configurations are covered in a bit more detail later in this document and are mostly already
implemented as seen in documentation available in the ODB [staging playground](https://lucuma-odb-staging.herokuapp.com/playground.html).


#### Dataset Events

Dataset event types are the same for all instruments.  Compared to the existing OCS codebase, they 
provide additional insight into the time required for the various parts of dataset production.

```
enum DatasetStageType {

  END_OBSERVE

  END_READOUT

  END_WRITE

  START_OBSERVE

  START_READOUT

  START_WRITE

}

type DatasetEvent {

  <ExecutionEvent fields here, not repeated to focus on what is new>

  eventType: DatasetStageType!

  stepId: StepId!

  datasetId: DatasetId!
  
  # Dataset, when available 
  dataset: Dataset
  
}

type Dataset {

  datasetId: DatasetId!
  
  # File timestamp
  timestamp: Timestamp!
  
  # DHS filename (probably made into a scalar limited by a regex)
  filename: String!

}

```

Multiple dataset events may refer to the same `stepId` because a single step may produce 
multiple datasets.


### `ExecutionConfiguration`

The execution configuration details instrument-specific instructions for the current step and a
guess at what future steps might contain.  This is where Observe obtains instrument configurations
to apply as it executes a sequence. Because each instrument's features will differ, there must be
specific instances for each instrument.

```
# Listing of available instruments.
enum InstrumentType {
  …
  GMOS_N
  GMOS_S
  …
}

# Common across all instruments will be a field that returns the instrument 
# type.
interface ExecutionConfiguration {
  instrument: InstrumentType!
}

type GmosNorthExecution implements ExecutionConfiguration {
  …
}

type GmosSouthExecution implements ExecutionConfiguration {
  …
}

```

To do anything with the execution configuration (other than check the instrument type), the query
will have to match on the specific configuration type.

```
query Execution($observationId: ObservationId!) {
  execution(observationId: $observationId) {
    exeuctionConfig {
      instrument
      ... on GmosNorthExecution {
        static {
          nodAndShuffle {
            posA {
              p { arcseconds }
              q { arcseconds }
            }
            posB {
              p { arcseconds }
              q { arcseconds }
            }
          }
        }
      }
    }
  }
}

```

Most instrument configurations will likely have the same structure, albeit with different individual
static and dynamic (step) types.  For example,

```
type GmosNorthExecution implements ExecutionConfiguration {

  instrument: InstrumentType!

  # Static configuration that cannot change over the course of the sequence
  static:      GmosNorthStatic!
  
  # Acquisition sequence, if any, is requested until the user confirms that the
  # target is acquired.  Some calibration observations, for example, will have
  # no acquisition steps.
  aquisition:  ExecutionSequence!
  
  # Science sequence describes how to configure the instrument to acheive its
  # science goals. 
  science:     ExecutionSequence!  
  
}

# The execution sequence pairs the next atom that should be executed with a 
# `possibleFuture` sequence for reference only.  Once the `nextAtom` is
# complete, a new query is required to get the new `nextAtom`.
type ExecutionSequence {

  # The next atom that should be executed, if any  
  nextAtom:       GmosNorthAtom
  
  # Pontential future sequence, if any
  possibleFuture: GmosNorthSequence
  
}
  

# -----------------------------------------------------------------------------
# The types below are defined and to a large degree implemented.  Documentation
# can be found in https://lucuma-odb-staging.herokuapp.com/playground.html
#
# This is simply an indication of what they contain.

type GmosNorthStatic {

  mosPreImaging: MosPreImaging!
  
  nodAndShuffle: NodAndShuffle
  
  stageMode:     GmosNorthStageMode!

}

type GmosNorthSequence {

  # Atoms that make up the sequence, to be executed in order.
  atoms: [GmosNorthAtom!]!

  # Sum of all contained atoms' estimated times.
  time:  StepTime!
  
}

# Indivisible series of steps.  Breaking an atom implies losing the time
# required to have obtained previous steps in the atom.  An example atom might
# be a science frame and accompanying flat.  Without the flat, the science is
# not usable.
type GmosNorthAtom {

  atomId: AtomId!
  
  # Sum of all step's estimated times.
  time:  StepTime!
  
  # Steps that comprise the atom.  The must be executed in order.  
  steps: [GmosNorthStep!]!

}

enum Breakpoint {

  ENABLED
  
  DISABLED
  
}

enum StepType {

  BIAS
  
  DARK
  
  GCAL
  
  SCIENCE
  
  SMART_GCAL

}

type GmosNorthStep {

  stepId:     StepId!

  # Breakpoints that are programmed into the sequence generation, or present in
  # manual sequence definitions.
  breakpoint: Breakpoint!
  
  # The GMOS instrument configuration at this step, apart from step-specific
  # details like offset positions or GCAL config.
  instrument: GmosNorthDynamic!
  
  # Step config contains the details of bias, dark, gcal, science, etc. steps
  # apart from the GMOS configuration itself, which is in `instrument`.  To
  # do anything (other than find the `StepType`) we have to match on the
  # particular StepConfig.  For example
  #
  # step {
  #   ... on Gcal {
  #     continuum
  #     filter
  #     shutter
  #   }   
  # }
  step:       StepConfig!
  
  # Planned time estimate for this step
  time:  StepTime!
  
}

type GmosNorthDynamic {
  exposure: Duration!
  readout:  GmosCcdReadout!
  dtax:     GmosDtax!
  roi:      GmosRoi!
  grating:  GmosNorthGrating
  filter:   GmosNorthFilter
  fpu:      GmosNorthFpu
}

interface StepConfig {
 
  stepType: StepType!
  
}

type Bias implements StepConfig {
  stepType: StepType!
}

type Dark implements StepConfig {
  stepType: StepType!
}

type Gcal implements StepConfig {
  stepType:  StepType!
  continuum: GcalContinuum
  arcs:      [GcalArc!]!
  filter:    GcalFilter!
  diffuser:  GcalDiffuser!
  shutter:   GcalShutter!
}

type Science implements StepConfig {
  stepType:  StepType!
  offset:    Offset!
}

```

As discussed in the SDL comments above, it is expected that Observe will pull the next atom to
execute at the beginning of the observation and each time thereafter that an atom is completed.
This allows the course of the sequence to change over its execution according to conditions or
other considerations. The `possibleFuture` shows what steps are anticipated for reference in the UI
but should not be relied upon for actual execution.

The same model works whether we're supplying the atoms according to a fixed manual sequence or
generating them according to an algorithm.  In the case of a fixed manual sequence the
`possibleFuture` will simply always come to pass.

The acquisition and science sequences are maintained separately because we assume that human
intervention is required to determine when the acquisition is successful.  Observe would pull from
the acquisition sequence and prompt the user for confirmation of completion, repeating the last atom
until the user says to stop.  _Note: this works for GMOS longslit but I'm not sure 
about other modes. If possible, though, let's keep this simple._



## Mutation API

Recording a sequence command requires creating a `RecordSequenceCommandInput` and passing it to the
corresponding mutation: `recordSequenceCommand`.  As a result, a `SequenceEvent` is generated and
sent to subscribers.

The `RecordSequenceCommandInput` repeats some information in the `SequenceEvent` itself, but having
a separate input type (and payload result type) is a recommended practice in general. In particular,
it greatly facilitates schema evolution.

```
type Mutation {
  …
  # Adding sequence commands (ABORT, PAUSE, etc.) will change the observation
  # execution state and must be access protected. 
  recordSequenceCommand(
    input: RecordSequenceCommandInput!
  ): RecordSequenceCommandResult!
  …
}

type RecordSequenceCommandInput {
  generated:     Timestamp!
  observationId: ObservationId!
  
  # See the SequenceCommandType enum in the Query section
  command:       SequenceCommandType!
}
```

The server returns an `RecordSequenceCommandResult` as a result. Just as for separate `*Input` 
types, separate `*Result` types simplify schema evolution.

```
type RecordSequenceCommandResult {

  # The event that is created
  event: SequenceEvent!
  
  # Observation execution state as a result of adding the sequence command (not
  # specified here)
  state: ExecutionState!
}

```

An example of using this mutation:

```
mutation RecordSequenceCommand($input: RecordSequenceCommandInput!) {
  recordSequenceCommand(input: $input) {
      event {
        eventId
      }
      state {
        sequenceState
      }
    }
  }
}
```

Step stage updates follow the same pattern as sequence commands with `Input` and `Result` types and
a corresponding mutation.

```
type Mutation {
  …
  recordStepStage(
    input: RecordStepStageInput!
  ): RecordStepStageResult!
  …
}

type RecordStepStageInput {
  generated:     Timestamp!
  observationId: ObservationId!
  stepId!        StepId!
  stage:         StepStageType!
}

type RecordStepStageResult {
  event: StepEvent!
  state: ExecutionState!
}
```

Each step may produce more than one dataset, and each individual dataset passes through several
stages on its way to disk: exposure, readout, write. Recording events for each stage of a dataset
will give us greater insight into time estimation accuracy, help with debugging, and generally make
execution more transparent to users.

It is assumed that the DHS filename and file timestamp are not known at the outset, so these are
optional in the `RecordDatasetStageInput` below.

```
type Mutation {
  …
  recordDatasetStage(
    input: RecordDatasetStageInput!
  ): RecrodDatasetStageResult!
  …
}

type RecordDatasetStageInput {
  generated:     Timestamp!
  observationId: ObservationId!
  stepId:        StepId!

  stage:         DatasetStageType!
  datasetId:     DatasetId!
  
  filename:      String
  timestamp:     Timestamp
}

type RecordDatasetStageResult {
  event: DatasetEvent!
  state: ExecutionState!
}
```

## Subscription API

Subscriptions would enable clients to watch for events or execution state changes.  As the entity
triggering these events, Observe is not a likely subscriber so this will only be sketched out here.

```
type Subscription {
  …
  # Subscribes to execution events 
  executionEvent(
  
    # Limits events to those associated with a particular program if specified
    programId:     ProgramId
    
    # Limits events to a particular observation if specified
    observationId: ObservationId
    
  ): ExecutionEvent!
  …
  
  # Individual subscriptions for sequence, step, and dataset events here
  
  # Subscribes to observation execution state
  executionState(
  
    # Limits events to those associated with a particular program if specified
    programId:     ProgramId
    
    # Limits events to a particular observation if specified
    observationId: ObservationId
    
  ): ExecutionState!
  
}
```