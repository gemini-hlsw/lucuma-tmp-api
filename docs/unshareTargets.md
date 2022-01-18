# Un-share Targets

Removing the sharing model for targets necessitates a number of API changes, but the best way 
forward is not immediately obvious.  This document proposes a series of updates that accomplish 
the goal while still supporting bulk editing.  

## Target IDs

Target IDs make otherwise identical targets distinct.  For example, two "NGC 5949" instances 
would have distinct IDs, making them incompatible with grouping by target.  Instead of referring to 
targets by ID, we'll instead use target names and require unique names within a target list.

(Since the `name` field will be needed to identify a target to edit, this raises the question of 
how to change the target name.  Unfortunately it seems like a special `newName` edit field will be 
required.)


## Asterisms and Targets
   
I propose to remove the asterism as a separate type.  Observations instead have a list of 
science targets, accessible via a `science` field in the `targets` object.

The `Targets` object would house everything related to targets, including the science target(s), 
guide stars, blind offset target, etc.  The `science` field in `Targets` may return no targets, 
one target, or multiple targets. We could add an optional `firstScience` field as well.  It 
would be the first target in the list, if any, and might be useful for the majority of 
instruments which have a single science target.

We'll pull the base coordinate from asterism into `Targets` as well, but for the moment ignore 
guide stars and other data that would eventually wind up here. Putting it all together, a query 
like the following could be written

```
query Observation {
  observation(observationId: "obs-3") {
    name
    targets {
      base(at: "20210706-0123") {
        ra  { hms }
        dec { dms }
      }
      science {
        name
        tracking {
          ... on Sidereal {
            ra  { hms }
            dec { hms }
            pm {
              ra  { milliarcsecondsPerYear }
              dec { milliarcsecondsPerYear }
            }
          }
        }
      }
    }
  }
}
```

listing the base position at a given time along with all science targets. The base position here 
would be computed from the science targets by default, and made editable as `explicitBase` even 
for single-target observations.

Guide stars, blind offsets etc. would then have a future home as siblings to the `science` 
targets. Asterism details (e.g., GHOST configuration) would also eventually be added separately 
from the science target list itself.

   
## Editing

When creating a new observation, the mutation would now accept a list of target information 
detailing the targets to add.  A bigger challenge though is editing existing observations and their 
targets.  In general the user applies an edit by detailing fields that they wish to change in an 
input.  For example, the `EditObservationInput` contains a field for the `name` and one for the 
`status`, among other items.  To update the name and leave the status (and everything else) 
unchanged, you send an input such as:

```json
{
  "editObservation": {
    "observationId": "o-3",
    "name": "Biff"
  }
}
```
to the `editObservation` mutation (currently named `updateObservation`).

This scheme breaks down when one of the fields is a list.  The user may want to delete all the 
items in the list, replace the list altogether, edit some items while leaving others unchanged, 
add new items, etc.  In order to differentiate the various cases, the target list edit input can 
have two explicit alternatives, `replaceList` or `editList`, only one of which could be specified.

```
input EditTargetListInput {
  replaceList: [ CreateTargetInput! ]
  editList:    [ EditTargetActionInput! ]
}
```

Choosing `replaceList` and providing an empty list of inputs would leave the observation without 
targets.  Choosing `replaceList` and describing one or more targets would remove any existing 
targets and add the new ones (assuming they are well defined).

Choosing `editList` with an empty list of inputs would generate an error.  Where edits are 
provided they would each refer to existing targets by name.

Choosing _both_ `replaceList` and `editList` would generate an error.

Here the `CreateTargetInput` would have to offer either sidereal or nonsidereal options (at 
least, perhaps adding ToO targets in the future).  Exactly _one of the two_ would have to be 
supplied for each element of the `replaceList` list of `CreateTargetInput`.

```
input CreateTargetInput {
  sidereal:    CreateSiderealInput    # See existing API
  nonsidereal: CreateNonSiderealInput # See existing API
  # ToO
}
```

Likewise, the `EditTargetInput` would need to provide sidereal and non-sidereal options.  In 
this case though the user might want to add targets, delete them, or else simply edit them.

```
input EditTargetActionInput {
  add:    CreateTargetInput 
  delete: String
  edit:   EditTargetInput
}

input EditTargetInput {
  sidereal:    EditSiderealInput    # See existing API
  nonsidereal: EditNonSiderealInput # See existing API
  # ToO
}
```

Each edit in the `editList` list would define exactly one of the `add` / `delete` / `edit` options 
above.

A few examples might help to clarify the idea.

### Delete the target list altogether 

```json
{
  "editObservation": {
    "observationId": "o-3",
    "targets": {
      "science": {
        "replaceList": []
      }
    }
  }
}
```

### Replace the target list entirely with a single target

```json
{
  "editObservation": {
    "observationId": "o-3",
    "targets": {
      "science": {
        "replaceList": [
          {
            "sidereal": {
              "name": "NGC 5949",
              "ra":   { "hms": "15:28:00.668" },
              "dec":  { "dms": "64:45:47.4"   },
              "magnitudes": [{
                "band":   "B",
                "value":  12.7,
                "system": "VEGA"
              }]
            }
          }
        ]
      }
    } 
  }
}
```

### Edit the radial velocity of one of the targets

```json
{
  "editObservation": {
    "observationId": "o-3",
    "targets": {
      "science": {
        "editList": [
          {
            "edit": {
              "sidereal": {
                "name": "NGC 5949",
                "radialVelocity": { "metersPerSecond": 423607 }
              }
            }
          }
        ]
      }
    }
  }
}
```
   
### Delete one target, add another, leave the rest (if any) untouched

```json
{
  "editObservation": {
    "observationId": "o-3",
    "targets": {
      "science": {
        "editList": [
          {
            "delete": "NGC 5949"
          },
          {
            "add": {
              "sidereal": {
                "name": "NGC 3269",
                "ra":  { "hms": "10:29:57.070" },
                "dec": { "dms": "-35:13:27.8"  },
                "radialVelocity": { "metersPerSecond": 3753885 },
                "magnitudes": [{
                  "band":   "B",
                  "value":  13.24,
                  "system": "VEGA"
                }]
              }
            }
          }
        ]
      }
    }
  }
}
```

Because these updates apply to the observation itself, they can seem rather lengthy. The path to 
arrive at a science target descends through multiple levels. Additional purposed mutations that 
drill down directly to the level of interest could be added as necessary.  For example, an 
`editSiderealScienceTarget` mutation could take a program ID, or a list of observation IDs, along 
with a sidereal target name.  It would then apply provided updates directly.

For example, set the radial velocity of all "NGC 5949" targets in program `p-34d5`.

```json
{
  "editSiderealScienceTarget": {
    "programId":      "p-34d5",
    "name":           "NGC 5949",
    "radialVelocity": { "metersPerSecond": 423607 }
  }
}
```

## Grouping 

To support grouping observations by common targets, I believe we should maintain a canonical 
target order.  Otherwise, two target lists that are identical except for order will appear 
distinct.  For example, target name plus target type and then by coordinates or horizons id, etc.

It may be desirable to uncover groupings by science targets alone, by guide targets alone, etc.? 
Grouping by the entire `Targets` object though seems like the minimum we must support.

```
type Query {

  # Observations grouped by commonly held Targets
  targetGroups(
  
    # Program ID
    programId: ProgramId!
  
    # Retrieve the `first` n values after the given cursor
    first: Int
    
    # Retrieve values after the one associated with this cursor
    after: Cursor
    
    # Set to true to include deleted observations
    includeDeleted: Boolean! = false   
    
  ): TargetGroupConnection!

}

# A grouping of observations that use the same Targets.  Each node in a
# TargetGroupConnection would be of this type.
type TargetGroup {

  # IDs of observations in the group
  observationIds: [ObservationId!]!
  
  # Observations that use these targets
  observations(
    # Retrieve `first` values after the given cursor
    first: Int

    # Retrieve values after the one associated with this cursor
    after: Cursor

    # Set to true to include deleted values
    includeDeleted: Boolean! = false
  ): ObservationConnection!

  # Targets held in common across the observations
  targets: Targets!

} 
```

This would support queries such as:

```
query ObservationsByTargets {
  targetGroups(programId: "p-3") {
    nodes {
      observationIds
      target {
        science {
          name
        }
      }
    }
  }
}
```

which tells you the science targets shared by each grouping along with the observation ids 
themselves.

Grouping can be used by Explore to present the tree of target information shown in the "Targets" 
tab in the inception documents.


## Bulk Edit

Here we can take the same target editor portion of observation updating and apply it to multiple 
observations at once.

```
type Mutation {

  # Bulk edit targets
  editTargets(
    input: BulkEditTargetsInput!
  ): [Observation!]!
  
}

input BulkEditTargetsInput {
  observationIds: [ObservationId!]!
  targets:        EditTargetsInput!
}

input EditTargetsInput {
  explicitBase: CoordinatesInput
  science:      EditTargetListInput # as seen in the "Editing" section above
  
  # Other kinds of targets here -- guide stars, blind offsets etc.  
}

```

Then, for example, you could assign the same science target to many observations at once with a 
`BulkEditTargetsInput` such as:

```json
{
  "observationIds": [ "o-a13f", "o-b4fe", "o-c573" ],
  "targets": {
    "science": {
      "replaceList": [
        {
          "sidereal": {
            "name": "NGC 5949",
            "ra":  { "hms": "15:28:00.668" },
            "dec": { "dms": "64:45:47.4"   },
              "magnitudes": [{
              "band":   "B",
              "value":  12.7,
              "system": "VEGA"
            }]
          }
        }
      ]
    }
  } 
}
```
