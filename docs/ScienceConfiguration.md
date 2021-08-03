# Observation science configuration

Each observation can have a base configuration belonging to a family of similar configurations,
e.g. GmosNLongSlit or GmosSLongSlit.
Each of thew would be defined by a set of parameters, e.g. Filter, Disperser, Slit Width, etc
Each family would have the same types of parameters but different families can have other sets, e.g. for example for
GmosNLongSlit we need to provide a GMOS-N filter, a GMOS-N disperser and a slit width.

## Creation

When creating an observation a configuration can be provided:

```graphql
mutation {
  createObservation(input: {
    name: "o-3",
    programId: "p-2",
    scienceConfiguration: {
      gmosNorthLongSlit: {
        filter: Y
        disperser: B600_G5303
        slitWidth: {
          arcseconds: 5
        }
      }
    }
  }) {
    scienceConfiguration {
        instrument
        mode
        ... on GmosNorthLongSlit {
          gn_filter: filter
          gn_disperser: disperser
          gn_slitWidth: slitWidth {
            arcseconds
          }
        }
        ... on GmosSouthLongSlit {
          gs_filter: filter
          gs_disperser: disperser
          gs_slitWidth: slitWidth {
            arcseconds
          }
        }
    }
  }
}
```

With a result
```json
{
  "data": {
    "createObservation": {
      "scienceConfiguration": {
        "instrument": "GMOS_NORTH",
        "mode": "GMOS_NORTH_LONG_SLIT",
        "gn_filter": "Y",
        "gn_disperser": "B600_G5303",
        "gn_slitWidth": {
          "arcseconds": 5
        }
      }
    }
  }
}
```

## Deletion
A configuration can be deleted by passing a `null` on `updateObservation`

```graphql
mutation {
  updateObservation(input: {
    observationId: "o-5",
    scienceConfiguration: null
  }) {
    scienceConfiguration {
      instrument
    }
  }
}
```

With result:
```json
{
  "data": {
    "updateObservation": {
      "scienceConfiguration": null
    }
  }
}
```

# Set configuration
It is possible to switch in one go to another instance using `set`. Typically here you'd need to
provide all the parameters of the configuration

```graphql
mutation {
  updateObservation(input: {
    observationId: "o-5",
    scienceConfiguration: {
      set: {
        gmosNorthLongSlit: {
          filter: CA_T
          disperser: R400_G5305
          slitWidth: {
            arcseconds: 2
          }
        }
      }
    }
  }) {
    scienceConfiguration {
      instrument
      mode
			... on GmosNorthLongSlit {
      	filter
        disperser
      }
    }
  }
}
```

In this case the previous configuration is removed and a new one saved instead
```json
{
  "data": {
    "updateObservation": {
      "scienceConfiguration": {
        "instrument": "GMOS_NORTH",
        "mode": "GMOS_NORTH_LONG_SLIT",
        "filter": "CA_T",
        "disperser": "R400_G5305"
      }
    }
  }
}
```

# Edit configuration
It is possible to directly edit the current configuration members, e.g. change a filter
```graphql
mutation {
  updateObservation(input: {
    observationId: "o-5",
    scienceConfiguration: {
      edit: {
        gmosNorthLongSlit: {
          filter: HA_C
        }
      }
    }

  }) {
    scienceConfiguration {
      instrument
      mode
			... on GmosNorthLongSlit {
      	filter
      }
    }
  }
}
```

Attempts to edit an non present configuration will be ignored
