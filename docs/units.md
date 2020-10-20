# Quantities with Units

Our API will contain many quantities with units.  For example, an offset in p (or q for that matter) is typically expressed as an angular separation in arcsecs.  Wavelengths are often expressed in microns but sometimes in nm.  It can be useful to have an RA expressed as `HH:MM:SS.SSS`, as a floating point value in hours, floating point degrees, or even as microarcsecs for a precise integral form that can withstand round trips (or maybe even separating out integral hours, minutes of arc, etc).

This document explores several options for expressing quantities with units in a GraphQL schema.  There are a couple of dimensions to the problem that should be considered:

* Different API consumers (casual one-off "playground" queries vs. script writers vs. UI software)
* Query outputs vs. inputs to modify values

## API Consumers and Round-Trip

For the first bullet, I believe we should favor human consumers over software.  That means providing at least an option to obtain values in units that are natural for them.  At the same time it needs to be possible for software to precisely "round-trip" values between the client and server.  As example of what we mean by that consider a `Wavelength` type.  For most instruments the natural unit for wavelength is µm (though for GMOS we use nm).  If a wavelenth is computed and sent as a floating point value, but stored as a Postgres `NUMERIC(9,6)` µm in the database, we will have round-trip differences.  For example, say a script computes a value in µm:

```
> 1.05 - 0.1 
res0: Double = 0.9500000000000001
```

If this is sent as a `Float` to the database, it will be stored precisely as `0.950000`.  When read back as µm the script would see `0.95` not `0.9500000000000001`.

## Input vs Output

Considering query outputs vs. modification inputs, a goal is to make the API similar in both cases but it may not be possible to make it identical.  This will be noted as necessary in the options below.


## Schema Options

Continuing with the wavelength example, the rest of the document presents options for encoding values with units in the schema.


### Assumed Canonical Unit

The simplest alternative would be to provide just a single scalar canonical result and let the caller do the translation as required.  A query result for example might be:

```
"wavelength": 0.5505
```

The caller would have to just "know" that this value is expressed as µm via the documentation.  This is [problematic in general](https://www.latimes.com/archives/la-xpm-1999-oct-01-mn-17288-story.html), and precludes a round-trip safe value if expressed as a GraphQL `Float`.  A GraphQL `Int` in pm would safely round-trip but wouldn't be expected by, say, Flamingos2 users.


### Canonical with Explicit Unit Parsing

As another alternative we could provide `String` values that include units that would then have to be parsed:

```
"wavelength": "550.5 nm" 
```

This feels like an undue burden on the client and is not subject to introspection (i.e., GraphQL tools like Playground cannot discover and enforce the `String` format).  It also doesn't address round-trip issues because of course `"0.9500000000000001 µm"` would come back as `"0.95 µm"`.


### Value and Units Fields

Probably a better option would be to include information about the units in use in separate nested fields:

```
wavelength {
  value
  units
}
⤹
"wavelength": {
  "value": 0.5505,
  "units": "MICROMETERS"
}
```

where the "units" correspond to an enumeration of wavelength unit options like `MICROMETERS`, `NANOMETERS`, `ANGSTROMS`.  Here the units field could even be added to a query (which could be defaulted to `MICROMETERS`).  Overriding the default to request nm would look like:

```
wavelength(units: "NANOMETERS") {
  value
  units
}
⤹
"wavelength": {
  "value": 550.5
  "units": "NANOMETERS"
}
```

Instead of making `value` always be a GraphQL `Float` we could offer choices like `asInt`, and `asString`:


```
wavelength(units: "ANGSTROM") {
  asInt
}
⤹
"wavelength": {
  "asInt": 5505
}
```

Or for example:

```
wavelength(units: "NANOMETERS") {
  asInt
  asFloat
  asString
}
⤹
"wavelength": {
  "asInt": 551,
  "asFloat": 550.5,
  "asString": "550.5"
}
```

Note the rounding for the integer case.

As an input, we cannot and should not demand the value in all underlying representations.  Instead we'd have to support all options but make them optional.  The server would flag an error if no value is provided or if mutliple values are provided.  For example, either of these would be rejected:

```
"wavelength": {
  "units": "ANGSTROM"
}

"wavelength": {
  "asInt": 5505,
  "asFloat": 5505.0,
  "units": "ANGSTROM"
}
```

Units could be defaulted but perhaps should be required so that the caller is forced to think about it.


### Units as Nested Fields


It's trivial to provide query options in any unit that makes sense.  Then the client can decide what they need and ask for it in that form.  For example, a wavelength could be requested in all available units as

```
wavelength {
  picometers
  angstroms  
  nanometers
  micrometers
}
```

This is similar to adding a `"units"` parameter to the `wavelength` field but here _each different kind of unit can have the distinct underlying type that makes the most sense_.  For example, `picometers` would return a round-trip safe GraphQL `Int` because that's the smallest unit we can distinguish in the database.  The larger types would be GraphQL `Float`s.  Of course the client would typically request just the units of interest:

```
wavelength {
  micrometers
}
```

As another example, consider a right ascension query where there are multiple formats of potential interest:

```
ra {
  hms
  hours
  degrees
  microarcseconds
}
⤹
"ra": {
  "hms": "05:55:10.305000",
  "hours": 5.919529166666667,
  "degrees": 88.7929375,
  "microarcseconds": 319654575000
}
```

Of course, if you just want the HMS string you only ask for that:

```
ra {
  hms
}
```

There is no natural way using the "Values and Units Fields" to get the "hms" `String` because a query like

```
ra(units: "HMS") {
  asFloat
}
```

doesn't make sense and would have to be rejected (instead we could only support `asString` here).


Again, when seen as an input we cannot require all possible units or any particular unit. In this example, `hms`, `hours` etc would all have to be optional.  Yet we need at least one of them and couldn't handle more than one. For example, this input would be rejected but could not be prevented by the schema:

```
"ra": {
  "hms": "05:55:10.305000",
  "hours": 0.0
}
```

### Units as Nested Fields, Identifying Canonical Form

As a variation on the last option, we could make it clearer which form is round-trip safe by prepending "as" to the other options:

```
wavelength {
  picometers
  asAngstroms  
  asNanometers
  asMicrometers
}
```

making it clearer that Å, nm, and µm are all conversions from the minimum supported wavelength resolution of pm.  In the schema, `picometers` would have GraphQL type `Int` and the others `Float`.


### Combination

In the interest of making scripts easy to read and write, and yet providing round-trip safe values for UIs and scripts that need it, we could offer multiple options.  Here the `picometers`, `angstroms` etc. fields would all simply be GraphQL `Float` types for simplicity and clarity while specific typed options are available for applications.


```
wavelength {
  picometers
  angstroms  
  nanometers
  micrometers

  intValue(unit: "MICROMETERS")
  floatValue(unit: "MICROMETERS")
  format(unit: "MICROMETERS", precision: 3)
}
```

This would allow natural looking script and GraphQL Playground queries like

```
wavelength { micrometers }
⤹
"wavelength": {
  "micrometers": 0.5505
}


```

and at the same time allow applications like Explore to request a precise value like

```
wavelength { intValue(unit: "PICOMETERS") }
⤹
"wavelength": {
  "intValue": 550500
}
```

or

```
wavelength { format(unit: "MICROMETERS", precision: 6) }
⤹
"wavelength": {
  "format": "0.550500"
}
```

The input format would offer similar options.  You could supply a floating point value in any supported unit:

```
"wavelength": {
  "micrometers": 0.5505
}
```

or specify a value and units

```
"wavelength": {
  "intValue": 550500
  "units": "PICOMETERS"
}
```

As before, the server would have to verify that multiple values are not provided and that the units are clear as this would not be apparent from the schema itself.

