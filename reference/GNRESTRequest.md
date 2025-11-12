# GeoNetwork REST API REST Request

GeoNetwork REST API REST Request

GeoNetwork REST API REST Request

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object.

## Value

Object of [`R6Class`](https://r6.r-lib.org/reference/R6Class.html) for
modelling a GeoNetwork REST request

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Public fields

- `rootName`:

  root name

- `children`:

  children

## Methods

### Public methods

- [`GNRESTRequest$new()`](#method-GNRESTRequest-new)

- [`GNRESTRequest$setChild()`](#method-GNRESTRequest-setChild)

- [`GNRESTRequest$encode()`](#method-GNRESTRequest-encode)

- [`GNRESTRequest$clone()`](#method-GNRESTRequest-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a GNRESTRequest

#### Usage

    GNRESTRequest$new(...)

#### Arguments

- `...`:

  any parameter to pass to the request

------------------------------------------------------------------------

### Method `setChild()`

Set child

#### Usage

    GNRESTRequest$setChild(key, value)

#### Arguments

- `key`:

  key

- `value`:

  value

------------------------------------------------------------------------

### Method `encode()`

Encodes request as XML

#### Usage

    GNRESTRequest$encode()

#### Returns

an object of class `character` representing the XML

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GNRESTRequest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
