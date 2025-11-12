# A GeoNetwork version

This class is an utility wrap the Geonetwork version

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object.

## Value

Object of [`R6Class`](https://r6.r-lib.org/reference/R6Class.html) for
modelling a GeoNetwork version

## Details

GeoNetwork REST API - GeoNetwork Version

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Public fields

- `version`:

  version

- `value`:

  value

## Methods

### Public methods

- [`GNVersion$new()`](#method-GNVersion-new)

- [`GNVersion$lowerThan()`](#method-GNVersion-lowerThan)

- [`GNVersion$greaterThan()`](#method-GNVersion-greaterThan)

- [`GNVersion$equalTo()`](#method-GNVersion-equalTo)

- [`GNVersion$clone()`](#method-GNVersion-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes an object of class GNVersion

#### Usage

    GNVersion$new(version)

#### Arguments

- `version`:

  version

------------------------------------------------------------------------

### Method `lowerThan()`

Compares to a version and returns TRUE if it is lower, FALSE otherwise

#### Usage

    GNVersion$lowerThan(version)

#### Arguments

- `version`:

  version

#### Returns

`TRUE` if lower, `FALSE` otherwise

------------------------------------------------------------------------

### Method `greaterThan()`

Compares to a version and returns TRUE if it is greater, FALSE otherwise

#### Usage

    GNVersion$greaterThan(version)

#### Arguments

- `version`:

  version

#### Returns

`TRUE` if lower, `FALSE` otherwise

------------------------------------------------------------------------

### Method `equalTo()`

Compares to a version and returns TRUE if it is equal, FALSE otherwise

#### Usage

    GNVersion$equalTo(version)

#### Arguments

- `version`:

  version

#### Returns

`TRUE` if lower, `FALSE` otherwise

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GNVersion$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \dontrun{
version <- GNVersion$new("2.6.4")
# }
```
