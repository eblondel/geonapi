# A GeoNetwork privilege configuration

This class is an utility to configure privileges

## Format

[R6Class](https://r6.r-lib.org/reference/R6Class.html) object.

## Value

Object of [R6Class](https://r6.r-lib.org/reference/R6Class.html) for
modelling a GeoNetwork Privilege configuration

## Details

GeoNetwork REST API - GeoNetwork privilege configuration

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Public fields

- `group`:

  group

- `privileges`:

  privileges

## Methods

### Public methods

- [`GNPriv$new()`](#method-GNPriv-new)

- [`GNPriv$clone()`](#method-GNPriv-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a `GNPriv` object

#### Usage

    GNPriv$new(group, privileges)

#### Arguments

- `group`:

  group

- `privileges`:

  privileges

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GNPriv$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \dontrun{
 priv <- GNPriv$new(group="all", privileges=c("view","dynamic","featured"))
# }
```
