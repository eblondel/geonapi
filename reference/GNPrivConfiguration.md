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

- `privileges`:

  privileges

## Methods

### Public methods

- [`GNPrivConfiguration$new()`](#method-GNPrivConfiguration-new)

- [`GNPrivConfiguration$setPrivileges()`](#method-GNPrivConfiguration-setPrivileges)

- [`GNPrivConfiguration$clone()`](#method-GNPrivConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes an object of class `GNPrivConfiguration`

#### Usage

    GNPrivConfiguration$new()

------------------------------------------------------------------------

### Method `setPrivileges()`

Sets the operation privileges for a particular group. Allowed group
values are "guest","intranet" and "all". Allowed values for operation
privileges are "view", "download", "editing", "notify", "dynamic" and
"featured".

#### Usage

    GNPrivConfiguration$setPrivileges(group, privileges)

#### Arguments

- `group`:

  group

- `privileges`:

  privileges

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GNPrivConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \dontrun{
 pcfg <- GNPrivConfiguration$new()
 pcfg$setPrivileges("all", c("view","dynamic","featured"))
# }
```
