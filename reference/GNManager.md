# GeoNetwork REST API Manager

The function `GNManager$new` will set-up the right Geonetwork manager
depending on the GeoNetwork version specified by the user. For the
time-being, GeoNetwork with version \< 4 will be interfaced with the
GeoNetwork legacy API (see detailed documentation at
[GNLegacyAPIManager](https://eblondel.github.io/geonapi/reference/GNLegacyAPIManager.md)),
while starting with GeoNetwork 3.2, the new GeoNetwork OpenAPI will be
used.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object.

## Value

Object of [`R6Class`](https://r6.r-lib.org/reference/R6Class.html) with
methods for communication with the API of a GeoNetwork instance.

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Super class

[`geonapi::GNAbstractManager`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.md)
-\> `GNManager`

## Methods

### Public methods

- [`GNManager$new()`](#method-GNManager-new)

- [`GNManager$clone()`](#method-GNManager-clone)

Inherited methods

- [`geonapi::GNAbstractManager$ERROR()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-ERROR)
- [`geonapi::GNAbstractManager$INFO()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-INFO)
- [`geonapi::GNAbstractManager$WARN()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-WARN)
- [`geonapi::GNAbstractManager$getClassName()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-getClassName)
- [`geonapi::GNAbstractManager$getLang()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-getLang)
- [`geonapi::GNAbstractManager$getUrl()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-getUrl)
- [`geonapi::GNAbstractManager$logger()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-logger)
- [`geonapi::GNAbstractManager$login()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-login)

------------------------------------------------------------------------

### Method `new()`

Initializes a GNManager

#### Usage

    GNManager$new(url, user = NULL, pwd = NULL, version, logger = NULL)

#### Arguments

- `url`:

  url

- `user`:

  user

- `pwd`:

  pwd

- `version`:

  version

- `logger`:

  logger

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GNManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \dontrun{
   GMManager$new("http://localhost:8080/geonetwork", "admin", "geonetwork", "3.0.0")
#> Error: object 'GMManager' not found
# }

```
