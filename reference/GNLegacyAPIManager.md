# GNLegacyAPIManager

GNLegacyAPIManager

GNLegacyAPIManager

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object.

## Value

Object of [`R6Class`](https://r6.r-lib.org/reference/R6Class.html) with
methods for communication with the REST API of a GeoNetwork instance
using the legacy API.

## Note

From 2025-05-02, the INSPIRE metadata validation does not require
anymore an API Key. Therefore, it is not required to specify an
`geometa_inspireValidator`. To send your metadata to INSPIRE, just set
`geometa_inspire` to `TRUE`.

From 2025-05-02, the INSPIRE metadata validation does not require
anymore an API Key. Therefore, it is not required to specify an
`geometa_inspireValidator`. To send your metadata to INSPIRE, just set
`geometa_inspire` to `TRUE`.

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Super class

[`geonapi::GNAbstractManager`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.md)
-\> `GNLegacyAPIManager`

## Methods

### Public methods

- [`GNLegacyAPIManager$new()`](#method-GNLegacyAPIManager-new)

- [`GNLegacyAPIManager$login()`](#method-GNLegacyAPIManager-login)

- [`GNLegacyAPIManager$getGroups()`](#method-GNLegacyAPIManager-getGroups)

- [`GNLegacyAPIManager$getCategories()`](#method-GNLegacyAPIManager-getCategories)

- [`GNLegacyAPIManager$insertMetadata()`](#method-GNLegacyAPIManager-insertMetadata)

- [`GNLegacyAPIManager$setPrivConfiguration()`](#method-GNLegacyAPIManager-setPrivConfiguration)

- [`GNLegacyAPIManager$get()`](#method-GNLegacyAPIManager-get)

- [`GNLegacyAPIManager$getMetadataByID()`](#method-GNLegacyAPIManager-getMetadataByID)

- [`GNLegacyAPIManager$getMetadataByUUID()`](#method-GNLegacyAPIManager-getMetadataByUUID)

- [`GNLegacyAPIManager$getInfoByID()`](#method-GNLegacyAPIManager-getInfoByID)

- [`GNLegacyAPIManager$getInfoByUUID()`](#method-GNLegacyAPIManager-getInfoByUUID)

- [`GNLegacyAPIManager$updateMetadata()`](#method-GNLegacyAPIManager-updateMetadata)

- [`GNLegacyAPIManager$deleteMetadata()`](#method-GNLegacyAPIManager-deleteMetadata)

- [`GNLegacyAPIManager$deleteMetadataAll()`](#method-GNLegacyAPIManager-deleteMetadataAll)

- [`GNLegacyAPIManager$clone()`](#method-GNLegacyAPIManager-clone)

Inherited methods

- [`geonapi::GNAbstractManager$ERROR()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-ERROR)
- [`geonapi::GNAbstractManager$INFO()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-INFO)
- [`geonapi::GNAbstractManager$WARN()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-WARN)
- [`geonapi::GNAbstractManager$getClassName()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-getClassName)
- [`geonapi::GNAbstractManager$getLang()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-getLang)
- [`geonapi::GNAbstractManager$getUrl()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-getUrl)
- [`geonapi::GNAbstractManager$logger()`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.html#method-logger)

------------------------------------------------------------------------

### Method `new()`

This method is used to instantiate a GNLegacyAPIManager with the `url`
of the GeoNetwork and credentials to authenticate (`user`/`pwd`).

The `keyring_backend` can be set to use a different backend for storing
the Geonetwork password/token with keyring (Default value is 'env').

The logger can be either NULL, "INFO" (with minimum logs), or "DEBUG"
(for complete curl http calls logs)

#### Usage

    GNLegacyAPIManager$new(
      url,
      user = NULL,
      pwd = NULL,
      version,
      logger = NULL,
      keyring_backend = "env"
    )

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

- `keyring_backend`:

  keyring backend. Default is 'env'

------------------------------------------------------------------------

### Method `login()`

\#' This methods attempts a connection to GeoNetwork REST API. User
internally during initialization of `GNLegacyAPIManager`.

#### Usage

    GNLegacyAPIManager$login(user, pwd)

#### Arguments

- `user`:

  user

- `pwd`:

  pwd

------------------------------------------------------------------------

### Method `getGroups()`

Retrieves the list of user groups available in Geonetwork

#### Usage

    GNLegacyAPIManager$getGroups()

#### Returns

an object of class `data.frame`

------------------------------------------------------------------------

### Method `getCategories()`

Retrieves the list of categories available in Geonetwork

#### Usage

    GNLegacyAPIManager$getCategories()

#### Returns

an object of class `data.frame`

------------------------------------------------------------------------

### Method `insertMetadata()`

Inserts a metadata by file, XML object or geometa object of class
[ISOMetadata](https://rdrr.io/pkg/geometa/man/ISOMetadata.html) or
[ISOFeatureCatalogue](https://rdrr.io/pkg/geometa/man/ISOFeatureCatalogue.html).
If successful, returns the Geonetwork metadata internal identifier
(integer). Extra parameters `geometa_validate` (TRUE by default) and
`geometa_inspire` (FALSE by default) can be used with geometa objects
for perform ISO and INSPIRE validation respectively.

#### Usage

    GNLegacyAPIManager$insertMetadata(
      xml = NULL,
      file = NULL,
      geometa = NULL,
      group,
      category = NULL,
      stylesheet = NULL,
      validate = FALSE,
      geometa_validate = TRUE,
      geometa_inspire = FALSE,
      geometa_inspireValidator = NULL
    )

#### Arguments

- `xml`:

  xml object of class
  [XMLInternalNode-class](https://rdrr.io/pkg/XML/man/XMLNode-class.html)

- `file`:

  file

- `geometa`:

  geometa, object of class
  [ISOMetadata](https://rdrr.io/pkg/geometa/man/ISOMetadata.html) or
  [ISOFeatureCatalogue](https://rdrr.io/pkg/geometa/man/ISOFeatureCatalogue.html)

- `group`:

  group

- `category`:

  category

- `stylesheet`:

  stylesheet

- `validate`:

  validate

- `geometa_validate`:

  validate geometa object

- `geometa_inspire`:

  validate geometa object vs. INSPIRE

- `geometa_inspireValidator`:

  geometa INSPIRE validator to use. Deprecated, see below note

------------------------------------------------------------------------

### Method `setPrivConfiguration()`

Set the privilege configuration for a metadata. 'id' is the metadata
integer id. 'config' is an object of class "GNPrivConfiguration".

#### Usage

    GNLegacyAPIManager$setPrivConfiguration(id, config)

#### Arguments

- `id`:

  id

- `config`:

  config

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Generic getter for metadata. Possible values for by are 'id', 'uuid'.
Used internally only. The 'output' argument gives the type of output to
return, with possible values "id", "metadata", "info".

#### Usage

    GNLegacyAPIManager$get(id, by, output)

#### Arguments

- `id`:

  id

- `by`:

  by

- `output`:

  output

------------------------------------------------------------------------

### Method `getMetadataByID()`

Get a metadata by Id

#### Usage

    GNLegacyAPIManager$getMetadataByID(id)

#### Arguments

- `id`:

  id

#### Returns

an object of class `ISOMetadata` (ISO 19115) or `ISOFeatureCatalogue`
(ISO 19110) (from geometa package)

------------------------------------------------------------------------

### Method `getMetadataByUUID()`

Get a metadata by UUID

#### Usage

    GNLegacyAPIManager$getMetadataByUUID(uuid)

#### Arguments

- `uuid`:

  uuid

#### Returns

an object of class `ISOMetadata` (ISO 19115) or `ISOFeatureCatalogue`
(ISO 19110) (from geometa package)

------------------------------------------------------------------------

### Method `getInfoByID()`

Get a metadata Info by Id.

#### Usage

    GNLegacyAPIManager$getInfoByID(id)

#### Arguments

- `id`:

  id

#### Returns

an XML document object

------------------------------------------------------------------------

### Method `getInfoByUUID()`

Get a metadata Info by UUID

#### Usage

    GNLegacyAPIManager$getInfoByUUID(uuid)

#### Arguments

- `uuid`:

  uuid

#### Returns

an XML document object

------------------------------------------------------------------------

### Method `updateMetadata()`

Updates a metadata by file, XML object or geometa object of class
'ISOMetadata' or 'ISOFeatureCatalogue'. Extra parameters
`geometa_validate` (TRUE by default) and `geometa_inspire` (FALSE by
default) can be used with geometa objects for perform ISO and INSPIRE
validation respectively.

#### Usage

    GNLegacyAPIManager$updateMetadata(
      id,
      xml = NULL,
      file = NULL,
      geometa = NULL,
      geometa_validate = TRUE,
      geometa_inspire = FALSE,
      geometa_inspireValidator = NULL
    )

#### Arguments

- `id`:

  metadata id

- `xml`:

  xml object of class
  [XMLInternalNode-class](https://rdrr.io/pkg/XML/man/XMLNode-class.html)

- `file`:

  file

- `geometa`:

  geometa, object of class
  [ISOMetadata](https://rdrr.io/pkg/geometa/man/ISOMetadata.html) or
  [ISOFeatureCatalogue](https://rdrr.io/pkg/geometa/man/ISOFeatureCatalogue.html)

- `geometa_validate`:

  validate geometa object

- `geometa_inspire`:

  validate geometa object vs. INSPIRE

- `geometa_inspireValidator`:

  geometa INSPIRE validator to use. Deprecated, see below note.

------------------------------------------------------------------------

### Method `deleteMetadata()`

Deletes metadata by Id.

#### Usage

    GNLegacyAPIManager$deleteMetadata(id)

#### Arguments

- `id`:

  id

#### Returns

the id of the record deleted, `NULL` otherwise

------------------------------------------------------------------------

### Method `deleteMetadataAll()`

Deletes all metadata

#### Usage

    GNLegacyAPIManager$deleteMetadataAll()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GNLegacyAPIManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \dontrun{
   GNLegacyAPIManager$new("http://localhost:8080/geonetwork", "admin", "geonetwork", "3.0.0")
#> Error in curl::curl_fetch_memory(url, handle = handle): Couldn't connect to server [localhost]:
#> Failed to connect to localhost port 8080 after 0 ms: Couldn't connect to server
# }
```
