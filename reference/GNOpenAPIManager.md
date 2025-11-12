# GNOpenAPIManager

GNOpenAPIManager

GNOpenAPIManager

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

From 2025-05-02, the INSPIRE metadata validation does not require
anymore an API Key. Therefore, it is not required to specify an
`geometa_inspireValidator`. To send your metadata to INSPIRE, just set
`geometa_inspire` to `TRUE`.

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Super class

[`geonapi::GNAbstractManager`](https://eblondel.github.io/geonapi/reference/GNAbstractManager.md)
-\> `GNOpenAPIManager`

## Methods

### Public methods

- [`GNOpenAPIManager$new()`](#method-GNOpenAPIManager-new)

- [`GNOpenAPIManager$login()`](#method-GNOpenAPIManager-login)

- [`GNOpenAPIManager$getGroups()`](#method-GNOpenAPIManager-getGroups)

- [`GNOpenAPIManager$getTags()`](#method-GNOpenAPIManager-getTags)

- [`GNOpenAPIManager$getCategories()`](#method-GNOpenAPIManager-getCategories)

- [`GNOpenAPIManager$downloadMetadataByUUID()`](#method-GNOpenAPIManager-downloadMetadataByUUID)

- [`GNOpenAPIManager$getMetadataByUUID()`](#method-GNOpenAPIManager-getMetadataByUUID)

- [`GNOpenAPIManager$insertRecord()`](#method-GNOpenAPIManager-insertRecord)

- [`GNOpenAPIManager$insertMetadata()`](#method-GNOpenAPIManager-insertMetadata)

- [`GNOpenAPIManager$updateMetadata()`](#method-GNOpenAPIManager-updateMetadata)

- [`GNOpenAPIManager$deleteMetadata()`](#method-GNOpenAPIManager-deleteMetadata)

- [`GNOpenAPIManager$uploadAttachment()`](#method-GNOpenAPIManager-uploadAttachment)

- [`GNOpenAPIManager$publishThumbnail()`](#method-GNOpenAPIManager-publishThumbnail)

- [`GNOpenAPIManager$doiCheckPreConditions()`](#method-GNOpenAPIManager-doiCheckPreConditions)

- [`GNOpenAPIManager$createDOI()`](#method-GNOpenAPIManager-createDOI)

- [`GNOpenAPIManager$deleteDOI()`](#method-GNOpenAPIManager-deleteDOI)

- [`GNOpenAPIManager$clone()`](#method-GNOpenAPIManager-clone)

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

This method is used to instantiate a `GNOpenAPIManager` with the `url`
of the GeoNetwork and credentials to authenticate (`user`/`pwd`).

The `keyring_backend` can be set to use a different backend for storing
the Geonetwork password/token with keyring (Default value is 'env').

The logger can be either NULL, "INFO" (with minimum logs), or "DEBUG"
(for complete curl http calls logs)

#### Usage

    GNOpenAPIManager$new(
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

  keyring backend

------------------------------------------------------------------------

### Method `login()`

This methods attempts a connection to GeoNetwork REST API. User
internally during initialization of `GNLegacyAPIManager`.

#### Usage

    GNOpenAPIManager$login(user, pwd)

#### Arguments

- `user`:

  user

- `pwd`:

  pwd

------------------------------------------------------------------------

### Method `getGroups()`

Retrieves the list of user groups available in Geonetwork

#### Usage

    GNOpenAPIManager$getGroups()

#### Returns

an object of class `data.frame`

------------------------------------------------------------------------

### Method `getTags()`

Retrieves the list of tags (categories) available in Geonetwork

#### Usage

    GNOpenAPIManager$getTags()

#### Returns

an object of class `data.frame`

------------------------------------------------------------------------

### Method `getCategories()`

Retrieves the list of categories (same as tags) available in Geonetwork

#### Usage

    GNOpenAPIManager$getCategories()

#### Returns

an object of class `data.frame`

------------------------------------------------------------------------

### Method `downloadMetadataByUUID()`

Download a metadata by UUID.

#### Usage

    GNOpenAPIManager$downloadMetadataByUUID(
      uuid,
      addSchemaLocation = TRUE,
      increasePopularity = TRUE,
      approved = TRUE,
      filename
    )

#### Arguments

- `uuid`:

  uuid

- `addSchemaLocation`:

  add schema location. Default is `TRUE`

- `increasePopularity`:

  increase popularity. Default is `TRUE`

- `approved`:

  approved

- `filename`:

  output filename

------------------------------------------------------------------------

### Method `getMetadataByUUID()`

Get a metadata by UUID.

#### Usage

    GNOpenAPIManager$getMetadataByUUID(
      uuid,
      addSchemaLocation = TRUE,
      increasePopularity = TRUE,
      approved = TRUE
    )

#### Arguments

- `uuid`:

  uuid

- `addSchemaLocation`:

  add schema location. Default is `TRUE`

- `increasePopularity`:

  increase popularity. Default is `TRUE`

- `approved`:

  approved

#### Returns

Returns an object of class `ISOMetadata` (ISO 19115) or
`ISOFeatureCatalogue` (ISO 19110) (from geometa package)

------------------------------------------------------------------------

### Method `insertRecord()`

Inserts a record by file, XML object or geometa object of class
[ISOMetadata](https://rdrr.io/pkg/geometa/man/ISOMetadata.html) or
[ISOFeatureCatalogue](https://rdrr.io/pkg/geometa/man/ISOFeatureCatalogue.html).
Extra parameters related to geometa objects: `geometa_validate` (TRUE by
default) and `geometa_inspire` (FALSE by default) can be used to perform
ISO and INSPIRE validation respectively. argument.

#### Usage

    GNOpenAPIManager$insertRecord(
      xml = NULL,
      file = NULL,
      geometa = NULL,
      metadataType = "METADATA",
      uuidProcessing = "NOTHING",
      group,
      category = NULL,
      rejectIfInvalid = FALSE,
      publishToAll = TRUE,
      transformWith = "_none_",
      schema = NULL,
      extra = NULL,
      geometa_validate = TRUE,
      geometa_inspire = FALSE,
      geometa_inspireValidator = NULL
    )

#### Arguments

- `xml`:

  object of class
  [XMLInternalNode-class](https://rdrr.io/pkg/XML/man/XMLNode-class.html)

- `file`:

  file

- `geometa`:

  geometa object of class
  [ISOMetadata](https://rdrr.io/pkg/geometa/man/ISOMetadata.html) or
  [ISOFeatureCatalogue](https://rdrr.io/pkg/geometa/man/ISOFeatureCatalogue.html)

- `metadataType`:

  metadata type. By default `METADATA`

- `uuidProcessing`:

  UUID processing. By default `NOTHING`. Other possible value:
  `OVERWRITE`

- `group`:

  group

- `category`:

  category

- `rejectIfInvalid`:

  reject if invalid. Default `FALSE`

- `publishToAll`:

  publish to all. Default `TRUE`

- `transformWith`:

  transform with. Default is `_none_`

- `schema`:

  schema

- `extra`:

  extra

- `geometa_validate`:

  validate geometa object

- `geometa_inspire`:

  validate geometa object vs. INSPIRE

- `geometa_inspireValidator`:

  geometa INSPIRE validator to use. Deprecated, see below note.

------------------------------------------------------------------------

### Method `insertMetadata()`

Inserts a metadata by file, XML object or geometa object of class
[ISOMetadata](https://rdrr.io/pkg/geometa/man/ISOMetadata.html) or
[ISOFeatureCatalogue](https://rdrr.io/pkg/geometa/man/ISOFeatureCatalogue.html).
Extra parameters related to geometa objects: `geometa_validate` (TRUE by
default) and `geometa_inspire` (FALSE by default) can be used to perform
ISO and INSPIRE validation respectively.

#### Usage

    GNOpenAPIManager$insertMetadata(
      xml = NULL,
      file = NULL,
      geometa = NULL,
      metadataType = "METADATA",
      uuidProcessing = "NOTHING",
      group,
      category = NULL,
      rejectIfInvalid = FALSE,
      publishToAll = TRUE,
      transformWith = "_none_",
      schema = NULL,
      extra = NULL,
      geometa_validate = TRUE,
      geometa_inspire = FALSE,
      geometa_inspireValidator = NULL
    )

#### Arguments

- `xml`:

  object of class
  [XMLInternalNode-class](https://rdrr.io/pkg/XML/man/XMLNode-class.html)

- `file`:

  file

- `geometa`:

  geometa object of class
  [ISOMetadata](https://rdrr.io/pkg/geometa/man/ISOMetadata.html) or
  [ISOFeatureCatalogue](https://rdrr.io/pkg/geometa/man/ISOFeatureCatalogue.html)

- `metadataType`:

  metadata type. By default `METADATA`

- `uuidProcessing`:

  UUID processing. By default `NOTHING`. Other possible value:
  `OVERWRITE`

- `group`:

  group

- `category`:

  category

- `rejectIfInvalid`:

  reject if invalid. Default `FALSE`

- `publishToAll`:

  publish to all. Default `TRUE`

- `transformWith`:

  transform with. Default is `_none_`

- `schema`:

  schema

- `extra`:

  extra

- `geometa_validate`:

  validate geometa object

- `geometa_inspire`:

  validate geometa object vs. INSPIRE

- `geometa_inspireValidator`:

  geometa INSPIRE validator to use. Deprecated, see below note.

------------------------------------------------------------------------

### Method `updateMetadata()`

Inserts a metadata by file, XML object or geometa object of class
[ISOMetadata](https://rdrr.io/pkg/geometa/man/ISOMetadata.html) or
[ISOFeatureCatalogue](https://rdrr.io/pkg/geometa/man/ISOFeatureCatalogue.html).
Extra parameters related to geometa objects: `geometa_validate` (TRUE by
default) and `geometa_inspire` (FALSE by default) can be used to perform
ISO and INSPIRE validation respectively.

#### Usage

    GNOpenAPIManager$updateMetadata(
      xml = NULL,
      file = NULL,
      geometa = NULL,
      metadataType = "METADATA",
      group,
      category = NULL,
      rejectIfInvalid = FALSE,
      publishToAll = TRUE,
      transformWith = "_none_",
      schema = NULL,
      extra = NULL,
      geometa_validate = TRUE,
      geometa_inspire = FALSE,
      geometa_inspireValidator = NULL
    )

#### Arguments

- `xml`:

  object of class
  [XMLInternalNode-class](https://rdrr.io/pkg/XML/man/XMLNode-class.html)

- `file`:

  file

- `geometa`:

  geometa object of class
  [ISOMetadata](https://rdrr.io/pkg/geometa/man/ISOMetadata.html) or
  [ISOFeatureCatalogue](https://rdrr.io/pkg/geometa/man/ISOFeatureCatalogue.html)

- `metadataType`:

  metadata type. By default `METADATA`

- `group`:

  group

- `category`:

  category

- `rejectIfInvalid`:

  reject if invalid. Default `FALSE`

- `publishToAll`:

  publish to all. Default `TRUE`

- `transformWith`:

  transform with. Default is `_none_`

- `schema`:

  schema

- `extra`:

  extra

- `geometa_validate`:

  validate geometa object

- `geometa_inspire`:

  validate geometa object vs. INSPIRE

- `geometa_inspireValidator`:

  geometa INSPIRE validator to use. Deprecated, see below note.

------------------------------------------------------------------------

### Method `deleteMetadata()`

Deletes a metadata by ID

#### Usage

    GNOpenAPIManager$deleteMetadata(id, withBackup = TRUE)

#### Arguments

- `id`:

  id

- `withBackup`:

  proceed with backup. Default is `TRUE`

------------------------------------------------------------------------

### Method `uploadAttachment()`

Uploads attachment

#### Usage

    GNOpenAPIManager$uploadAttachment(
      id,
      file,
      visibility = "public",
      approved = TRUE
    )

#### Arguments

- `id`:

  metadata identifier

- `file`:

  file to upload

- `visibility`:

  public or private

- `approved`:

  object of class `logical`

#### Returns

a named list of the uploaded attachment, including the url, size, id and
type, `NULL` otherwise

------------------------------------------------------------------------

### Method `publishThumbnail()`

Publishes thumbnail based on URL

#### Usage

    GNOpenAPIManager$publishThumbnail(id, url, desc = "")

#### Arguments

- `id`:

  metadata identifier

- `url`:

  thumbnail URL

- `desc`:

  thumbnail description

#### Returns

`TRUE` if published, `FALSE` otherwise

------------------------------------------------------------------------

### Method `doiCheckPreConditions()`

Checks pre-conditions to publish DOI

#### Usage

    GNOpenAPIManager$doiCheckPreConditions(id)

#### Arguments

- `id`:

  metadata identifier

#### Returns

`TRUE` if DOI pre-conditions are fulfiled, `FALSE` otherwise

------------------------------------------------------------------------

### Method `createDOI()`

Submit a record to the Datacite metadata store in order to create a DOI.

#### Usage

    GNOpenAPIManager$createDOI(id)

#### Arguments

- `id`:

  metadata identifier

#### Returns

`TRUE` if metadata record has been submitted with DOI created, `FALSE`
otherwise

------------------------------------------------------------------------

### Method `deleteDOI()`

Remove a DOI (this is not recommended, DOI are supposed to be persistent
once created. This is mainly here for testing).

#### Usage

    GNOpenAPIManager$deleteDOI(id)

#### Arguments

- `id`:

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GNOpenAPIManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \dontrun{
   GNOpenAPIManager$new("http://localhost:8080/geonetwork", "admin", "geonetwork", "4.0.5")
#> Error in curl::curl_fetch_memory(url, handle = handle): Couldn't connect to server [localhost]:
#> Failed to connect to localhost port 8080 after 0 ms: Couldn't connect to server
# }
```
