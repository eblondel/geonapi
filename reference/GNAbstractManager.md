# GNAbstractManager

GNAbstractManager

GNAbstractManager

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object.

## Value

Object of [`R6Class`](https://r6.r-lib.org/reference/R6Class.html) with
methods for communication with the REST API of a GeoNetwork instance.

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Public fields

- `verbose.info`:

  If package info log messages have to be printed out

- `verbose.debug`:

  If curl debug log messages have to be printed out

- `loggerType`:

  the type of logger

- `url`:

  the Base url of GeoNetwork

- `version`:

  the version of GeoNetwork. Handled as `GNVersion` object

- `lang`:

  the language for Geonetwork service. Default is `eng`

- `basicAuth`:

  if basic auth is performed

## Methods

### Public methods

- [`GNAbstractManager$logger()`](#method-GNAbstractManager-logger)

- [`GNAbstractManager$INFO()`](#method-GNAbstractManager-INFO)

- [`GNAbstractManager$WARN()`](#method-GNAbstractManager-WARN)

- [`GNAbstractManager$ERROR()`](#method-GNAbstractManager-ERROR)

- [`GNAbstractManager$new()`](#method-GNAbstractManager-new)

- [`GNAbstractManager$getUrl()`](#method-GNAbstractManager-getUrl)

- [`GNAbstractManager$getLang()`](#method-GNAbstractManager-getLang)

- [`GNAbstractManager$login()`](#method-GNAbstractManager-login)

- [`GNAbstractManager$getClassName()`](#method-GNAbstractManager-getClassName)

- [`GNAbstractManager$clone()`](#method-GNAbstractManager-clone)

------------------------------------------------------------------------

### Method `logger()`

Provides log messages

#### Usage

    GNAbstractManager$logger(type, text)

#### Arguments

- `type`:

  type of log ("INFO", "WARN", "ERROR")

- `text`:

  the log message text

------------------------------------------------------------------------

### Method `INFO()`

Provides INFO log messages

#### Usage

    GNAbstractManager$INFO(text)

#### Arguments

- `text`:

  the log message text

------------------------------------------------------------------------

### Method `WARN()`

Provides WARN log messages

#### Usage

    GNAbstractManager$WARN(text)

#### Arguments

- `text`:

  the log message text

------------------------------------------------------------------------

### Method `ERROR()`

Provides ERROR log messages

#### Usage

    GNAbstractManager$ERROR(text)

#### Arguments

- `text`:

  the log message text

------------------------------------------------------------------------

### Method `new()`

This method is used to instantiate a GNAbstractManager with the `url` of
the GeoNetwork and credentials to authenticate (`user`/`pwd`). By
default, the `logger` argument will be set to `NULL` (no logger).

The `keyring_backend` can be set to use a different backend for storing
the Geonetwork password/token with keyring (Default value is 'env').

The logger can be either NULL, "INFO" (with minimum logs), or "DEBUG"
(for complete curl http calls logs)

#### Usage

    GNAbstractManager$new(
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

### Method `getUrl()`

Get URL

#### Usage

    GNAbstractManager$getUrl()

#### Returns

an object of class `character`

------------------------------------------------------------------------

### Method `getLang()`

Get service language

#### Usage

    GNAbstractManager$getLang()

#### Returns

an object of class `character`

------------------------------------------------------------------------

### Method `login()`

Log-ins. This methods (here abstract) attempts a connection to
GeoNetwork API. Used internally by subclasses of GNAbstractManager to
login Geonetwork.

#### Usage

    GNAbstractManager$login(user, pwd)

#### Arguments

- `user`:

  user

- `pwd`:

  pwd

------------------------------------------------------------------------

### Method `getClassName()`

Get class name

#### Usage

    GNAbstractManager$getClassName()

#### Returns

an object of class `character`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GNAbstractManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
