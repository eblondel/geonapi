# GeoNetwork REST API Manager Utils

GeoNetwork REST API Manager Utils

GeoNetwork REST API Manager Utils

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object.

## Value

Object of [`R6Class`](https://r6.r-lib.org/reference/R6Class.html) with
static util methods for communication with the REST API of a GeoNetwork
instance.

## Static methods

- `getUserAgent()`:

  This method is used to get the user agent for performing GeoNetwork
  API requests. Here the user agent will be compound by geonapi package
  name and version.

- `getUserToken(user, pwd)`:

  This method is used to get the user authentication token for
  performing GeoNetwork API requests. Token is given a Base64 encoded
  string.

- `GET(url, path, token, verbose)`:

  This method performs a GET request for a given `path` to GeoNetwork
  REST API

- `PUT(url, path, token, filename, contentType, verbose)`:

  This method performs a PUT request for a given `path` to GeoNetwork
  REST API, to upload a file of name `filename` with given `contentType`

- `POST(url, path, token, content, contentType, encode, verbose)`:

  This method performs a POST request for a given `path` to GeoNetwork
  REST API, to post content of given `contentType`

- `DELETE(url, path, token, verbose)`:

  This method performs a DELETE request for a given GeoNetwork resource
  identified by a `path` in GeoNetwork REST API

- `parseResponseXML(req)`:

  Convenience method to parse XML response from GeoNetwork REST API.
  Although package httr suggests the use of xml2 package for handling
  XML, geonapi still relies on the package XML. Response from httr is
  retrieved as text, and then parsed as XML 'xmlParse' function.

- `getPayloadXML(obj)`:

  Convenience method to create payload XML to send to GeoNetwork.

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Methods

### Public methods

- [`GNUtils$clone()`](#method-GNUtils-clone)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GNUtils$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
