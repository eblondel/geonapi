% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GNManager.R
\docType{class}
\name{GNManager}
\alias{GNManager}
\title{GeoNetwork REST API Manager}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} with methods for communication with
the REST API of a GeoNetwork instance.
}
\description{
GeoNetwork REST API Manager
}
\section{Fields}{

\describe{
\item{\code{loggerType}}{the type of logger}

\item{\code{verbose.info}}{if geosapi logs have to be printed}

\item{\code{verbose.debug}}{if curl logs have to be printed}

\item{\code{url}}{the Base url of GeoNetwork}

\item{\code{version}}{the version of GeoNetwork. Handled as \code{GNVersion} object}

\item{\code{user}}{the user name}
}}

\section{Methods}{

\describe{
 \item{\code{new(url, user, pwd, version, logger)}}{
   This method is used to instantiate a GNManager with the \code{url} of the
   GeoNetwork and credentials to authenticate (\code{user}/\code{pwd}). By default,
   the \code{logger} argument will be set to \code{NULL} (no logger). This argument
   accepts two possible values: \code{INFO}: to print only geonapi logs,
   \code{DEBUG}: to print geonapi and CURL logs
 }
 \item{\code{logger(type, text)}}{
   Basic logger to report geonapi logs. Used internally
 }
 \item{\code{INFO(text)}}{
   Logger to report information. Used internally
 }
 \item{\code{WARN(text)}}{
   Logger to report warnings. Used internally
 }
 \item{\code{ERROR(text)}}{
   Logger to report errors. Used internally
 }
 \item{\code{getUrl()}}{
   Get the authentication URL
 }
 \item{\code{getLang()}}{
   Get the service lang
 }
 \item{\code{login(user, pwd)}}{
   This methods attempts a connection to GeoNetwork REST API. User internally
   during initialization of \code{GNManager}.
 }
 \item{\code{getCookies()}}{
   Get user session cookies
 }
 \item{\code{getToken()}}{
   Get user session token
 }
 \item{\code{getClassName()}}{
   Retrieves the name of the class instance
 }
 \item{\code{getGroups()}}{
   Retrives the list of user groups available in Geonetwork
 }
 \item{\code{insertMetadata(xml, file, geometa, group, category, stylesheet, validate, geometa_validate, geometa_inspire)}}{
   Inserts a metadata by file, XML object or \pkg{geometa} object of class
   \code{ISOMetadata} or \code{ISOFeatureCatalogue}. If successful, returns the Geonetwork
   metadata internal identifier (integer). Extra parameters \code{geometa_validate} (TRUE 
   by default) and \code{geometa_inspire} (FALSE by default) can be used with geometa objects 
   for perform ISO and INSPIRE validation respectively.
 }
 \item{\code{setPrivConfiguration(id, config)}}{
   Set the privilege configuration for a metadata. 'id' is the metadata integer id.
   'config' is an object of class "GNPrivConfiguration".
 }
 \item{\code{get(id, by, output)}}{
   Generic getter for metadata. Possible values for by are 'id', 'uuid'. Used
   internally only. The 'output' argument gives the type of output to return,
   with possible values "id", "metadata", "info".
 }
 \item{\code{getMetadataByID(id)}}{
   Get a metadata by Id. Returns an object of class \code{ISOMetadata} (ISO 19115)
   or \code{ISOFeatureCatalogue} (ISO 19110) (from \pkg{geometa} package)
 }
 \item{\code{getMetadataByUUID(uuid)}}{
   Get a metadata by UUID. Returns an object of class \code{ISOMetadata} (ISO 19115)
   or \code{ISOFeatureCatalogue} (ISO 19110) (from \pkg{geometa} package)
 }
 \item{\code{getInfoByID(id)}}{
   Get a metadata Info by Id. Returns an XML document object
 }
 \item{\code{getInfoByUUID(uuid)}}{
   Get a metadata Info by UUID. Returns an XML document object
 }
 \item{\code{updateMetadata(id, xml, file, geometa, geometa_validate, geometa_inspire)}}{
   Updates a metadata by file, XML object or \pkg{geometa} object of class
   'ISOMetadata' or 'ISOFeatureCatalogue'. Extra parameters \code{geometa_validate} (TRUE 
   by default) and \code{geometa_inspire} (FALSE by default) can be used with geometa objects 
   for perform ISO and INSPIRE validation respectively.
 }
 \item{\code{deleteMetadata(id)}}{
   Deletes a metadata
 }
 \item{\code{deleteMetadataAll()}}{
   Deletes all metadata for which the authenticated user is owner
 }
}
}

\examples{
\dontrun{
   GMManager$new("http://localhost:8080/geonetwork", "admin", "geonetwork")
}

}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{api}
\keyword{geonetwork}
\keyword{rest}
