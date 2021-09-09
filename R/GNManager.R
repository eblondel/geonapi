#' GeoNetwork REST API Manager
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom openssl base64_encode
#' @import httr
#' @import XML
#' @import keyring
#' @import geometa
#' 
#' @export
#' @keywords geonetwork rest api
#' @description The function \code{GNManager$new} will set-up the right Geonetwork
#' manager depending on the GeoNetwork version specified by the user. For the time-being,
#' GeoNetwork with version < 4 will be interfaced with the GeoNetwork legacy API (see detailed 
#' documentation at \link{GNLegacyAPIManager}), while starting with GeoNetwork 4, the new 
#' GeoNetwork OpenAPI will be used.
#' @return Object of \code{\link{R6Class}} with methods for communication with
#' the API of a GeoNetwork instance.
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'    GMManager$new("http://localhost:8080/geonetwork", "admin", "geonetwork", "3.0.0")
#' }
#'
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
GNManager <- R6Class("GNManager",
  inherit = GNAbstractManager,
  lock_class = FALSE,
  private = list(),
  public = list(
    initialize = function(url, user = NULL, pwd = NULL, version, logger = NULL){
      #nothing done:
      #GNManager$new becomes static to delegate to appropriate manager
    }
  )
)

GNManager$new <- function(url, user = NULL, pwd = NULL, version, logger = NULL){
  gn_version <- GNVersion$new(version = version)
  if(gn_version$value$major < 4){ #to be refine to some 3.x versions if needed
    return(GNLegacyAPIManager$new(url, user = user, pwd = pwd, version = version, logger = logger))
  }else{
    return(GNOpenAPIManager$new(url, user = user, pwd = pwd, version = version, logger = logger))
  }
}
