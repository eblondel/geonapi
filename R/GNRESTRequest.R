#' GeoNetwork REST API REST Request
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords geonetwork rest api
#' @return Object of \code{\link{R6Class}} for modelling a GeoNetwork REST request
#' @format \code{\link{R6Class}} object.
#'
#' @section Abstract Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate a GNRESTRequest
#'  }
#'  \item{\code{setChild(key,value)}}{
#'    Sets a child element
#'  }
#'  \item{\code{encode()}}{
#'    Encodes a GNRESTRequest R6 object to XML representation
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GNRESTRequest <- R6Class("GNRESTRequest",
  public = list(
    rootName = NULL,
    children = list(),
    initialize = function(...){
      self$rootName = "request"
      self$children = list(...)
    },
    
    setChild = function(key, value){
      self$children[[key]] <- value
    },
    
    encode = function(){
      rootXML <- newXMLNode(self$rootName)
      for(childName in names(self$children)){
        child <- self$children[[childName]]
        childXML <- newXMLNode(childName, child, parent = rootXML)
      }
      return(rootXML)
    }
  )                     
)