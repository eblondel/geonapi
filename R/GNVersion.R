#' GeoNetwork REST API - GeoNetwork Version
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name GNVersion
#' @title A GeoNetwork version
#' @description This class is an utility wrap the Geonetwork version
#' @keywords GeoNetwork version
#' @return Object of \code{\link{R6Class}} for modelling a GeoNetwork version
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#' version <- GNVersion$new("2.6.4")
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(version)}}{
#'    This method is used to instantiate a GNVersion object.
#'  }
#'  \item{\code{lowerThan(version)}}{
#'    Compares to a version and returns TRUE if it is lower, FALSE otherwise
#'  }
#'  \item{\code{greaterThan(version)}}{
#'    Compares to a version and returns TRUE if it is greater, FALSE otherwise
#'  }
#'  \item{\code{equalTo(version)}}{
#'    Compares to a version and returns TRUE if it is equal, FALSE otherwise
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GNVersion <- R6Class("GNVersion", 
   private = list(
     getVersionValue = function(version){
       version <- gsub("x", "0", version)
       version <- gsub("-SNAPSHOT", "", version)
       versions <- unlist(strsplit(version, "\\."))
       value <- list()
       value[["major"]] <- as.integer(versions[1])
       value[["minor"]] <- as.integer(versions[2])
       value[["revision"]] <- 0
       if(length(versions)==3){
         value[["revision"]] <- as.integer(versions[3])
       }
       return(value)
     }
   ),
   
   public = list(
     version = NULL,
     value = NULL,
     initialize = function(version){ 
       value <- private$getVersionValue(version)
       if(is.list(value)){
         self$version <- version
         self$value <- value
       }
     },
     
     #lowerThan
     #---------------------------------------------------------------------------
     lowerThan = function(version){
       lower <- FALSE
       if(is.character(version)){
         value <- private$getVersionValue(version)
       }else if(is.list(version)){
         value <- version
       }
       lower <- (self$value$major < value$major)
       if(!lower & identical(self$value$major, value$major)){
         lower <- (self$value$minor < value$minor)
       }
       if(!lower & identical(self$value$minor, value$minor)){
         lower <- (self$value$revision < value$revision)
       }
       return(lower)
     },
     
     #greaterThan
     #---------------------------------------------------------------------------
     greaterThan = function(version){
       greater <- FALSE
       if(is.character(version)){
         value <- private$getVersionValue(version)
       }else if(is.list(version)){
         value <- version
       }
       greater <- (self$value$major > value$major)
       if(!greater & identical(self$value$major, value$major)){
         greater <- (self$value$minor > value$minor)
       } 
       if(!greater & identical(self$value$minor, value$minor)){
         greater <- (self$value$revision > value$revision)
       }
       return(greater)
     },
     
     #equalTo
     #---------------------------------------------------------------------------
     equalTo = function(version){
       equal <- FALSE
       if(is.character(version)){
         value <- private$getVersionValue(version)
       }else if(is.list(version)){
         value <- version
       }
       equal <- !self$lowerThan(version) & !self$greaterThan(version)
       return(equal)
     }
     
   )                  
)