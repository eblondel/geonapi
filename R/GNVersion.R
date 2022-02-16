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
     #'@field version version
     version = NULL,
     #'@field value value
     value = NULL,
     #'@description Initializes an object of class \link{GNVersion}
     #'@param version version
     initialize = function(version){ 
       value <- private$getVersionValue(version)
       if(is.list(value)){
         self$version <- version
         self$value <- value
       }
     },
     
     #'@description  Compares to a version and returns TRUE if it is lower, FALSE otherwise
     #'@param version version
     #'@return \code{TRUE} if lower, \code{FALSE} otherwise
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
     
     #'@description  Compares to a version and returns TRUE if it is greater, FALSE otherwise
     #'@param version version
     #'@return \code{TRUE} if lower, \code{FALSE} otherwise
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
     
     #'@description  Compares to a version and returns TRUE if it is equal, FALSE otherwise
     #'@param version version
     #'@return \code{TRUE} if lower, \code{FALSE} otherwise
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