#' GeoNetwork REST API - GeoNetwork privilege configuration
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name GNPrivConfiguration
#' @title A GeoNetwork privilege configuration
#' @description This class is an utility to configure privileges
#' @keywords GeoNetwork privilege configuration
#' @return Object of \code{\link{R6Class}} for modelling a GeoNetwork Privilege configuration
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'  pcfg <- GNPrivConfiguration$new()
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate a GNPrivConfiguration object.
#'  }
#'  \item{\code{setPrivileges(group, privileges)}}{
#'    Sets the operation privileges for a particular group. Allowed group values
#'    are "guest","intranet" and "all". Allowed values for operation privileges
#'    are "view", "download", "editing", "notify", "dynamic" and "featured".
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GNPrivConfiguration <- R6Class("GNPrivConfiguration",
   public = list(
     privileges = list(),
     initialize = function(){},
     
     #setPrivileges
     #--------------------------------------------------------------------------
     setPrivileges = function(group, privileges){
       priv <- GNPriv$new(group = group, privileges = privileges)
       groupValues <- sapply(self$privileges, function(x){return(x$group)})
       if(priv$group %in% groupValues){
         self$privileges <- self$privileges[which(groupValues != priv$group)]
       }
       self$privileges <- c(self$privileges, priv)
     }
   )                  
)