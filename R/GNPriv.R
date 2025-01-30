#' GeoNetwork REST API - GeoNetwork privilege configuration
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name GNPriv
#' @title A GeoNetwork privilege configuration
#' @description This class is an utility to configure privileges
#' @keywords GeoNetwork privilege configuration
#' @return Object of \link[R6]{R6Class} for modelling a GeoNetwork Privilege configuration
#' @format \link[R6]{R6Class} object.
#' 
#' @examples
#' \dontrun{
#'  priv <- GNPriv$new(group="all", privileges=c("view","dynamic","featured"))
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GNPriv <- R6Class("GNPriv",
   public = list(
     #'@field group group
     group = NULL,
     #'@field privileges privileges
     privileges = list(),
     
     #'@description Initializes a \code{GNPriv} object
     #'@param group group
     #'@param privileges privileges
     initialize = function(group, privileges){
       allowedGroups <- c("guest", "intranet", "all")
       if(!(group %in% allowedGroups)){
         stop(sprintf("Unsupported group '%s' value. Possible values are [%s]",
                      group, paste0(allowedGroups, collapse=",")))
       }
       groupCode <- switch(group, "guest" = -1, "intranet" = 0, "all" = 1)
       allowedPrivileges <- c("view", "download", "editing", "notify", "dynamic", "featured")
       if(!all(privileges %in% allowedPrivileges)){
         stop(sprintf("One or more privilege(s) not matching possibles [%s]",
                      paste0(allowedPrivileges, collapse=",")))
       }
       privCodes <- as.integer(sapply(privileges, function(x){
         code <- switch(x, "view" = 0, "download" = 1, "editing" = 2,
                        "notify" = 3, "dynamic" = 5, "featured" = 6)
         return(code)
       }))
       
       self$group = groupCode
       self$privileges = privCodes
     }
   )                  
)