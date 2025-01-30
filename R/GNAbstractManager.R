#' @name GNAbstractManager
#' @title GNAbstractManager
#' @aliases GNAbstractManager
#'
#' @docType class
#' 
#' @export
#' @keywords geonetwork rest api
#' @return Object of \code{\link[R6]{R6Class}} with methods for communication with
#' the REST API of a GeoNetwork instance.
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
GNAbstractManager <- R6Class("GNAbstractManager",
   
   #TODO provider specific formatter to prevent these fields to be printable
   private = list(
     keyring_backend = NULL,
     keyring_service = NULL,
     user = NULL,
     cookies = NULL,
     
     #getPwd
     getPwd = function(){
       pwd <- NULL
       if(!is.null(private$keyring_service)){
         keyring_pwd <- suppressWarnings(try(private$keyring_backend$get(service = private$keyring_service, username = paste0(private$user, "_pwd")), silent = TRUE))
         if(!is(keyring_pwd, "try-error")) pwd <- keyring_pwd
       }
       return(pwd)
     },
     
     #getToken (if existing)
     getToken = function(){
       token <- NULL
       if(!is.null(private$keyring_service)){
         keyring_token <- suppressWarnings(try(private$keyring_backend$get(service = private$keyring_service, username = paste0(private$user, "_token")), silent = TRUE))
         if(!is(keyring_token, "try-error")) token <- keyring_token
       }
       return(token)
     }
   ),
   
   public = list(
      #'@field verbose.info If package info log messages have to be printed out
     verbose.info = FALSE,
     #'@field verbose.debug If curl debug log messages have to be printed out
     verbose.debug = FALSE,
     #' @field loggerType the type of logger
     loggerType = NULL,
     #'@description Provides log messages
     #'@param type type of log ("INFO", "WARN", "ERROR")
     #'@param text the log message text
     logger = function(type, text){
       if(self$verbose.info){
         cat(sprintf("[geonapi][%s] %s \n", type, text))
       }
     },
     #'@description Provides INFO log messages
     #'@param text the log message text
     INFO = function(text){self$logger("INFO", text)},
     #'@description Provides WARN log messages
     #'@param text the log message text
     WARN = function(text){self$logger("WARN", text)},
     #'@description Provides ERROR log messages
     #'@param text the log message text
     ERROR = function(text){self$logger("ERROR", text)},
     
     #'@field url the Base url of GeoNetwork
     url = NA,
     #'@field version the version of GeoNetwork. Handled as \code{GNVersion} object
     version = NULL,
     #' @field lang the language for Geonetwork service. Default is \code{eng}
     lang = "eng",
     #'@field basicAuth if basic auth is performed
     basicAuth = FALSE,
     
     #'@description This method is used to instantiate a \link{GNAbstractManager} with the \code{url} of the
     #'    GeoNetwork and credentials to authenticate (\code{user}/\code{pwd}). By default,
     #'    the \code{logger} argument will be set to \code{NULL} (no logger).
     #'    
     #'    The \code{keyring_backend} can be set to use a different backend for storing 
     #'    the Geonetwork password/token with \pkg{keyring} (Default value is 'env').
     #'    
     #'    The logger can be either NULL, "INFO" (with minimum logs), or "DEBUG" 
     #'    (for complete curl http calls logs)
     #'    
     #' @param url url
     #' @param user user
     #' @param pwd pwd
     #' @param version version
     #' @param logger logger
     #' @param keyring_backend keyring backend. Default is 'env'
     #'
     initialize = function(url, user = NULL, pwd = NULL, version, logger = NULL,
                           keyring_backend = 'env'){
       
       #logger
       if(!missing(logger)){
         if(!is.null(logger)){
           self$loggerType <- toupper(logger)
           if(!(self$loggerType %in% c("INFO","DEBUG"))){
             stop(sprintf("Unknown logger type '%s", logger))
           }
           if(self$loggerType == "INFO"){
             self$verbose.info = TRUE
           }else if(self$loggerType == "DEBUG"){
             self$verbose.info = TRUE
             self$verbose.debug = TRUE
           }
         }
       }
       
       #GN version
       if(!is(version, "character")) stop("The version should be an object of class 'character'")
       self$version <- GNVersion$new(version = version)
       
       #keyring backend
       if(!keyring_backend %in% names(keyring:::known_backends)){
          errMsg <- sprintf("Backend '%s' is not a known keyring backend!", keyring_backend)
          self$ERROR(errMsg)
          stop(errMsg)
       }
       private$keyring_backend <- keyring:::known_backends[[keyring_backend]]$new()
       
       #baseUrl
       self$url = sprintf("%s/srv", url)
       private$keyring_service <- paste0("geonapi@", url)
       
       invisible(self)
     },
     
     #'@description Get URL
     #'@return an object of class \code{character}
     getUrl = function(){
       return(self$url)
     },
     
     #'@description Get service language
     #'@return an object of class \code{character}
     getLang = function(){
       return(self$lang)
     },
     
     #'@description Log-ins. This methods (here abstract) attempts a connection to GeoNetwork API. Used internally
     #'  by subclasses of \link{GNAbstractManager} to login Geonetwork.
     #'@param user user
     #'@param pwd pwd
     login = function(user, pwd){
        stop("Login method not implemented")
     },
     
     #'@description Get class name
     #'@return an object of class \code{character}  
     getClassName = function(){
       return(class(self)[1])
     }
   )
                     
)
