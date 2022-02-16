#' @name GNAbstractManager
#' @title GNAbstractManager
#' @aliases GNAbstractManager
#'
#' @docType class
#' 
#' @export
#' @keywords geonetwork rest api
#' @return Object of \code{\link{R6Class}} with methods for communication with
#' the REST API of a GeoNetwork instance.
#' @format \code{\link{R6Class}} object.
#'
#' @field loggerType the type of logger
#' @field verbose.info if geosapi logs have to be printed
#' @field verbose.debug if curl logs have to be printed
#' @field url the Base url of GeoNetwork
#' @field version the version of GeoNetwork. Handled as \code{GNVersion} object
#' @field lang the language for Geonetwork service. Default is \code{eng}
#' @field user the user name
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(url, user, pwd, version, logger, keyring_backend)}}{
#'    This method is used to instantiate a GNManager with the \code{url} of the
#'    GeoNetwork and credentials to authenticate (\code{user}/\code{pwd}). By default,
#'    the \code{logger} argument will be set to \code{NULL} (no logger).
#'    
#'    The \code{keyring_backend} can be set to use a different backend for storing 
#'    the Geonetwork password/token with \pkg{keyring} (Default value is 'env').
#'    
#'    The logger can be either NULL, "INFO" (with minimum logs), or "DEBUG" 
#'    (for complete curl http calls logs)
#'  }
#'  \item{\code{logger(type, text)}}{
#'    Basic logger to report geonapi logs. Used internally
#'  }
#'  \item{\code{INFO(text)}}{
#'    Logger to report information. Used internally
#'  }
#'  \item{\code{WARN(text)}}{
#'    Logger to report warnings. Used internally
#'  }
#'  \item{\code{ERROR(text)}}{
#'    Logger to report errors. Used internally
#'  }
#'  \item{\code{getUrl()}}{
#'    Get the authentication URL
#'  }
#'  \item{\code{getLang()}}{
#'    Get the service lang
#'  }
#'  \item{\code{login(user, pwd)}}{
#'    This methods (here abstract) attempts a connection to GeoNetwork API. Used internally
#'    by subclass of \code{GNAbstractManager} to login Geonetwork.
#'  }
#'  \item{\code{getClassName()}}{
#'    Retrieves the name of the class instance
#'  }
#'}
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
     #logger
     verbose.info = FALSE,
     verbose.debug = FALSE,
     loggerType = NULL,
     logger = function(type, text){
       if(self$verbose.info){
         cat(sprintf("[geonapi][%s] %s \n", type, text))
       }
     },
     INFO = function(text){self$logger("INFO", text)},
     WARN = function(text){self$logger("WARN", text)},
     ERROR = function(text){self$logger("ERROR", text)},
     
     #manager
     url = NA,
     version = NULL,
     lang = "eng",
     basicAuth = FALSE,
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
     
     #getUrl
     #---------------------------------------------------------------------------
     getUrl = function(){
       return(self$url)
     },
     
     #getLang
     #---------------------------------------------------------------------------
     getLang = function(){
       return(self$lang)
     },
     
     #login
     #---------------------------------------------------------------------------
     login = function(user, pwd){
        stop("Login method not implemented")
     },
     
     #getClassName
     #---------------------------------------------------------------------------    
     getClassName = function(){
       return(class(self)[1])
     }
   )
                     
)
