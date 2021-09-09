#' @name GNOpenAPIManager
#' @title GNOpenAPIManager
#' @aliases GNOpenAPIManager
#'
#' @docType class
#' 
#' @export
#' @keywords geonetwork rest api
#' @return Object of \code{\link{R6Class}} with methods for communication with
#' the REST API of a GeoNetwork instance using the legacy API.
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'    GNOpenAPIManager$new("http://localhost:8080/geonetwork", "admin", "geonetwork", "4.0.5")
#' }
#'
#'@section Abstract Methods:
#' \describe{
#'  \item{\code{new(url, user, pwd, version, logger)}}{
#'    This method is used to instantiate a \code{GNOpenAPIManager} with the \code{url} of the
#'    GeoNetwork and credentials to authenticate (\code{user}/\code{pwd}). By default,
#'    the \code{logger} argument will be set to \code{NULL} (no logger). This argument
#'    accepts two possible values: \code{INFO}: to print only geonapi logs,
#'    \code{DEBUG}: to print geonapi and CURL logs
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
#'}
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(url, user, pwd, version, logger)}}{
#'    This method is used to instantiate a \code{GNOpenAPIManager} with the \code{url} of the
#'    GeoNetwork and credentials to authenticate (\code{user}/\code{pwd}). By default,
#'    the \code{logger} argument will be set to \code{NULL} (no logger). This argument
#'    accepts two possible values: \code{INFO}: to print only geonapi logs,
#'    \code{DEBUG}: to print geonapi and CURL logs
#'  }
#'  \item{\code{login(user, pwd)}}{
#'    This methods attempts a connection to GeoNetwork REST API. User internally
#'    during initialization of \code{GNLegacyAPIManager}.
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
GNOpenAPIManager <- R6Class("GNOpenAPIManager",
  inherit = GNAbstractManager,
  private = list(),
  
  public = list(
    #manager
    initialize = function(url, user = NULL, pwd = NULL, version, logger = NULL){
      super$initialize(url, user = user, pwd = pwd, version = version, logger = logger)
      self$basicAuth <- TRUE
      
      #try to login
      if(!is.null(user) && !is.null(pwd)){        
        self$INFO(sprintf("Connecting to GeoNetwork services as authenticated user '%s'", user))
        self$login(user, pwd)
      }else{
        self$INFO("Connected to GeoNetwork services as anonymous user")
      }
    },
    
    #login
    #---------------------------------------------------------------------------
    login = function(user, pwd){
      
      req <- GNUtils$POST(
        url = self$getUrl(), path = "/info?type=me",
        user = user, pwd = pwd, content = NULL, contentType = NULL,
        verbose = TRUE 
      )
      
      private$user <- user
      private$keyring_backend$set_with_value(private$keyring_service, username = paste0(user,"_pwd"), password = pwd)
      
      req_cookies <- cookies(req)
      cookies <- as.list(req_cookies$value)
      names(cookies) <- req_cookies$name
      if(length(cookies[names(cookies)=="XSRF-TOKEN"])>0){
        token <- cookies[names(cookies)=="XSRF-TOKEN"][[1]]
        private$keyring_backend$set_with_value(private$keyring_service, username = paste0(user,"_token"), password = token)
      }
      cookies <- unlist(cookies[names(cookies)!="XSRF-TOKEN"])
      private$cookies <- paste0(sapply(names(cookies), function(cookiename){paste0(cookiename,"=",cookies[[cookiename]])}),collapse=";")
      
      keyring_token <- private$getToken()
      if(!is.null(keyring_token)){
        req <- GNUtils$POST(
          url = self$getUrl(), path = "/info?type=me",
          user = user, pwd = private$getPwd(), token = keyring_token, cookies = private$cookies, content = NULL, contentType = NULL,
          verbose = TRUE 
        )
      }
      
      if(status_code(req) == 401){
        err <- "Impossible to login to GeoNetwork: Wrong credentials"
        self$ERROR(err)
        stop(err)
      }
      if(status_code(req) == 404){
        err <- "Impossible to login to GeoNetwork: Incorrect URL or GeoNetwork temporarily unavailable"
        self$ERROR(err)
        stop(err)
      }
      if(status_code(req) != 200){
        err <- "Impossible to login to GeoNetwork: Unexpected error"
        self$ERROR(err)
        stop(err)
      }
      
      if(status_code(req) == 200){
        self$INFO("Successfully authenticated to GeoNetwork!\n")
      }
      return(TRUE)
    }

  )
                              
)
