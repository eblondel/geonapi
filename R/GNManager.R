#' GeoNetwork REST API Manager
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom openssl base64_encode
#' @import httr
#' @import XML
#' @importFrom openssl base64_encode
#' @export
#' @keywords geonetwork rest api
#' @return Object of \code{\link{R6Class}} with methods for communication with
#' the REST API of a GeoNetwork instance.
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'    GMManager$new("http://localhost:8080/geonetwork", "admin", "geonetwork")
#' }
#'
#' @field loggerType the type of logger
#' @field verbose.info if geosapi logs have to be printed
#' @field verbose.debug if curl logs have to be printed
#' @field url the Base url of GeoNetwork
#' @field version the version of GeoNetwork. Handled as \code{GNVersion} object
#' @field user the user name
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(url, user, pwd, version, logger)}}{
#'    This method is used to instantiate a GNManager with the \code{url} of the
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
#'  \item{\code{login(user, pwd)}}{
#'    This methods attempts a connection to GeoNetwork REST API. User internally
#'    during initialization of \code{GNManager}.
#'  }
#'  \item{\code{getClassName()}}{
#'    Retrieves the name of the class instance
#'  }
#'  \item{\code{insertMetadata(xml, file, group, category, stylesheet, validate)}}{
#'    Inserts a metadata by file or XML object. If successful, returns the Geonetwork
#'    metadata internal identifier (integer).
#'  }
#'  \item{\code{setPrivConfiguration(id, config)}}{
#'    Set the privilege configuration for a metadata. 'id' is the metadata integer id.
#'    'config' is an object of class "GNPrivConfiguration".
#'  }
#'  \item{\code{get(id, by, output)}}{
#'    Generic getter for metadata. Possible values for by are 'id', 'uuid'. Used
#'    internally only. The 'output' argument gives the type of output to return,
#'    with possible values "id", "metadata", "info".
#'  }
#'  \item{\code{getMetadataByID(id)}}{
#'    Get a metadata by Id. Returns an \code{ISOMetadata} object (from \pkg{geometa} package)
#'  }
#'  \item{\code{getMetadataByUUID(uuid)}}{
#'    Get a metadata by UUID. Returns an \code{ISOMetadata} object (from \pkg{geometa} package)
#'  } 
#'  \item{\code{getInfoByID(id)}}{
#'    Get a metadata Info by Id. Returns an XML document object
#'  }
#'  \item{\code{getInfoByUUID(uuid)}}{
#'    Get a metadata Info by UUID. Returns an XML document object
#'  }
#'  \item{\code{updateMetadata(id, xml, file)}}{
#'    Updates a metadata
#'  }
#'  \item{\code{deleteMetadata(id)}}{
#'    Deletes a metadata
#'  }
#'  \item{\code{deleteMetadataAll()}}{
#'    Deletes all metadata for which the authenticated user is owner
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
GNManager <- R6Class("GNManager",
  lock_objects = FALSE,
  
  #TODO provider specific formatter to prevent these fields to be printable
  private = list(
    user = NULL,
    pwd = NULL,
    token = NULL,
    
    #getEditingMetadataVersion
    #---------------------------------------------------------------------------
    getEditingMetadataVersion = function(id){
      self$INFO(sprintf("Fetching metadata version for id = %s", id))
      version <- NULL
      req <- GNUtils$GET(
        url = self$getUrl(),
        path = "/metadata.edit!",
        token = private$token,
        query = list(id = id),
        verbose = self$verbose.debug
      )
      xml <- NULL
      if(status_code(req) == 200){
        self$INFO("Successfully fetched editing metadata!")
        xml <- GNUtils$parseResponseXML(req)
        versionXML <- getNodeSet(xml, "//geonet:info/version",
                                 c(geonet = "http://www.fao.org/geonetwork"))
        if(length(versionXML)>0){
          version <- as.integer(xmlValue(versionXML[[1]]))
        }else{
          stop("No geonet:info XML element found")
        }
      }else{
        self$ERROR("Error while fetching metadata version")
      }
      
      return(version)
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
    basicAuth = FALSE,
    initialize = function(url, user = NULL, pwd = NULL, version, logger = NULL){
      
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
      
      #service lang
      isGN26 <- self$version$value$major == 2 && self$version$value$minor == 6
      self$lang <- ifelse(isGN26,"en","eng")
      
      #basic auth
      self$basicAuth <- self$version$value$major == 3
      
      #baseUrl
      self$url = sprintf("%s/srv/%s", url, self$lang)
      
      
      #try to login
      if(!is.null(user) && !is.null(pwd)){        
        self$INFO(sprintf("Connecting to GeoNetwork services as authenticated user '%s'", user))
        self$login(user, pwd)
      }else{
        self$INFO("Connected to GeoNetwork services as anonymous user")
      }
      
      
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
      
      if(self$basicAuth){
        private$user = user
        private$pwd = pwd
      }
      
      gnRequest <- GNRESTRequest$new(username = user, password = pwd)
      req <- GNUtils$POST(
        url = self$getUrl(),
        path = "/xml.user.login",
        content = GNUtils$getPayloadXML(gnRequest),
        contentType = "text/xml",
        verbose = self$verbose.debug
      )
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
      }else{
        if(!self$basicAuth) private$token <- cookies(req)$value
        self$INFO("Successfully authenticated to GeoNetwork!\n")
      }
      return(TRUE)
    },

    #getClassName
    #---------------------------------------------------------------------------
    getClassName = function(){
      return(class(self)[1])
    },
    
    #insertMetadata
    #---------------------------------------------------------------------------
    insertMetadata = function(xml = NULL, file = NULL, group,
                              category = NULL, stylesheet = NULL,
                              validate = FALSE){
      self$INFO("Inserting metadata ...")
      out <- NULL
      data <- NULL
      isTempFile <- FALSE
      if(!is.null(xml)){
        tempf = tempfile(tmpdir = tempdir())
        file <- paste(tempf,".xml",sep='')
        isTempFile <- TRUE
        saveXML(xml, file, encoding = "UTF-8")
      }
      if(!is.null(file)){
        xml <- xmlParse(file)
        data <- as(xml, "character")
        if(isTempFile) unlink(file)
      }else{
        stop("At least one of 'file' or 'xml' argument is required!")
      }
      cdata <- newXMLCDataNode(data)
      if(is.null(category)) category <- "_none_"
      if(is.null(stylesheet)) stylesheet <- "_none_"
      gnRequest <- GNRESTRequest$new(
        data = cdata,
        group = group,
        category = category,
        stylesheet = stylesheet,
        validate = ifelse(validate, "on", "off")
      )
      req <- GNUtils$POST(
        url = self$getUrl(),
        path = "/xml.metadata.insert",
        token = private$token, user = private$user, pwd = private$pwd,
        content = GNUtils$getPayloadXML(gnRequest),
        contentType = "text/xml",
        verbose = self$verbose.debug
      )
      if(status_code(req) == 200){
        self$INFO("Successfully inserted metadata!")
        response <- GNUtils$parseResponseXML(req)
        out <- as.integer(xpathApply(response, "//id", xmlValue)[[1]])
      }else{
        self$ERROR("Error while inserting metadata")
      }
      return(out)
    },
    
    #setPrivConfiguration
    #---------------------------------------------------------------------------
    setPrivConfiguration = function(id, config){
      self$INFO(sprintf("Setting privileges for metadata id = %s", id))
      out <- FALSE
      if(!is(config, "GNPrivConfiguration")){
        stop("The 'config' value should be an object of class 'GNPrivConfiguration")
      }
      queryParams = list(id = id)
      for(grant in config$privileges){
        for(priv in grant$privileges){
          el <- paste0("_", grant$group, "_", priv)
          queryParams[[el]] <- "on"
        }
      }
      req <- GNUtils$GET(
        url = self$getUrl(),
        path = "/metadata.admin",
        token = private$token, user = private$user, pwd = private$pwd,
        query = queryParams,
        verbose = self$verbose.debug
      )
      if(status_code(req) == 200){
        self$INFO(sprintf("Successfully set privileges for metadata id = %s!", id))
        out <- TRUE
      }else{
        self$ERROR("Error while setting privileges")
      }
      return(out)
    },
    
    
    #get
    #---------------------------------------------------------------------------
    get = function(id, by, output){
      allowedByValues <- c("id","uuid")
      if(!(by %in% allowedByValues)){
        stop(sprintf("Unsupported 'by' parameter value '%s'. Possible values are [%s]",
                     by, paste0(allowedByValues, collapse=",")))
      }
      allowedOutputTypes <- c("id", "metadata", "info")
      if(!(output %in% allowedOutputTypes)){
        stop(sprintf("Unsupported 'output' type '%s'. Possible values are [%s]",
                     output, paste0(allowedOutputTypes, collapse=",")))
      }
      self$INFO(sprintf("Fetching metadata for %s = %s", by, id))
      out <- NULL
      if(by=="id"){
        gnRequest <- GNRESTRequest$new(id = id)
      }else if(by=="uuid"){
        gnRequest <- GNRESTRequest$new(uuid = id)
      }
      req <- GNUtils$POST(
        url = self$getUrl(),
        path = "/xml.metadata.get",
        token = private$token, user = private$user, pwd = private$pwd,
        content = GNUtils$getPayloadXML(gnRequest),
        contentType = "text/xml",
        verbose = self$verbose.debug
      )
      if(status_code(req) == 200){
        self$INFO("Successfully fetched metadata!")
        xml <- GNUtils$parseResponseXML(req, "UTF-8")
        if(output == "id"){
          idXML <- getNodeSet(xml, "//geonet:info/id", c(geonet = "http://www.fao.org/geonetwork"))
          if(length(idXML)>0){
            out <- as.integer(xmlValue(idXML[[1]]))
          }else{
            stop("No geonet:info XML element found")
          }
        }else if(output == "metadata"){
          #bridge to geometa package once geometa XML decoding supported
          isoClass <- xmlName(xmlRoot(xml))
          out <- NULL
          if(isoClass=="MD_Metadata"){
            out <- geometa::ISOMetadata$new(xml = xml)
          }else if(isoClass=="FC_FeatureCatalogue"){
            out <- geometa::ISOFeatureCatalogue$new(xml = xml)
          }
        }else if(output == "info"){
          #TODO support for GNInfo object
          stop("GNInfo is not yet implemented in geonapi!")
        }
      }else{
        self$ERROR("Error while fetching metadata")
      }
      return(out)
    },
    
    #getMetadataByID
    #---------------------------------------------------------------------------
    getMetadataByID = function(id){
      return(self$get(id, by = "id", output = "metadata")) 
    },
    
    #getMetadataByUUID
    #---------------------------------------------------------------------------
    getMetadataByUUID = function(uuid){
      return(self$get(uuid, by = "uuid", output = "metadata")) 
    },
    
    #getInfoByID
    #---------------------------------------------------------------------------
    getInfoByID = function(id){
      return(self$get(id, by = "id", output = "info")) 
    },
    
    #getInfoByUUID
    #---------------------------------------------------------------------------
    getInfoByUUID = function(uuid){
      return(self$get(uuid, by = "uuid", output = "info")) 
    },
    
    #updateMetadata
    #---------------------------------------------------------------------------
    updateMetadata = function(id, xml = NULL, file = NULL){
      
      self$INFO(sprintf("Updating metadata id = %s ...", id))
      out <- NULL
      data <- NULL
      isTempFile <- FALSE
      if(!is.null(xml)){
        tempf = tempfile(tmpdir = tempdir())
        file <- paste(tempf,".xml",sep='')
        isTempFile <- TRUE
        saveXML(xml, file, encoding = "UTF-8")
      }
      if(!is.null(file)){
        xml <- xmlParse(file)
        data <- as(xml, "character")
        if(isTempFile) unlink(file)
      }else{
        stop("At least one of 'file' or 'xml' argument is required!")
      }
      cdata <- newXMLCDataNode(data)
      
      gnRequest <- GNRESTRequest$new(
        id = id,
        version = private$getEditingMetadataVersion(id),
        data = cdata
      )
      
      req <- GNUtils$POST(
        url = self$getUrl(),
        path = "/metadata.update.finish",
        token = private$token, user = private$user, pwd = private$pwd,
        content = GNUtils$getPayloadXML(gnRequest),
        contentType = "text/xml",
        verbose = self$verbose.debug
      )
      if(status_code(req) == 200){
        self$INFO("Successfully updated metadata!")
        response <- GNUtils$parseResponseXML(req)
        out <- as.integer(xpathApply(response, "//id", xmlValue)[[1]])
      }else{
        self$ERROR("Error while updating metadata")
      }
      return(out)
      
    },
    
    #deleteMetadata
    #---------------------------------------------------------------------------
    deleteMetadata = function(id){
      self$INFO(sprintf("Deleting metadata id = %s ...", id))
      out <- NULL
      gnRequest <- GNRESTRequest$new(id = id)
      req <- GNUtils$POST(
        url = self$getUrl(),
        path = "/xml.metadata.delete",
        token = private$token, user = private$user, pwd = private$pwd,
        content = GNUtils$getPayloadXML(gnRequest),
        contentType = "text/xml",
        verbose = self$verbose.debug
      )
      if(status_code(req) == 200){
        self$INFO("Successfully deleted metadata!")
        response <- GNUtils$parseResponseXML(req)
        out <- as.integer(xpathApply(response, "//id", xmlValue)[[1]])
      }else{
        self$ERROR("Error while deleting metadata")
      }
      return(out)
    },
    
    #deleteMetadataAll
    #---------------------------------------------------------------------------
    deleteMetadataAll = function(){
      self$INFO("Deleting all owned metadata...")
      
      if(self$version$lowerThan("2.10.x")){
        stop("Method unsupported for GN < 2.10.X")
      }
      
      #select all
      gnSelectRequest <- GNRESTRequest$new()
      gnSelectRequest$setChild("selected", "add-all")
      selectReq <- GNUtils$POST(
        url = self$getUrl(),
        path = "/xml.metadata.select",
        token = private$token, user = private$user, pwd = private$pwd,
        content = GNUtils$getPayloadXML(gnSelectRequest),
        contentType = "text/xml",
        verbose = self$verbose.debug
      )
      #delete all
      gnDeleteRequest <- GNRESTRequest$new()
      deleteReq <- GNUtils$POST(
        url = self$getUrl(),
        path = "/xml.metadata.batch.delete",
        token = private$token, user = private$user, pwd = private$pwd,
        content = GNUtils$getPayloadXML(gnDeleteRequest),
        contentType = "text/xml",
        verbose = self$verbose.debug
      )
      response <- GNUtils$parseResponseXML(deleteReq)
      return(response)
    }
  )
                     
)
