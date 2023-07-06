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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
GNOpenAPIManager <- R6Class("GNOpenAPIManager",
  inherit = GNAbstractManager,
  private = list(),
  
  public = list(
    
    #'@description This method is used to instantiate a \code{GNOpenAPIManager} with the \code{url} of the
    #'    GeoNetwork and credentials to authenticate (\code{user}/\code{pwd}).
    #'    
    #'    The \code{keyring_backend} can be set to use a different backend for storing 
    #'    the Geonetwork password/token with \pkg{keyring} (Default value is 'env').
    #'    
    #'    The logger can be either NULL, "INFO" (with minimum logs), or "DEBUG" 
    #'    (for complete curl http calls logs)
    #'@param url url
    #'@param user user
    #'@param pwd pwd
    #'@param version version
    #'@param logger logger
    #'@param keyring_backend keyring backend
    initialize = function(url, user = NULL, pwd = NULL, version, logger = NULL,
                          keyring_backend = 'env'){
      super$initialize(url, user = user, pwd = pwd, version = version, logger = logger,
                       keyring_backend = keyring_backend)
      self$basicAuth <- TRUE
      
      #baseUrl
      self$url = sprintf("%s/srv", url)
      private$keyring_service <- paste0("geonapi@", url)
      
      #try to login
      if(!is.null(user) && !is.null(pwd)){        
        self$INFO(sprintf("Connecting to GeoNetwork services as authenticated user '%s'", user))
        self$login(user, pwd)
      }else{
        self$INFO("Connected to GeoNetwork services as anonymous user")
      }
    },
    
    #'@description This methods attempts a connection to GeoNetwork REST API. User internally
    #'    during initialization of \code{GNLegacyAPIManager}.
    #'@param user user
    #'@param pwd pwd   
    login = function(user, pwd){
      
      req <- GNUtils$POST(
        url = paste0(self$getUrl(), "/", self$lang), path = "/info?type=me",
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
          url = paste0(self$getUrl(), "/", self$lang), path = "/info?type=me",
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
    },
    
    
    #'@description Retrieves the list of user groups available in Geonetwork
    #'@return an object of class \code{data.frame}
    getGroups = function(){
      out <- NULL
      self$INFO("Getting user groups...")
      req <- GNUtils$GET(
        url = self$getUrl(),
        path = "/api/groups",
        token = private$getToken(), cookies = private$cookies,
        user = private$user, 
        pwd = private$getPwd(),
        verbose = self$verbose.debug
      )
      if(status_code(req) == 200){
        self$INFO("Successfully fetched user groups!")
        json <- content(req)
        out <- do.call("rbind", lapply(json, function(group){
          out.group <- data.frame(
            id = group$id,
            name = group$name,
            stringsAsFactors = FALSE
          )
          return(out.group)
        }))
      }else{
        self$ERROR("Error while fetching user groups")
      }
      return(out)
    },
    
    #'@description Retrieves the list of tags (categories) available in Geonetwork
    #'@return an object of class \code{data.frame}
    getTags = function(){
      out <- NULL
      self$INFO("Getting tags (categories)...")
      req <- GNUtils$GET(
        url = self$getUrl(),
        path = "/api/tags",
        token = private$getToken(), cookies = private$cookies,
        user = private$user, 
        pwd = private$getPwd(),
        verbose = self$verbose.debug
      )
      if(status_code(req) == 200){
        self$INFO("Successfully fetched tags (categories)!")
        json <- content(req, encoding = "UTF-8")
        out <- do.call("rbind", lapply(json, function(json.tag){
          out.tag <- data.frame(
            id = json.tag$id,
            name = json.tag$name,
            stringsAsFactors = FALSE
          )
          labels <- data.frame(json.tag$label)
          colnames(labels) <- paste0("label_", colnames(labels))
          out.tag <- cbind(out.tag, labels)
          return(out.tag)
        }))
      }else{
        self$ERROR("Error while fetching tags (categories)")
      }
      return(out)
    },
    
    #'@description Retrieves the list of categories (same as tags) available in Geonetwork
    #'@return an object of class \code{data.frame}
    getCategories = function(){
      return(self$getTags())
    },
    
    #'@description Get a metadata by UUID.
    #'@param uuid uuid
    #'@param addSchemaLocation add schema location. Default is \code{TRUE}
    #'@param increasePopularity increase popularity. Default is \code{TRUE}
    #'@param approved approved
    #'@return Returns an object of class \code{ISOMetadata} (ISO 19115)
    #' or \code{ISOFeatureCatalogue} (ISO 19110) (from \pkg{geometa} package)
    getMetadataByUUID = function(uuid, 
                                 addSchemaLocation = TRUE, increasePopularity = TRUE, approved = TRUE){
      addSchemaLocation <- tolower(as.character(addSchemaLocation))
      increasePopularity <- tolower(as.character(increasePopularity))
      approved <- tolower(as.character(approved))
    
      self$INFO(sprintf("Fetching metadata for uuid = '%s'", uuid))
      out <- NULL
      req <- GNUtils$GET(
        url = self$getUrl(),
        path = sprintf("/api/records/%s/formatters/xml?addSchemaLocation=%s&increasePopularity=%s&approved=%s", 
                       uuid, addSchemaLocation, increasePopularity, approved),
        token = private$getToken(), cookies = private$cookies,
        user = private$user,
        pwd = private$getPwd(),
        accept = "application/xml", contentType = "application/xml",
        verbose = self$verbose.debug
      )
      if(status_code(req) == 200){
        self$INFO("Successfully fetched metadata!")
        xml <- GNUtils$parseResponseXML(req, "UTF-8")
        
        #bridge to geometa package once geometa XML decoding supported
        isoClass <- xmlName(xmlRoot(xml))
        out <- NULL
        if(isoClass=="MD_Metadata"){
          out <- geometa::ISOMetadata$new(xml = xml)
        }else if(isoClass=="FC_FeatureCatalogue"){
          out <- geometa::ISOFeatureCatalogue$new(xml = xml)
        }
      }else{
        self$ERROR(sprintf("Error while fetching metadata - %s", message_for_status(status_code(req))))
        self$ERROR(content(req))
      }
      return(out)
    },
    
    
    #'@description Inserts a record by file, XML object or \pkg{geometa} object of class \code{ISOMetadata} or \code{ISOFeatureCatalogue}. 
    #'    Extra parameters related to \pkg{geometa} objects: \code{geometa_validate} (TRUE by default) and \code{geometa_inspire} 
    #'    (FALSE by default) can be used to perform ISO and INSPIRE validation respectively. In that case on object of class 
    #'    \code{geometa::INSPIREMetadataValidator}, with a proper user API key, should be specified as \code{geometa_inspireValidator} 
    #'    argument.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param file file
    #'@param geometa geometa object of class \code{ISOMetadata} or \code{ISOFeatureCatalogue}
    #'@param metadataType metadata type. By default \code{METADATA}
    #'@param uuidProcessing UUID processing. By default \code{NOTHING}. Other possible value: \code{OVERWRITE}
    #'@param group group
    #'@param category category
    #'@param rejectIfInvalid reject if invalid. Default \code{FALSE}
    #'@param publishToAll publish to all. Default \code{TRUE}
    #'@param transformWith transform with. Default is \code{_none_}
    #'@param schema schema
    #'@param extra extra
    #'@param geometa_validate validate geometa object
    #'@param geometa_inspire validate geometa object vs. INSPIRE
    #'@param geometa_inspireValidator geometa INSPIRE validator to use 
    insertRecord = function(xml = NULL, file = NULL, geometa = NULL,
                            metadataType = "METADATA", uuidProcessing = "NOTHING", 
                            group, category = NULL, rejectIfInvalid = FALSE, publishToAll = TRUE,
                            transformWith = "_none_", schema = NULL, extra = NULL,
                            geometa_validate = TRUE, geometa_inspire = FALSE, geometa_inspireValidator = NULL){
      
      allowedMetadataTypes <- c("METADATA", "TEMPLATE", "SUB_TEMPLATE", "TEMPLATE_OF_SUB_TEMPLATE")
      if(!metadataType %in% allowedMetadataTypes){
        errMsg <- sprintf("Invalid metadataType value '%s'. Value should be among values [%s]", metadataType,
                          paste0(allowedMetadataTypes, collapse=","))
        self$ERROR(errMsg)
        stop(errMsg)
      }
      
      allowedUuidProcessing <- c("GENERATEUUID", "NOTHING", "OVERWRITE")
      if(!uuidProcessing %in% allowedUuidProcessing){
        errMsg <- sprintf("Invalid uuidProcessing value '%S'. Value should be among values [%s]", uuidProcessing,
                          paste0(allowedUuidProcessing, collapse=","))
      }
      
      if(is.null(category)) category <- "_none_"
      
      self$INFO("Uploading metadata ...")
      out <- NULL
      data <- NULL
      isTempFile <- FALSE
      if(!is.null(xml)){
        tempf = tempfile(tmpdir = tempdir())
        file <- paste(tempf,".xml",sep='')
        isTempFile <- TRUE
        saveXML(xml, file, encoding = "UTF-8")
      }
      if(!is.null(geometa)){
        if(!is(geometa, "ISOMetadata") & !is(geometa, "ISOFeatureCatalogue")){
          stop("Object 'geometa' should be of class 'ISOMetadata' or 'ISOFeatureCatalogue")
        }
        tempf = tempfile(tmpdir = tempdir())
        file <- paste(tempf,".xml",sep='')
        isTempFile <- TRUE
        geometa$save(file = file, validate = geometa_validate, 
                     inspire = geometa_inspire, inspireValidator = geometa_inspireValidator)
      }
      
      if(is.null(file)){
        stop("At least one of 'file', 'xml', or 'geometa' argument is required!")
      }
      
      #request payload
      reqParams <- list(
        metadataType = metadataType,
        uuidProcessing = uuidProcessing,
        group = group,
        category = category,
        rejectIfInvalid = tolower(as.character(rejectIfInvalid)),
        publishToAll = tolower(as.character(publishToAll)),
        transformWith = transformWith,
        schema = schema,
        extra = extra
      )
      reqParams <- reqParams[!sapply(reqParams, is.null)]
      path = sprintf("/api/records?%s", paste0(sapply(names(reqParams), function(x){paste0(x,"=",reqParams[[x]])}), collapse="&")) 
      
      req <- GNUtils$POST(
        url = self$getUrl(),
        path = path,
        token = private$getToken(), cookies = private$cookies,
        user = private$user, 
        pwd = private$getPwd(),
        content = list(
          file = httr::upload_file(file)
        ),
        contentType = "multipart/form-data",
        encode = "multipart",
        verbose = self$verbose.debug
      )
      if(status_code(req) == 201){
        self$INFO("Successfully uploaded metadata!")
        response <- content(req)
        out <- response
      }else{
        self$ERROR(sprintf("Error while uploading metadata - %s", message_for_status(status_code(req))))
      }
      if(isTempFile) unlink(file)
      return(out)
    },
    
    #'@description Inserts a metadata by file, XML object or \pkg{geometa} object of class \code{ISOMetadata} or \code{ISOFeatureCatalogue}. 
    #'    Extra parameters related to \pkg{geometa} objects: \code{geometa_validate} (TRUE by default) and \code{geometa_inspire} 
    #'    (FALSE by default) can be used to perform ISO and INSPIRE validation respectively. In that case on object of class 
    #'    \code{geometa::INSPIREMetadataValidator}, with a proper user API key, should be specified as
    #'    \code{geometa_inspireValidator} argument.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param file file
    #'@param geometa geometa object of class \code{ISOMetadata} or \code{ISOFeatureCatalogue}
    #'@param metadataType metadata type. By default \code{METADATA}
    #'@param uuidProcessing UUID processing. By default \code{NOTHING}. Other possible value: \code{OVERWRITE}
    #'@param group group
    #'@param category category
    #'@param rejectIfInvalid reject if invalid. Default \code{FALSE}
    #'@param publishToAll publish to all. Default \code{TRUE}
    #'@param transformWith transform with. Default is \code{_none_}
    #'@param schema schema
    #'@param extra extra
    #'@param geometa_validate validate geometa object
    #'@param geometa_inspire validate geometa object vs. INSPIRE
    #'@param geometa_inspireValidator geometa INSPIRE validator to use 
    insertMetadata = function(xml = NULL, file = NULL, geometa = NULL,
                              metadataType = "METADATA", uuidProcessing = "NOTHING", 
                              group, category = NULL, rejectIfInvalid = FALSE, publishToAll = TRUE,
                              transformWith = "_none_", schema = NULL, extra = NULL,
                              geometa_validate = TRUE, geometa_inspire = FALSE, geometa_inspireValidator = NULL){
      self$INFO("Inserting metadata ...")
      inserted <- self$insertRecord(xml = xml, file = file, geometa = geometa,
                        metadataType = metadataType, uuidProcessing = uuidProcessing, 
                        group = group, category = category, rejectIfInvalid = rejectIfInvalid, publishToAll = publishToAll,
                        transformWith = transformWith, schema = schema, extra = extra,
                        geometa_validate = geometa_validate, 
                        geometa_inspire = geometa_inspire, geometa_inspireValidator = geometa_inspireValidator)
    },
    
    #'@description Inserts a metadata by file, XML object or \pkg{geometa} object of class \code{ISOMetadata} or \code{ISOFeatureCatalogue}. 
    #'    Extra parameters related to \pkg{geometa} objects: \code{geometa_validate} (TRUE by default) and \code{geometa_inspire} 
    #'    (FALSE by default) can be used to perform ISO and INSPIRE validation respectively. In that case on object of class 
    #'    \code{geometa::INSPIREMetadataValidator}, with a proper user API key, should be specified as
    #'    \code{geometa_inspireValidator} argument.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param file file
    #'@param geometa geometa object of class \code{ISOMetadata} or \code{ISOFeatureCatalogue}
    #'@param metadataType metadata type. By default \code{METADATA}
    #'@param group group
    #'@param category category
    #'@param rejectIfInvalid reject if invalid. Default \code{FALSE}
    #'@param publishToAll publish to all. Default \code{TRUE}
    #'@param transformWith transform with. Default is \code{_none_}
    #'@param schema schema
    #'@param extra extra
    #'@param geometa_validate validate geometa object
    #'@param geometa_inspire validate geometa object vs. INSPIRE
    #'@param geometa_inspireValidator geometa INSPIRE validator to use 
    updateMetadata = function(xml = NULL, file = NULL, geometa = NULL,
                              metadataType = "METADATA",
                              group, category = NULL, rejectIfInvalid = FALSE, publishToAll = TRUE,
                              transformWith = "_none_", schema = NULL, extra = NULL,
                              geometa_validate = TRUE, geometa_inspire = FALSE, geometa_inspireValidator = NULL){
      self$INFO("Updating metadata ...")
      self$insertRecord(xml = xml, file = file, geometa = geometa,
                          metadataType = metadataType, uuidProcessing = "OVERWRITE", 
                          group = group, category = category, rejectIfInvalid = rejectIfInvalid, publishToAll = publishToAll,
                          transformWith = transformWith, schema = schema, extra = extra,
                          geometa_validate = geometa_validate, 
                          geometa_inspire = geometa_inspire, geometa_inspireValidator = geometa_inspireValidator)
    },
    
    #'@description Deletes a metadata by ID
    #'@param id id
    #'@param withBackup proceed with backup. Default is \code{TRUE}
    deleteMetadata = function(id, withBackup = TRUE){
      self$INFO(sprintf("Deleting metadata id = %s ...", id))
      out <- NULL
      req <- GNUtils$DELETE(
        url = self$getUrl(),
        path = sprintf("/api/records?uuids=%s&withBackup=%s", id, tolower(as.character(withBackup))),
        token = private$getToken(), cookies = private$cookies,
        user = private$user,
        pwd = private$getPwd(),
        verbose = self$verbose.debug
      )
      if(status_code(req) == 200){
        self$INFO("Successfully deleted metadata!")
        response = content(req)
        out <- response
      }else{
        self$ERROR(sprintf("Error while deleting metadata - %s", message_for_status(status_code(req))))
        self$ERROR(content(req))
      }
      return(out)
    },
    
    #'@description Uploads attachment
    #'@param id metadata identifier
    #'@param file file to upload
    #'@param visibility public or private
    #'@param approved object of class \code{logical}
    #'@return a named list of the uploaded attachment, including the url, size, id and type, \code{NULL} otherwise
    uploadAttachment = function(id, file, visibility = "public", approved = TRUE){
      out <- NULL
      self$INFO(sprintf("Attach file '%s' to record '%s'...", file, id))
      path = sprintf("/api/records/%s/attachments?visibility=%s&approved=%s", id, visibility, approved) 
      req <- GNUtils$POST(
        url = self$getUrl(),
        path = path,
        token = private$getToken(), cookies = private$cookies,
        user = private$user, 
        pwd = private$getPwd(),
        content = list(
          file = httr::upload_file(file)
        ),
        contentType = "multipart/form-data",
        encode = "multipart",
        verbose = self$verbose.debug
      )
      if(status_code(req) == 201){
        self$INFO("Successfully uploaded attachment!")
        response <- content(req, "parsed")
        out <- response
        if(startsWith(out$url, "http://localhost:8080")){
          out$url <- gsub("http://localhost:8080", paste0(unlist(strsplit(GN$getUrl(), "/"))[1:3],collapse="/"), out$url)
        }
      }else{
        self$ERROR(sprintf("Error while uploading attachment - %s", message_for_status(status_code(req))))
        self$ERROR(content(req))
      }
      return(out)
    },
    
    #'@description Publishes thumbnail based on URL
    #'@param id metadata identifier
    #'@param url thumbnail URL
    #'@param desc thumbnail description
    #'@return \code{TRUE} if published, \code{FALSE} otherwise
    publishThumbnail = function(id, url, desc = ""){
      out <- FALSE
      self$INFO(sprintf("Publish thumbnail '%s' to record '%s'...", url, id))
      path = sprintf("/api/records/%s/processes/thumbnail-add?thumbnail_url=%s&thumbnail_desc=%s&process=thumbnail-add&id=%s", 
                     id, url, desc, id) 
      req <- GNUtils$POST(
        url = self$getUrl(),
        path = path,
        token = private$getToken(), cookies = private$cookies,
        user = private$user, 
        pwd = private$getPwd(),
        content = NULL, contentType = "application/json",
        verbose = self$verbose.debug
      )
      if(status_code(req) == 204){
        self$INFO("Successfully published thumbnail!")
        response <- content(req)
        out <- response
      }else{
        self$ERROR(sprintf("Error while publishing thumbnail - %s", message_for_status(status_code(req))))
        self$ERROR(content(req))
      }
      return(out)
    },
    
    #'@description Checks pre-conditions to publish DOI
    #'@param id metadata identifier
    #'@return \code{TRUE} if DOI pre-conditions are fulfiled, \code{FALSE} otherwise
    doiCheckPreConditions = function(id){
      out <- FALSE
      path = sprintf("/api/records/%s/doi/checkPreConditions", id)
      req <- GNUtils$GET(
        url = self$getUrl(),
        path = path,
        token = private$getToken(), cookies = private$cookies,
        user = private$user, 
        pwd = private$getPwd(),
        contentType = "application/json",
        verbose = self$verbose.debug
      )
      if(status_code(req)==200){
        self$INFO(sprintf("Metadata record '%s' fulfills DOI pre-conditions", id))
        out <- TRUE
      }
      if(status_code(req)==400){
        self$ERROR(sprintf("Metadata record '%s' does not fulfill DOI pre-conditions", id))
        self$ERROR(content(req)$description)
      }
      if(status_code(req)==403){
        self$ERROR(sprintf("You don't have rights to edit metadata record '%s'", id))
      }
      if(status_code(req)==404){
        self$ERROR(sprintf("Metadata record '%s' does not exist", id))
      }
      if(status_code(req)==500){
        self$ERROR("Service unavailable")
      }
      return(out)
    },
    
    #'@description Submit a record to the Datacite metadata store in order to create a DOI.
    #'@param id metadata identifier
    #'@return \code{TRUE} if metadata record has been submitted with DOI created, \code{FALSE} otherwise 
    createDOI = function(id){
      out <- FALSE
      path = sprintf("/api/records/%s/doi", id)
      req = GNUtils$PUT(
        url = self$getUrl(),
        path = path,
        token = private$getToken(), cookies = private$cookies,
        user = private$user, 
        pwd = private$getPwd(),
        contentType = NULL,
        verbose = self$verbose.debug
      )
      print(content(req))
      if(status_code(req)==201){
        self$INFO(sprintf("DOI successfuly registered on DataCite for metadata record '%s'", id))
        out <- TRUE
        attr(out, "report") <- content(req)$description
      }
      if(status_code(req)==403){
        self$ERROR(sprintf("You don't have rights to edit metadata record '%s'", id))
      }
      if(status_code(req)==404){
        self$ERROR(sprintf("Metadata record '%s' does not exist", id))
      }
      if(status_code(req)==500){
        self$ERROR("Service unavailable")
      }
      return(out)
    },
    
    #'@description Remove a DOI (this is not recommended, DOI are supposed to be 
    #'persistent once created. This is mainly here for testing).
    #'@param id 
    deleteDOI = function(id){
      out <- FALSE
      path = sprintf("/api/records/%s/doi", id)
      req = GNUtils$DELETE(
        url = self$getUrl(),
        path = path,
        token = private$getToken(), cookies = private$cookies,
        user = private$user, 
        pwd = private$getPwd(),
        verbose = self$verbose.debug
      )
      if(status_code(req)==201){
        self$INFO(sprintf("DOI successfuly unregistered from DataCite for metadata record '%s'", id))
        out <- TRUE
      }
      if(status_code(req)==403){
        self$ERROR(sprintf("You don't have rights to unregister DOI for metadata record '%s'", id))
      }
      if(status_code(req)==404){
        self$ERROR(sprintf("Metadata record '%s' or DOI does not exist", id))
      }
      if(status_code(req)==500){
        self$ERROR("Service unavailable")
      }
    }
    
  )
                              
)
