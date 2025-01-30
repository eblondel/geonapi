#' @name GNLegacyAPIManager
#' @title GNLegacyAPIManager
#' @aliases GNLegacyAPIManager
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
#'    GNLegacyAPIManager$new("http://localhost:8080/geonetwork", "admin", "geonetwork", "3.0.0")
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
GNLegacyAPIManager <- R6Class("GNLegacyAPIManager",
   inherit = GNAbstractManager,
   private = list(
     
     #getEditingMetadataVersion
     #---------------------------------------------------------------------------
     getEditingMetadataVersion = function(id){
       self$INFO(sprintf("Fetching metadata version for id = %s", id))
       version <- NULL
       req <- GNUtils$GET(
         url = self$getUrl(),
         path = "/metadata.edit!",
         token = private$getToken(),
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
         self$ERROR(sprintf("Error while fetching metadata version - %s", message_for_status(status_code(req))))
         self$ERROR(content(req))
       }
       
       return(version)
     }
   ),
   
   public = list(
     #'@description This method is used to instantiate a GNLegacyAPIManager with the \code{url} of the
     #'    GeoNetwork and credentials to authenticate (\code{user}/\code{pwd}).
     #'    
     #'    The \code{keyring_backend} can be set to use a different backend for storing 
     #'    the Geonetwork password/token with \pkg{keyring} (Default value is 'env').
     #'    
     #'    The logger can be either NULL, "INFO" (with minimum logs), or "DEBUG" 
     #'    (for complete curl http calls logs)
     #' @param url url
     #' @param user user
     #' @param pwd pwd
     #' @param version version
     #' @param logger logger
     #' @param keyring_backend keyring backend. Default is 'env'
     initialize = function(url, user = NULL, pwd = NULL, version, logger = NULL,
                           keyring_backend = 'env'){
       super$initialize(url, user = user, pwd = pwd, version = version, logger = logger,
                        keyring_backend = keyring_backend)
       #service lang
       isGN26 <- self$version$value$major == 2 && self$version$value$minor == 6
       self$lang <- ifelse(isGN26,"en","eng")
       
       #basic auth
       self$basicAuth <- self$version$value$major == 3
       
       #baseUrl
       self$url = sprintf("%s/srv/%s", url, self$lang)
       private$keyring_service <- paste0("geonapi@", url)
       
       #try to login
       if(!is.null(user) && !is.null(pwd)){        
         self$INFO(sprintf("Connecting to GeoNetwork services as authenticated user '%s'", user))
         self$login(user, pwd)
       }else{
         self$INFO("Connected to GeoNetwork services as anonymous user")
       }
     },
     
     #'@description #'    This methods attempts a connection to GeoNetwork REST API. User internally
     #'    during initialization of \code{GNLegacyAPIManager}.
     #'@param user user
     #'@param pwd pwd
     login = function(user, pwd){
       
       req <- NULL
       if(self$basicAuth){
         #for newer versions >= 3
         req <- GNUtils$POST(
           url = self$getUrl(), path = "/info?type=me",
           user = user, pwd = pwd, content = NULL, contentType = NULL,
           verbose = TRUE 
         )
       }else{
         #for older versions < 3
         gnRequest <- GNRESTRequest$new(username = user, password = pwd)
         req <- GNUtils$POST(
           url = self$getUrl(),
           path = "/xml.user.login",
           content = gnRequest$encode(),
           contentType = "text/xml",
           verbose = self$verbose.debug
         )
       }
       
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
     },
     
     #'@description Retrieves the list of user groups available in Geonetwork
     #'@return an object of class \code{data.frame}
     getGroups = function(){
       out <- NULL
       self$INFO("Getting user groups...")
       req <- GNUtils$GET(
         url = self$getUrl(),
         path = "/xml.info",
         token = private$getToken(), cookies = private$cookies,
         user = private$user, 
         pwd = private$getPwd(),
         query = list(type = "groups"),
         verbose = self$verbose.debug
       )
       if(status_code(req) == 200){
         self$INFO("Successfully fetched user groups!")
         xml <- GNUtils$parseResponseXML(req, "UTF-8")
         xml.groups <- getNodeSet(xml, "//group/group")
         out <- do.call("rbind", lapply(xml.groups, function(xml.group){
           out.group <- data.frame(
             id = xmlGetAttr(xml.group, "id"),
             name = xmlValue(xmlChildren(xml.group)$name),
             stringsAsFactors = FALSE
           )
           return(out.group)
         }))
       }else{
         self$ERROR("Error while fetching user groups")
       }
       return(out)
     },
     
     #'@description Retrieves the list of categories available in Geonetwork
     #'@return an object of class \code{data.frame}
     getCategories = function(){
        out <- NULL
        self$INFO("Getting categories...")
        req <- GNUtils$GET(
           url = self$getUrl(),
           path = "/xml.info",
           token = private$getToken(), cookies = private$cookies,
           user = private$user, 
           pwd = private$getPwd(),
           query = list(type = "categories"),
           verbose = self$verbose.debug
        )
        if(status_code(req) == 200){
           self$INFO("Successfully fetched categories!")
           xml <- GNUtils$parseResponseXML(req, "UTF-8")
           xml.categories <- getNodeSet(xml, "//metadatacategory/category")
           out <- do.call("rbind", lapply(xml.categories, function(xml.category){
              out.group <- data.frame(
                 id = xmlGetAttr(xml.category, "id"),
                 name = xmlValue(xmlChildren(xml.category)$name),
                 stringsAsFactors = FALSE
              )
              xml.label <- xmlChildren(xml.category)$label
              xml.labels <- xmlChildren(xml.label)
              labels <- t(as.data.frame(sapply(xml.labels, xmlValue)))
              colnames(labels) <- paste0("label_", colnames(labels))
              row.names(labels) <- NULL
              out.group <- cbind(out.group, labels)
              return(out.group)
           }))
        }else{
           self$ERROR("Error while fetching categories")
        }
        return(out)
     },
     
     #'@description Inserts a metadata by file, XML object or \pkg{geometa} object of class
     #'    \code{ISOMetadata} or \code{ISOFeatureCatalogue}. If successful, returns the Geonetwork
     #'    metadata internal identifier (integer). Extra parameters \code{geometa_validate} (TRUE 
     #'    by default) and \code{geometa_inspire} (FALSE by default) can be used with geometa objects 
     #'    for perform ISO and INSPIRE validation respectively. In that case on object of class 
     #'    \code{geometa::INSPIREMetadataValidator}, with a proper user API key, should be specified as
     #'    \code{geometa_inspireValidator} argument. 
     #' @param xml xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #' @param file file
     #' @param geometa geometa, object of class \code{ISOMetadata} or \code{ISOFeatureCatalogue} from \pkg{geometa}
     #' @param group group
     #' @param category category
     #' @param stylesheet stylesheet
     #' @param validate validate
     #' @param geometa_validate validate geometa object
     #' @param geometa_inspire validate geometa object vs. INSPIRE
     #' @param geometa_inspireValidator geometa INSPIRE validator to use
     insertMetadata = function(xml = NULL, file = NULL, geometa = NULL, group,
                               category = NULL, stylesheet = NULL, validate = FALSE,
                               geometa_validate = TRUE, geometa_inspire = FALSE, geometa_inspireValidator = NULL){
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
         data <- geometa::readISO(file = file, raw = TRUE)
         if(isTempFile) unlink(file)
       }
       if(!is.null(geometa)){
         if(!is(geometa, "ISOMetadata") & !is(geometa, "ISOFeatureCatalogue")){
           stop("Object 'geometa' should be of class 'ISOMetadata' or 'ISOFeatureCatalogue")
         }
         data <- geometa$encode(validate = geometa_validate, 
                                inspire = geometa_inspire, inspireValidator = geometa_inspireValidator)
       }
       
       if(is.null(data)){
         stop("At least one of 'file', 'xml', or 'geometa' argument is required!")
       }
       
       if(is.null(category)) category <- "_none_"
       if(is.null(stylesheet)) stylesheet <- "_none_"
       gnRequest <- GNRESTRequest$new(
         data = data,
         group = group,
         category = category,
         stylesheet = stylesheet,
         validate = ifelse(validate, "on", "off")
       )
       
       req <- GNUtils$POST(
         url = self$getUrl(),
         path = "/xml.metadata.insert",
         token = private$getToken(), cookies = private$cookies,
         user = private$user, 
         pwd = private$getPwd(),
         content = gnRequest$encode(),
         contentType = "text/xml",
         verbose = self$verbose.debug
       )
       if(status_code(req) == 200){
         self$INFO("Successfully inserted metadata!")
         response <- GNUtils$parseResponseXML(req)
         out <- as.integer(xpathApply(response, "//id", xmlValue)[[1]])
       }else{
         self$ERROR(sprintf("Error while inserting metadata - %s", message_for_status(status_code(req))))
         self$ERROR(content(req))
       }
       return(out)
     },
     
     #'@description Set the privilege configuration for a metadata. 'id' is the metadata integer id.
     #'    'config' is an object of class "GNPrivConfiguration".
     #'@param id id
     #'@param config config
     setPrivConfiguration = function(id, config){
       self$INFO(sprintf("Setting privileges for metadata id = %s", id))
       out <- FALSE
       if(!is(config, "GNPrivConfiguration")){
         stop("The 'config' value should be an object of class 'GNPrivConfiguration")
       }
       queryPrivParams = list()
       for(grant in config$privileges){
         for(priv in grant$privileges){
           el <- paste0("_", grant$group, "_", priv)
           queryPrivParams[[el]] <- "on"
         }
       }
       queryParams <- list()
       if(self$version$value$major == 3){
         queryParams <- c(list("_content_type" = "xml"), queryPrivParams, list(id = id))
       }else{
         queryParams <- c(list(id = id), queryPrivParams)
       }
       
       req <- GNUtils$GET(
         url = self$getUrl(),
         path = ifelse(self$version$value$major < 3, "/metadata.admin", "md.privileges.update"),
         token = private$getToken(), cookies = private$cookies,
         user = private$user,
         pwd = private$getPwd(),
         query = queryParams,
         verbose = self$verbose.debug
       )
       if(status_code(req) == 200){
         self$INFO(sprintf("Successfully set privileges for metadata id = %s!", id))
         out <- TRUE
       }else{
         self$ERROR(sprintf("Error while setting privileges - %s", message_for_status(status_code(req))))
         self$ERROR(content(req))
       }
       return(out)
     },
     
     
     #'@description Generic getter for metadata. Possible values for by are 'id', 'uuid'. Used
     #'    internally only. The 'output' argument gives the type of output to return,
     #'    with possible values "id", "metadata", "info".
     #'@param id id
     #'@param by by
     #'@param output output
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
         token = private$getToken(), cookies = private$cookies,
         user = private$user,
         pwd = private$getPwd(),
         content = gnRequest$encode(),
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
           isoStandard <- switch(XML::xmlNamespace(xmlRoot(xml)),
             "http://www.isotc211.org/2005/gmd" = "19139",
             "http://standards.iso.org/iso/19115/-3/mdb/2.0" = "19115-3",
             "http://www.isotc211.org/2005/gfc" = "19139",
             "http://standards.iso.org/iso/19110/gfc/1.1" = "19115-3",
             "19139"
           )
           geometa::setMetadataStandard(isoStandard)
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
         self$ERROR(sprintf("Error while fetching metadata - %s", message_for_status(status_code(req))))
         self$ERROR(content(req))
       }
       return(out)
     },
     
     #'@description Get a metadata by Id
     #'@param id id
     #'@return an object of class \code{ISOMetadata} (ISO 19115) or \code{ISOFeatureCatalogue} 
     #' (ISO 19110) (from \pkg{geometa} package)
     getMetadataByID = function(id){
       return(self$get(id, by = "id", output = "metadata")) 
     },
     
     #'@description Get a metadata by UUID
     #'@param uuid uuid
     #'@return an object of class \code{ISOMetadata} (ISO 19115) or \code{ISOFeatureCatalogue} 
     #' (ISO 19110) (from \pkg{geometa} package)
     getMetadataByUUID = function(uuid){
       return(self$get(uuid, by = "uuid", output = "metadata")) 
     },
     
     #'@description Get a metadata Info by Id.
     #'@param id id
     #'@return an XML document object
     getInfoByID = function(id){
       return(self$get(id, by = "id", output = "info")) 
     },
     
     #'@description Get a metadata Info by UUID
     #'@param uuid uuid
     #'@return an XML document object
     getInfoByUUID = function(uuid){
       return(self$get(uuid, by = "uuid", output = "info")) 
     },
     
     #'@description Updates a metadata by file, XML object or \pkg{geometa} object of class
     #'    'ISOMetadata' or 'ISOFeatureCatalogue'. Extra parameters \code{geometa_validate} (TRUE 
     #'    by default) and \code{geometa_inspire} (FALSE by default) can be used with geometa objects 
     #'    for perform ISO and INSPIRE validation respectively. In that case on object of class 
     #'    \code{geometa::INSPIREMetadataValidator}, with a proper user API key, should be specified as
     #'    \code{geometa_inspireValidator} argument. 
     #' @param id metadata id
     #' @param xml xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #' @param file file
     #' @param geometa geometa, object of class \code{ISOMetadata} or \code{ISOFeatureCatalogue} from \pkg{geometa}
     #' @param geometa_validate validate geometa object
     #' @param geometa_inspire validate geometa object vs. INSPIRE
     #' @param geometa_inspireValidator geometa INSPIRE validator to use
     updateMetadata = function(id, xml = NULL, file = NULL, geometa = NULL,
                               geometa_validate = TRUE, geometa_inspire = FALSE, geometa_inspireValidator = NULL){
       
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
         data <- geometa::readISO(file = file, raw = TRUE)
         if(isTempFile) unlink(file)
       }
       if(!is.null(geometa)){
         if(!is(geometa, "ISOMetadata") & !is(geometa, "ISOFeatureCatalogue")){
           stop("Object 'geometa' should be of class 'ISOMetadata' or 'ISOFeatureCatalogue")
         }
         data <- geometa$encode(validate = geometa_validate, 
                                inspire = geometa_inspire, inspireValidator = geometa_inspireValidator)
       }
       
       if(is.null(data)){
         stop("At least one of 'file', 'xml', or 'geometa' argument is required!")
       }
       
       if(self$version$value$major >= 3){
         gnRequest <- GNRESTRequest$new(
           id = id,
           data = data
         )
       }else{
         gnRequest <- GNRESTRequest$new(
           id = id,
           version = private$getEditingMetadataVersion(id),
           data = data
         )
       }
       
       req <- GNUtils$POST(
         url = self$getUrl(),
         path = "/metadata.update.finish",
         token = private$getToken(), cookies = private$cookies,
         user = private$user,
         pwd = private$getPwd(),
         content = gnRequest$encode(),
         contentType = "text/xml",
         verbose = self$verbose.debug
       )
       if(status_code(req) == 200){
         self$INFO("Successfully updated metadata!")
         response <- GNUtils$parseResponseXML(req)
         out <- as.integer(xpathApply(response, "//id", xmlValue)[[1]])
       }else{
         self$ERROR(sprintf("Error while updating metadata - %s", message_for_status(status_code(req))))
         self$ERROR(content(req))
       }
       return(out)
       
     },
     
     #'@description Deletes metadata by Id.
     #'@param id id
     #'@return the id of the record deleted, \code{NULL} otherwise
     deleteMetadata = function(id){
       self$INFO(sprintf("Deleting metadata id = %s ...", id))
       out <- NULL
       gnRequest <- GNRESTRequest$new(id = id)
       req <- GNUtils$POST(
         url = self$getUrl(),
         path = "/xml.metadata.delete",
         token = private$getToken(), cookies = private$cookies,
         user = private$user,
         pwd = private$getPwd(),
         content = gnRequest$encode(),
         contentType = "text/xml",
         verbose = self$verbose.debug
       )
       if(status_code(req) == 200){
         self$INFO("Successfully deleted metadata!")
         response <- GNUtils$parseResponseXML(req)
         out <- as.integer(xpathApply(response, "//id", xmlValue)[[1]])
       }else{
         self$ERROR(sprintf("Error while deleting metadata - %s", message_for_status(status_code(req))))
         self$ERROR(content(req))
       }
       return(out)
     },
     
     #'@description Deletes all metadata
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
         token = private$getToken(), cookies = private$cookies,
         user = private$user,
         pwd = private$getPwd(),
         content = gnSelectRequest$encode(),
         contentType = "text/xml",
         verbose = self$verbose.debug
       )
       #delete all
       gnDeleteRequest <- GNRESTRequest$new()
       deleteReq <- GNUtils$POST(
         url = self$getUrl(),
         path = "/xml.metadata.batch.delete",
         token = private$getToken(), cookies = private$cookies,
         user = private$user,
         pwd = private$getPwd(),
         content = gnDeleteRequest$encode(),
         contentType = "text/xml",
         verbose = self$verbose.debug
       )
       response <- GNUtils$parseResponseXML(deleteReq)
       return(response)
     }
   )
                     
)
