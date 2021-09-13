#' GeoNetwork REST API Manager Utils
#'
#' @docType class
#' @export
#' @keywords geonetwork rest api
#' @return Object of \code{\link{R6Class}} with static util methods for communication
#' with the REST API of a GeoNetwork instance.
#' @format \code{\link{R6Class}} object.
#'
#' @section Static methods:
#' \describe{
#'  \item{\code{getUserAgent()}}{
#'    This method is used to get the user agent for performing GeoNetwork API requests.
#'    Here the user agent will be compound by geonapi package name and version.
#'  }
#'  \item{\code{getUserToken(user, pwd)}}{
#'    This method is used to get the user authentication token for performing GeoNetwork
#'    API requests. Token is given a Base64 encoded string.
#'  }
#'  \item{\code{GET(url, path, token, verbose)}}{
#'    This method performs a GET request for a given \code{path} to GeoNetwork REST API
#'  }
#'  \item{\code{PUT(url, path, token, filename, contentType, verbose)}}{
#'    This method performs a PUT request for a given \code{path} to GeoNetwork REST API,
#'    to upload a file of name \code{filename} with given \code{contentType}
#'  }
#'  \item{\code{POST(url, path, token, content, contentType, encode, verbose)}}{
#'    This method performs a POST request for a given \code{path} to GeoNetwork REST API,
#'    to post content of given \code{contentType}
#'  }
#'  \item{\code{DELETE(url, path, token, verbose)}}{
#'    This method performs a DELETE request for a given GeoNetwork resource identified
#'    by a \code{path} in GeoNetwork REST API
#'  }
#'  \item{\code{parseResponseXML(req)}}{
#'    Convenience method to parse XML response from GeoNetwork REST API. Although package \pkg{httr}
#'    suggests the use of \pkg{xml2} package for handling XML, \pkg{geonapi} still relies
#'    on the package \pkg{XML}. Response from \pkg{httr} is retrieved as text, and then parsed as
#'    XML 'xmlParse' function.
#'  }
#'  \item{\code{getPayloadXML(obj)}}{
#'    Convenience method to create payload XML to send to GeoNetwork.
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GNUtils <- R6Class("GNUtils")

GNUtils$getUserAgent <- function(){
  return(paste("geonapi", packageVersion("geonapi"), sep="-"))
}

GNUtils$GET <- function(url, path = NULL, token = NULL, cookies = NULL,
                        user = NULL, pwd = NULL,
                        query = NULL, 
                        accept = "application/json", contentType = "application/json", 
                        verbose = FALSE){
  if(verbose){
    req <- with_verbose(GNUtils$GET(url, path, token, cookies, user, pwd, query, accept, contentType))
  }else{
    if(!is.null(path)){
      if(!grepl("^/", path)) path = paste0("/", path)
      url <- paste0(url, path)
    }
    if(!is.null(user) && !is.null(pwd)){
      req <- httr::GET(
        url = url,
        query = query,
        add_headers(
          "Accept" = accept,
          "Content-Type" = contentType,
          "User-Agent" = GNUtils$getUserAgent(),
          "Authorization" = paste("Basic", GNUtils$getUserToken(user, pwd)),
          "X-XSRF-TOKEN" = token,
          "Set-Cookie" = cookies
        )
      )
    }else{
      req <- httr::GET(
        url = url,
        query = query,
        add_headers(
          "Accept" = accept,
          "Content-Type" = contentType,
          "User-Agent" = GNUtils$getUserAgent(),
          "X-XSRF-TOKEN" = token,
          "Set-Cookie" = cookies
        )
      )
    }
  }
  if(verbose) print(req)
  return(req)
}

GNUtils$PUT <- function(url, path = NULL, token = NULL, cookies = NULL,
                        user = NULL, pwd = NULL,
                        content = NULL, filename = NULL,
                        contentType, verbose = FALSE){
  if(verbose){
    req <- with_verbose(GNUtils$PUT(url, path, token, cookies, user, pwd, content, filename, contentType))
  }else{
    body <- NULL
    if(missing(content) | is.null(content)){
      if(missing(filename) | is.null(filename)){
        stop("The filename must be provided")
      }
      body <- httr::upload_file(filename)
    }else{
      body <- content
    }
    
    if(!is.null(path)){
      if(!grepl("^/", path)) path = paste0("/", path)
      url <- paste0(url, path)
    }
    if(!is.null(user) && !is.null(pwd)){
      req <- httr::PUT(
        url = url,
        add_headers(
          "User-Agent" = GNUtils$getUserAgent(),
          "Content-type" = contentType,
          "Authorization" = paste("Basic", GNUtils$getUserToken(user, pwd)),
          "X-XSRF-TOKEN" = token,
          "Set-Cookie" = cookies
        ),    
        body = body
      )
    }else{
      req <- httr::PUT(
        url = url,
        add_headers(
          "User-Agent" = GNUtils$getUserAgent(),
          "Content-type" = contentType,
          "X-XSRF-TOKEN" = token,
          "Set-Cookie" = cookies
        ),    
        body = body
      )
    }
  }
  return(req)
}

GNUtils$POST <- function(url, path = NULL, token = NULL, cookies = NULL,
                         user = NULL, pwd = NULL,
                         content, contentType, 
                         encode = "raw",
                         verbose = FALSE){
  if(verbose){
    req <- with_verbose(GNUtils$POST(url, path, token, cookies, user, pwd, content, contentType, encode))
  }else{
    if(!is.null(path)){
      if(!grepl("^/", path)) path = paste0("/", path)
      url <- paste0(url, path)
    }
    if(!is.null(user) && !is.null(pwd)){
      req <- httr::POST(
        url = url,
        add_headers(
          "User-Agent" = GNUtils$getUserAgent(),
          "Content-type" = contentType,
          "Authorization" = paste("Basic", GNUtils$getUserToken(user, pwd)),
          "X-XSRF-TOKEN" = token,
          "Set-Cookie" = cookies
        ),
        encode = encode,
        body = content
      )
    }else{
      req <- httr::POST(
        url = url,
        add_headers(
          "User-Agent" = GNUtils$getUserAgent(),
          "Content-type" = contentType,
          "X-XSRF-TOKEN" = token,
          "Set-Cookie" = cookies
        ),
        encode = encode,
        body = content
      )
    }
  }
  return(req)
}

GNUtils$DELETE <- function(url, path = NULL, token = NULL, cookies = NULL, user = NULL, pwd = NULL, verbose = FALSE){
  if(verbose){
    req <- with_verbose(GNUtils$DELETE(url, path, token, cookies, user, pwd))
  }else{
    if(!is.null(path)){
      if(!grepl("^/", path)) path = paste0("/", path)
      url <- paste0(url, path)
    }
    if(!is.null(user) && !is.null(pwd)){
      req <- httr::DELETE(
        url = url,
        add_headers(
          "User-Agent" = GNUtils$getUserAgent(),
          "Authorization" = paste("Basic", GNUtils$getUserToken(user, pwd)),
          "X-XSRF-TOKEN" = token,
          "Set-Cookie" = cookies
        )
      )
    }else{
      req <- httr::DELETE(
        url = url,
        add_headers(
          "User-Agent" = GNUtils$getUserAgent(),
          "X-XSRF-TOKEN" = token,
          "Set-Cookie" = cookies
        ),
        set_cookies(
          cookies  
        )
      )
    }
  }
  return(req)
}

GNUtils$parseResponseXML <- function(req, encoding = "UTF-8"){
  return(xmlParse(content(req, as = "text", encoding = encoding)))
}

GNUtils$getPayloadXML <- function(obj){
  if(!("encode" %in% names(obj))){
    stop("R6 class with no XML encoder method!")
  }
  xml <- obj$encode()
  xmltext <- as(xml, "character")
  payload <- gsub("[\r\n ] ", "", xmltext)
  return(payload)
}

GNUtils$getUserToken <- function(user, pwd){
  token <- openssl::base64_encode(charToRaw(paste(user, pwd, sep=":")))
  return(token)
}