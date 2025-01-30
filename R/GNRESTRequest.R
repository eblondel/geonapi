#' GeoNetwork REST API REST Request
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords geonetwork rest api
#' @return Object of \code{\link[R6]{R6Class}} for modelling a GeoNetwork REST request
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GNRESTRequest <- R6Class("GNRESTRequest",
  private = list(
    xmlNodeToCharacter = function (x, ..., indent = "", tagSeparator = "\n") 
    {
      out <- ""
      if (length(xmlAttrs(x))) {
        tmp <- paste(names(xmlAttrs(x)), paste("\"", XML:::insertEntities(xmlAttrs(x)), 
                                               "\"", sep = ""), sep = "=", collapse = " ")
      } else{
        tmp <- ""
      }
      if (length(x$namespaceDefinitions) > 0) {
        k = as(x$namespaceDefinitions, "character")
        ns = paste("xmlns", ifelse(nchar(names(k)), ":", ""), 
                   names(k), "=", ddQuote(k), sep = "", collapse = " ")
      } else{
        ns <- ""
      }
      subIndent <- paste(indent, " ", sep = "")
      if (is.logical(indent) && !indent) {
        indent <- ""
        subIndent <- FALSE
      }
      if (length(xmlChildren(x)) == 0) {
        out <- paste(out,indent, paste("<", xmlName(x, TRUE), ifelse(tmp != 
                                                                       "", " ", ""), tmp, ifelse(ns != "", " ", ""), ns, 
                                       "/>", tagSeparator, sep = ""), sep = "")
      } else if (length(xmlChildren(x)) == 1 && inherits(xmlChildren(x)[[1]], "XMLTextNode")) {
        out <- paste(out,indent, paste("<", xmlName(x, TRUE), ifelse(tmp != 
                                                                       "", " ", ""), tmp, ifelse(ns != "", " ", ""), ns, 
                                       ">", sep = ""), sep = "")
        kid = xmlChildren(x)[[1]]
        if (inherits(kid, "EntitiesEscaped")) 
          txt = xmlValue(kid)
        else txt = XML:::insertEntities(xmlValue(kid))
        out <- paste(out,txt, sep = "")
        out <- paste(out,paste("</", xmlName(x, TRUE), ">", tagSeparator, 
                               sep = ""), sep = "")
      } else {
        out <- paste(out,indent, paste("<", xmlName(x, TRUE), ifelse(tmp != 
                                                                       "", " ", ""), tmp, ifelse(ns != "", " ", ""), ns, 
                                       ">", tagSeparator, sep = ""), sep = "")
        for (i in xmlChildren(x)){
          out_child <- NULL
          if(is(i,"XMLNode")){
            if(is(i,"XMLCommentNode")){
              out_child <- paste0(capture.output(i),collapse="")
            }else if(is(i,"XMLCDataNode")){
              out_child <- paste0("<![CDATA[",as(i, "character"), "]]>")
            }else{
              out_child <- private$xmlNodeToCharacter(i)
            }
          }else{
            out_child <- paste(as(i,"character"),tagSeparator,sep="")
          }
          if(!is.null(out_child)) out <- paste(out, out_child, sep="") 
        }
        out<-paste(out,indent, paste("</", xmlName(x, TRUE), ">", tagSeparator, 
                                     sep = ""), sep = "")
      }
      return(out)
    }
  ),
  public = list(
    #'@field rootName root name
    rootName = NULL,
    #'@field children children
    children = list(),
    #'@description Initializes a \link{GNRESTRequest}
    #'@param ... any parameter to pass to the request
    initialize = function(...){
      self$rootName = "request"
      self$children = list(...)
    },
    
    #'@description Set child
    #'@param key key
    #'@param value value
    setChild = function(key, value){
      self$children[[key]] <- value
    },
    
    #'@description Encodes request as XML
    #'@return an object of class \code{character} representing the XML
    encode = function(){
      rootXML <- xmlOutputDOM("request")
      for(childName in names(self$children)){
        child <- self$children[[childName]]
        if(is(child,"XMLInternalNode") | is(child, "XMLInternalDocument")){
          childXML <- xmlOutputDOM(childName)
          childXML$addCData(as(child,"character"))
          rootXML$addNode(childXML$value())
        }else{
          rootXML$addTag(childName, child)
        }
        
      }
      out <- rootXML$value()
      out <- private$xmlNodeToCharacter(out)
      if(Encoding(out)!="UTF-8") out <- iconv(out, to = "UTF-8")
      out <- gsub("[\r\n ] ", "", out)
      out <- gsub(">\n", ">", out)
      return(out)
    }
  )                     
)