# test_GNManager.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GNManager.R
#=======================
require(geonapi, quietly = TRUE)
require(testthat)

context("GNManager")

test_that("Connect",{
  if(GN$version$value$major == 4){
    expect_is(GN, "GNOpenAPIManager")
  }else{
    expect_is(GN, "GNLegacyAPIManager")
  }
})

test_that("GET groups",{
  groups <- GN$getGroups()
  expect_is(groups, "data.frame")
  expect_equal(colnames(groups)[1], "id")
  expect_equal(colnames(groups)[2], "name")
})

test_that("GET tags/categories",{
  categories <- GN$getCategories()
  expect_is(categories, "data.frame")
  expect_equal(colnames(categories)[1], "id")
  expect_equal(colnames(categories)[2], "name")
  expect_true(all(sapply(colnames(categories)[3:ncol(categories)], function(x){startsWith(x, "label_")})))
})

test_that("CREATE metadata",{
  mdfile <- system.file("extdata/examples", "metadata.xml", package = "geonapi")
  md <- geometa::readISO19139(file = mdfile)
  if(GN$getClassName() == "GNLegacyAPIManager"){
    created = GN$insertMetadata(geometa = md, group = "1", category = ifelse(GN$getClassName() == "GNOpenAPIManager", "1", "datasets"))
    config <- GNPrivConfiguration$new()
    config$setPrivileges("all", c("view","dynamic","featured"))
    GN$setPrivConfiguration(id = created, config = config)
    expect_true(!is.null(created))
  }else if(GN$getClassName() == "GNOpenAPIManager"){
    created = GN$insertMetadata(geometa = md, group = "1", category = "1")
    expect_equal(created$numberOfRecordsProcessed, 1L)
  }
  
})

test_that("READ metadata",{
  id <- "my-metadata-identifier"
  md <- GN$getMetadataByUUID(id)
  expect_is(md, "ISOMetadata")
  expect_equal(md$fileIdentifier, id)
  expect_equal(md$dataSetURI, "my-dataset-identifier")
})

test_that("UPDATE metadata",{
  id <- "my-metadata-identifier"
  md <- GN$getMetadataByUUID(id)
  md$setDataSetURI("new-dataset-uri")
  md$identificationInfo[[1]]$setAbstract("new abstract with additional information")
  if(GN$getClassName() == "GNLegacyAPIManager"){
    metaId <- GN$get(md$fileIdentifier, by = "uuid", output = "id")
    updated <- GN$updateMetadata(id = metaId, geometa = md)
    expect_equal(updated, metaId)
  }else if(GN$getClassName() == "GNOpenAPIManager"){
    updated = GN$updateMetadata(geometa = md, group = "1", category = "1")
    expect_equal(updated$numberOfRecordsProcessed, 1L)
  }
})

test_that("DELETE metadata",{
  id <- "my-metadata-identifier"
  if(GN$getClassName() == "GNLegacyAPIManager"){
    metaId <- GN$get(id, by = "uuid", output = "id")
    deleted <- GN$deleteMetadata(id = metaId)
    expect_equal(deleted, metaId)
  }else if(GN$getClassName() == "GNOpenAPIManager"){
    deleted <- GN$deleteMetadata(id = id)
    expect_equal(deleted$numberOfRecordsProcessed, 1L)
  }
})