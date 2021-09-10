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
  created = GN$insertMetadata(geometa = md, group = "1", category = "datasets")
  config <- GNPrivConfiguration$new()
  config$setPrivileges("all", c("view","dynamic","featured"))
  GN$setPrivConfiguration(id = created, config = config)
  expect_true(!is.null(created))
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
  metaId <- GN$get(md$fileIdentifier, by = "uuid", output = "id")
  updated <- GN$updateMetadata(id = metaId, geometa = md)
  expect_equal(updated, metaId)
})

test_that("DELETE metadata",{
  id <- "my-metadata-identifier"
  metaId <- GN$get(id, by = "uuid", output = "id")
  deleted <- GN$deleteMetadata(id = metaId)
  expect_equal(deleted, metaId)
})