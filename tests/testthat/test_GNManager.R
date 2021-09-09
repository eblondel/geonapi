# test_GNManager.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GNManager.R
#=======================
require(geonapi, quietly = TRUE)
require(testthat)

context("GNManager")

test_that("Connect",{
  expect_is(GN, "GNLegacyAPIManager")
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