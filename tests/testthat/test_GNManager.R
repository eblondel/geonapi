# test_GNManager.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GNManager.R
#=======================
require(geonapi, quietly = TRUE)
require(testthat)

context("GNVersion")

test_that("GNManager connects",{

  gnman <- GNManager$new(
    url = "http://localhost:8080/geonetwork",
    user = "admin",
    pwd = "geonetwork",
    version = "2.6.3",
    "DEBUG"
  )
  
  test = gnman$getMetadataByUUID("my-metadata-uuid")
  
})