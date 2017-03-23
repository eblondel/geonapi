# test_GNRESTRequest.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GNRESTRequest.R
#=======================
require(geonapi, quietly = TRUE)
require(testthat)

context("GNRESTRequest")

test_that("GNRESTRequest encoding",{
  req <- GNRESTRequest$new(id = 1, name = "test")
  expect_is(req, "GNRESTRequest")
  expect_equal(req$rootName, "request")
  expect_equal(length(req$children), 2L)
  xml <- req$encode()
  expect_true(all(sapply(xmlChildren(xml), xmlValue) == req$children))
})