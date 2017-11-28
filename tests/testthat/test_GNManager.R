# test_GNManager.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GNManager.R
#=======================
require(geonapi, quietly = TRUE)
require(testthat)

context("GNManager")

test_that("GNManager connects",{
  expect_is(GN, "GNManager")
})