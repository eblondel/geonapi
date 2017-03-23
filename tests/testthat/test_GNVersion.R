# test_GNVersion.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GNVersion.R
#=======================
require(geonapi, quietly = TRUE)
require(testthat)

context("GNVersion")

test_that("GNVersion works as expected",{
  gnVersion <- GNVersion$new(version = "2.6.3")
  expect_is(gnVersion, "GNVersion")
  expect_equal(gnVersion$version, "2.6.3")
  expect_is(gnVersion$value, "list")
  expect_equal(length(gnVersion$value), 3L)
  expect_true(gnVersion$lowerThan("3.0.x"))
  expect_true(gnVersion$lowerThan("2.11.2"))
  expect_true(gnVersion$lowerThan("2.10.2"))
  expect_true(gnVersion$greaterThan("1.5.1"))
  expect_true(gnVersion$greaterThan("2.5.1"))
  expect_true(gnVersion$greaterThan("2.6.x"))
  expect_false(gnVersion$lowerThan("2.5.1"))
  expect_false(gnVersion$greaterThan("2.10.2"))
  expect_false(gnVersion$greaterThan("3.0.0"))
  expect_false(gnVersion$greaterThan("2.6.3"))
  expect_false(gnVersion$lowerThan("2.6.3"))
  expect_true(gnVersion$equalTo("2.6.3"))
})