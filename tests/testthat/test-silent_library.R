library(HOQCutil)
library(testthat)


context('silent_library\n')

test_that("load is silent for quoted package name", {
  ind = match('package:dplyr',search())
  expect_true( is.na(ind) )
  expect_output(silent_library('dplyr'),NA)
  ind = match('package:dplyr',search())
  expect_false( is.na(ind))
  detach(pos=ind)
  ind = match('package:dplyr',search())
  expect_true( is.na(ind) )
})

test_that("load is silent for unquoted package name", {
  ind = match('package:dplyr',search())
  expect_true( is.na(ind) )
  expect_output(silent_library(dplyr),NA)
  ind = match('package:dplyr',search())
  expect_false( is.na(ind))
  detach(pos=ind)
  ind = match('package:dplyr',search())
  expect_true( is.na(ind) )
})

test_that("load is silent for package named in an expression", {
  ind = match('package:dplyr',search())
  expect_true( is.na(ind) )
  packages_to_load <- c('glue','FactoMineR','dplyr')
  expect_output(silent_library(packages_to_load[[3]]),NA)
  ind = match('package:dplyr',search())
  expect_false( is.na(ind))
  detach(pos=ind)
  ind = match('package:dplyr',search())
  expect_true( is.na(ind) )
})


