library(HOQCutil)
library(testthat)


context('gen_ran_words')

test_that("gen_ran_words is working", {
  c1  <- gen_ran_words(2,14,prefix='pic_',seed=13)
  expect_equal( length(c1),2)
  expect_true( all (nchar(c1)==14))
  e3  <-  c("pic_xjmfvd2qow","pic_cel55qkdwn")
  expect_equal( c1,e3)
})

