
test_that("cut3d", {
  d123tab <- tibble::tribble(
    ~d1L, ~d1U, ~d2L, ~d2U, ~d3L, ~d3U, ~r ,
    1,    1,    1,    1,    NaN,  NaN, 1 ,
    1,    1,    2,    3,    NaN,  NaN, 2 ,
    1,    1,    4,    4,    1,    1,   3 ,
    1,    1,    4,    5,    2,    3,   4 ,
    1,    1,    NaN,  NaN,  4,    10,  5
  )
  mymat <- tibble::tribble(
    ~d1, ~d2, ~d3,
    1, 1, 6,
    1, 4, 1,
    1, 4, 6 ,
    1, 4, 12
  )
  r1 <- HOQCutil::cut3d(mymat,d123tab)
  expect_identical(class(r1),"numeric")
  expect_equal(length(r1),nrow(mymat))
  expect_equal(r1,c( 1, 3, 5, 1))
})
