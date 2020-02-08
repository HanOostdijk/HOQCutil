library(HOQCutil)
library(testthat)

context('replace_package_fun\n')

format_WENS2 <- function (x,WENS='NS',dig=3) {
  cat('print of  the body of internal function HOQCutil:::internal_demo_function :',sep='\n')
  print(internal_demo_function)
  NULL
}

assign('HOQC1181_format_WENS2',format_WENS2,envir = globalenv())

x1 = HOQCutil:::format_WENS(3.1415, WENS = 'NS', dig = 3)
replace_package_fun('format_WENS', 'HOQC1181_format_WENS2', 'HOQCutil',start = T)
x2 = HOQCutil:::format_WENS(3.1415, WENS = 'NS', dig = 3)
replace_package_fun('format_WENS', 'HOQC1181_format_WENS2', 'HOQCutil', start = F)
x3 = HOQCutil:::format_WENS(3.1415, WENS = 'NS', dig = 3)

rm(list='HOQC1181_format_WENS2',envir = globalenv() )

test_that("replace_package_fun works", {
  expect_identical(class(x1), "expression")
  expect_true(is.null(x2))
  expect_identical(class(x3), "expression")
})
