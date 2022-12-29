# test the functions in pdffuns.R
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

test_that("text2pdf and read pdf functions", {
  # create a pdf with the lines from regels1
  tmp     <- tempfile()
  tmp_out <- paste0(tmp,".pdf")
  regels1 <- c('en dat is één', 'en dat is twee', 'en dat is zev-e-ven')
  v <- text2pdf(regels1,tmp_out)

  # execute the functions
  x1 <- HOQCutil::read_pdf(tmp_out,by="cell")
  x2 <- HOQCutil::read_pdf(tmp_out,by="line")
  x3 <- HOQCutil::read_pdf_line(x1)

  unlink(tmp_out)

  b1 <- c(regels1,"1")                       # pagenumber is added by pandoc
  a1 <-  paste(b1,"#",sep="",collapse=" ")   # string with all words and last word in line has #
  a1 <- stringr::str_split(a1,stringr::fixed(" "))[[1]] # words
  a1a <- stringr::str_remove(a1,"#$")           # remove the #
  a1b <- stringr::str_detect(a1,"#$",negate=T)  # which words have #

  expect_true(is.null(v))
  expect_identical(x2,x3)
  expect_identical(names(x1),
         c("page", "framenr", "seqnr", "width", "height", "space", "x", "y", "text" ))
  expect_identical(names(x2),
         c("page", "framenr", "seqnr", "x", "y", "text" ))
  expect_identical(b1,dplyr::pull(x2,"text"))
  expect_identical(a1a,dplyr::pull(x1,"text"))
  expect_identical(a1b,dplyr::pull(x1,"space"))
})


