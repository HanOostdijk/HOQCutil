
context('set_mfe_lookup\n')
lookup_df = tibble::tribble(
  ~col1, ~col2,
  'x1' , 'one' ,
  'x1;x2' , 'two' ,
  'X2' , 'two' ,
  'x3;x4' , 'three four' ,
  'x3' , 'three'
)  %>%
  transform(col1 = strsplit(col1, ";"))

datavec = c('x1 and x2', 'only x2', 'x5', 'x3 and x4','x3 and x5')

r1 = mfe_lookup(datavec,lookup_df,default=TRUE)
r1e = c("one", "two", "x5", "three four", "three" )

r2 = mfe_lookup(datavec,lookup_df,default=FALSE)
r2e = c("one", "two", "", "three four", "three" )

r3 = mfe_lookup(datavec,lookup_df,default='not found!')
r3e = c("one", "two", "not found!", "three four", "three" )

r4 = mfe_lookup(c(),lookup_df,default='not found!')
r4e = character(0)

r5 = mfe_lookup(datavec,lookup_df,case_sensitive=TRUE)
r5e =  c("one", "only x2", "x5", "three four", "three" )


test_that("set_mfe_lookup", {

  expect_identical(r1,r1e)
  expect_identical(r2,r2e)
  expect_identical(r3,r3e)
  expect_identical(r4,r4e)
  expect_identical(r5,r5e)
})

