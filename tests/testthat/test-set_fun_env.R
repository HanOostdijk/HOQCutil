
context('set_fun_env global version1\n')
te = environment()

target1 <- function(){
  pi
}

test_that("correct environment", {
  expect_identical(environment(target1),te)
})

my_e = environment(target1)

test_that("set environment for 'global' function", {

  x1=set_fun_env("target1",ns = "HOQCutil",envir=my_e)

  expect_true(is.environment(x1))
  expect_identical(x1,environment(target1))
  expect_identical(environment(target1),asNamespace('HOQCutil'))
})

test_that("reset environment for 'global' function", {

  x2=set_fun_env("target1",ns =my_e, envir=my_e)
  expect_true(is.environment(x2))
  expect_identical(x2,environment(target1))
  expect_identical(environment(target1),te)
})

context('set_fun_env global version2\n')
# same test but done in the same way as for specific environment

rm(list='target1')
target1 <- function(){
  pi
}
my_e = environment(target1)

test_that("correct environment", {
  expect_identical(
    eval(parse(text="environment(target1)"),envir=my_e),
    my_e)
})

test_that("set environment for 'global' function version2", {

  x1=set_fun_env("target1",ns = "HOQCutil",envir=my_e)

  expect_true(is.environment(x1))
  expect_identical(
    x1,
    eval(parse(text="environment(target1)"),envir=my_e)
  )
  expect_identical(x1,asNamespace('HOQCutil'))
})

test_that("reset environment for 'global' function version2", {

  x2=set_fun_env("target1",ns =my_e, envir=my_e)
  expect_true(is.environment(x2))
  expect_identical(
    x2,
    eval(parse(text="environment(target1)"),envir=my_e)
  )
  expect_identical(x2,my_e)
})

context('set_fun_env specific environment\n')

my_e = new.env()

eval(parse(text=
  "target2 <- function(x){
    is.numeric(x)
  }"
  ),  envir=my_e)

test_that("correct environment", {
  expect_identical(
    eval(parse(text="environment(target2)"),envir=my_e),
    my_e)
})

test_that("set environment for function in specific environment", {

  x1=set_fun_env("target2",ns = "HOQCutil",envir=my_e)

  expect_true(is.environment(x1))
  expect_identical(
        x1,
        eval(parse(text="environment(target2)"),envir=my_e)
        )
  expect_identical(x1,asNamespace('HOQCutil'))
})

test_that("reset environment for function in specific environment", {

  x2=set_fun_env("target2",ns =my_e, envir=my_e)
  expect_true(is.environment(x2))
  expect_identical(
        x2,
        eval(parse(text="environment(target2)"),envir=my_e)
        )
  expect_identical(x2,my_e)
})


context('set_fun_env for inner function\n')
testfun1 <- function() {

  target3 <- function() {
    pi
  }

  my_e = environment(target3)
  x1 = set_fun_env("target3", ns = "HOQCutil", envir = my_e)
  x2 = set_fun_env("target3", ns = my_e, envir = my_e)

  list(e1 = my_e,
       e2 = asNamespace('HOQCutil'),
       e3 = x1,
       e4 = x2)
}

res1 = testfun1()

test_that("set/reset environment for inner function", {

  expect_identical(res1$e1,res1$e4)
  expect_identical(res1$e2,res1$e3)
})





