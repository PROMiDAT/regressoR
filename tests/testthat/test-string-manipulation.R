context("String Manipulation")

test_that("exe function works correctly",{
  n.e <- new.env()
  n.e$str <- "just a test."
  
  expect_equal(exe("5+5"), 10)
  expect_equal(exe("str", envir = n.e), "just a test.")
})

test_that("extract_code function works correctly",{
  f1 <- function(...) sum(...)
  
  expect_equal(extract_code("f1"), "f1 <- function (...) \nsum(...)")
  expect_equal(extract_code("exe"), "exe <- function (..., envir = parent.frame()) \n{\n    eval(parse(text = paste0(...)), envir = envir)\n}")
})

test_that("as_string_c function works correctly",{
  expect_equal(as_string_c(c("A", "B", "C")), "c('A','B','C')")
  expect_equal(as_string_c(c(5, 6, 7)), "c('5','6','7')")
  expect_equal(as_string_c(c(5, 6, 7), quote = FALSE), "c(5,6,7)")
})

test_that("translate function works correctly",{
  expect_equal(translate("knnl"), "K Vecinos Más Cercanos")
  expect_equal(translate("knnl", "en"), "K Nearest Neighbors")
})

test_that("models_mode function works correctly",{
  x <- list('knnl-mode1' = 1, 'knnl-mode2' = 2, 'knnl-mode2' = 5)
  expect_equal(models_mode(x), c('K Vecinos Más Cercanos-mode1','K Vecinos Más Cercanos-mode2','K Vecinos Más Cercanos-mode2'))
})