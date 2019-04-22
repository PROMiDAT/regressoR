context("Data Manipulation")

library(regressoR)

test_that("combine_names works for different types",{
  x <- c("A", "B", "C")
  y <- c("1", "2", "3")
  z <- c(1, 2, 3)
  
  expect_equal(combine_names(x, y), c("A.1","B.1","C.1","A.2","B.2","C.2","A.3","B.3","C.3"))
  expect_equal(combine_names(x, z), c("A.1","B.1","C.1","A.2","B.2","C.2","A.3","B.3","C.3"))
})

test_that("colnames_empty works for all cases",{
  expect_equal(colnames_empty(iris), colnames(iris))
  expect_equal(colnames_empty(NULL), "")
})
