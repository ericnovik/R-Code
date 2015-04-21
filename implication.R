library("testthat")

`%=>%` <- function(ant, cons) {
  if(ant & !cons) {
    return(FALSE)
  } else
    return(TRUE)
}

test_that("Implication operator is acting correctly", {
  expect_true(TRUE %=>% TRUE)
  expect_false(TRUE %=>% FALSE)
  expect_true(FALSE %=>% TRUE)
  expect_true(FALSE %=>% FALSE)  
})

