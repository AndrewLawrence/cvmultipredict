test_that("problem construction works", {

  expect_no_error(
    {
      res <- problem(
        "test",
        "regression",
        1:10,
        data.frame(x1 = rep(0:1, each = 5)),
        ids = paste0("s", 1:10),
        folds = setNames(lapply(1:10, \(x) (1:10)[-x]), paste0("fold", 1:10))
      )
    }
  )

  expect_identical(class(res), c("regression", "problem"))

})
