
test_that("cvmultipredict works", {
  path <- file.path(tempdir(), "my-test-dir")

  unlink(path, recursive = TRUE)
  withr::defer(unlink(path, recursive = TRUE))


  y <- rep(1:10, each = 20)

  x <- as.data.frame(unname(model.matrix(~ as.factor(y) - 1)))[, -1]

  p <- cvmultipredict::problem(x = x, y = y,
                               label = "quickreg", ytype = "regression",
                               ids = paste0("s", 1:200),
                               folds = list(half1 = seq(1, 200, by = 2),
                                            half2 = seq(2, 200, by = 2)))

  m <- cvmultipredict::default_model_manifest()
  m$models <- m$models[which(names(m$models) %in% "linear")]
  m$models$linear$params$pca <- FALSE

  a <- cvmultipredict::analysis(problem = p,
                                dir = file.path(path, "quickreg"),
                                manifest = m, create = TRUE,
                                overwrite_manifest = TRUE,
                                overwrite_problem = TRUE)

  # loading from an analysis that doesn't exist will cause an error:
  expect_error(
    suppressMessages(cvmultipredict::cvmultipredict(a, .RUN = FALSE))
  )

  # check no error running the linear model:
  expect_no_error({
    res1 <- suppressMessages(cvmultipredict::cvmultipredict(a))
  })

  expect_no_error({
    res1 <- pool_results(res1)
  })

  # check values are expected:
  expect_all_equal(unlist(res1[res1$.metric == "mse", ".estimate"]), 0.0)
  expect_all_equal(unlist(res1[res1$.metric == "rsq", ".estimate"]), 1.0)

  # expect no error loading the linear model:
  expect_no_error({
    res2 <- suppressMessages(cvmultipredict::cvmultipredict(a))
  })

  expect_no_error({
    res2 <- pool_results(res2)
  })
  expect_identical(res1, res2)
})
