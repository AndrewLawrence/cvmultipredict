
test_that("analysis creation and loading", {
  path <- file.path(tempdir(), "my-test-dir")

  unlink(path, recursive = TRUE)
  withr::defer(unlink(path, recursive = TRUE))

  prob <- problem(
    label = "test",
    ytype = "regression",
    y = 1:10,
    x = data.frame(x1 = rep(0:1, each = 5), x2 = rep(0:1, times = 5)),
    ids = paste0("s", 1:10),
    folds = setNames(lapply(1:10, \(x) (1:10)[-x]), paste0("fold", 1:10))
  )

  # Check no error when creating the problem:
  expect_no_error({
    analysis1 <- analysis(problem = prob,
                          dir = path)
  })

  # check it created the expected files:
  expect_true(dir.exists(path))
  expect_true(file.exists(file.path(path, "problem.RData")))
  expect_true(file.exists(file.path(path, "model_manifest.json")))
  expect_no_error(manifest(analysis1))

  # Check loading an analysis from a directory is identical to
  #   the initial creation:
  analysis1_fromdir <- analysis(dir = path)
  expect_identical(analysis1, analysis1_fromdir)


  # Check there is an error if you specify the wrong problem for an
  #   existing directory:
  prob2 <- problem(
    label = "test",
    ytype = "classification",
    y = as.factor(c(0, 1, 0, 0, 0, 1, 1, 1, 0, 1)),
    x = data.frame(x1 = rep(0:1, each = 5), x2 = rep(0:1, times = 5)),
    ids = paste0("s", 1:10),
    folds = setNames(lapply(1:10, \(x) (1:10)[-x]), paste0("fold", 1:10))
  )
  expect_error({
    analysis1_err <- analysis(dir = path, problem = prob2)
  })
})
