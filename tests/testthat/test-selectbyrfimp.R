test_that("prep selects the most important predictors", {
  skip_if_not_installed("ranger")
  set.seed(123)

  n <- 500

  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x4 <- rnorm(n)

  # outcome depends almost entirely on x1 and x2
  y <- factor(
    ifelse(
      3 * x1 + 2 * x2 + rnorm(n, sd = 0.1) > 0,
      "yes",
      "no"
    )
  )

  dat <- tibble::tibble(
    y, x1, x2, x3, x4
  )

  rec <-
    recipe(y ~ ., data = dat) |>
    step_selectbyrfimp(
      all_predictors(),
      n_selected = 2
    )

  rec_prep <- prep(rec)

  selected <- rec_prep$steps[[1]]$selected_predictors

  expect_true("x1" %in% selected)
  expect_true("x2" %in% selected)
  expect_equal(length(selected), 2)


  baked <- bake(rec_prep, new_data = dat)

  expect_equal(
    sort(names(baked)),
    sort(c("y", selected))
  )

  expect_length(
    rec_prep$steps[[1]]$selected_predictors,
    2
  )

  rec_n3 <-
    recipe(y ~ ., data = dat) |>
    step_selectbyrfimp(
      all_predictors(),
      n_selected = 3
    )

  rec_n3_prep <- prep(rec_n3)

  expect_length(
    rec_n3_prep$steps[[1]]$selected_predictors,
    3
  )

  # if we request more than r - the available number of predictors
  #   just return the r predictors:
  rec_n10 <-
    recipe(y ~ ., data = dat) |>
    step_selectbyrfimp(
      all_predictors(),
      n_selected = 10
    )

  rec_n10_prep <- prep(rec_n10)

  expect_length(
    rec_n10_prep$steps[[1]]$selected_predictors,
    4
  )

})

test_that("step exposes n_selected as tunable", {

  rec <-
    recipe(mpg ~ ., data = mtcars) |>
    step_selectbyrfimp(
      all_predictors()
    )

  step <- rec$steps[[1]]

  tun <- tunable(step)

  expect_true("n_selected" %in% tun$name)

  expect_equal(
    tun$component,
    "step_selectbyrfimp"
  )

  expect_equal(
    tun$source,
    "recipe"
  )
})
