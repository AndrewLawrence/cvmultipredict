test_that("known brier skill score values reproduce", {
  # The brier skill score of an binary null=intercept-only model is 0.0
  expect_equal(bss_class(
    data = data.frame(estimate = rep(0.5, 10),
                      truth = factor(rep(0:1, each = 5))),
    truth = truth,
    event_level = "second",
    estimate
  )[[".estimate"]],
  0.0)
  # the brier skill score of a binary perfect prediction is 1.0:
  expect_equal(bss_class(
    data = data.frame(estimate = rep(0:1, each = 5),
                      truth = factor(rep(0:1, each = 5))),
    truth = truth,
    estimate,
    event_level = "second"
  )[[".estimate"]],
  1.0)
})
