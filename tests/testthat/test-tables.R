test_that("gt_bar formulae behave as required", {
  # gt_bar() takes a vector `x` and derives 3 values from it:
  #  1. the width of a transparent "empty bar" that fills space (if any) from
  #      the left edge of the column to the start (lh end) of the value bar
  #  2. the width of a "value bar" that represents the value shown in the table
  #      column. This may be coloured yellow and extend leftwards from the zero
  #      point on the x-axis, for negative values of `x`, or coloured red and
  #      extend rightwards from the zero point, for positive values of `x`.
  #  3. the correct colour for the bar (including transparent for "empty bars")
  # for each value of x.
  #
  # We have three main scenarios to account for:
  #  1. All values of `x` are positive
  #  2. All values of `x` are negative
  #  3. `x` spans zero, containing both negative and positive values.
  #
  # Ideally we can create generic formulae that work across all three scenarios
  # instead of applying different formulae with if..else logic. Let's see...

  set.seed(6543)
  # x is all positive
  x <- sample(seq(10), 5) # 7 9 4 3 8

  x_min <- min(min(x), 0) # if min(x) > 0, set x_min to 0
  x_max <- max(max(x), 0) # if max(x) < 0, set x_max to 0
  x_range <- x_max - x_min
  x_pmin <- pmin(x, 0)
  expect_equal(x_min, 0)
  expect_equal(x_max, 9)
  expect_equal(x_range, 9)
  expect_equal(x_pmin, rep(0, 5))

  # now for our formulae:
  # empty_bar widths here should all be 0
  # value_bar widths here should be the same as x / x_range
  # all very straightforward in this scenario
  empty_bar_widths <- (abs(x_min - x_pmin) / x_range)
  expect_equal(empty_bar_widths, rep(0, 5))
  value_bar_widths <- (abs(x) / x_range)
  expect_equal(value_bar_widths, x / 9)

  # x is all negative
  x <- x * -1 # -7 -9 -4 -3 -8

  x_min <- min(min(x), 0) # if min(x) > 0, set x_min to 0
  x_max <- max(max(x), 0) # if max(x) < 0, set x_max to 0
  x_range <- x_max - x_min
  x_pmin <- pmin(x, 0)
  expect_equal(x_min, -9)
  expect_equal(x_max, 0)
  expect_equal(x_range, 9)
  expect_equal(x_pmin, x)

  # empty_bar widths here should be the gaps necessary between -9 and the value
  # value_bar widths here should be the abs values of x (div by x_range)
  # all fairly straightforward in this scenario though we do now have to think
  # about getting the empty bar widths correct
  empty_bar_widths <- (abs(x_min - x_pmin) / x_range)
  expect_equal(empty_bar_widths, c(2, 0, 5, 6, 1) / 9)
  value_bar_widths <- (abs(x) / x_range)
  expect_equal(value_bar_widths, c(7, 9, 4, 3, 8) / 9)

  # x spans 0
  x <- x + 5 # -2 -4 1 2 -3

  x_min <- min(min(x), 0) # if min(x) > 0, set x_min to 0
  x_max <- max(max(x), 0) # if max(x) < 0, set x_max to 0
  x_range <- x_max - x_min
  x_pmin <- pmin(x, 0)
  expect_equal(x_min, -4)
  expect_equal(x_max, 2)
  expect_equal(x_range, 6)
  expect_equal(x_pmin, c(-2, -4, 0, 0, -3))

  # empty_bar widths here should be the gaps necessary between -4 and:
  # the lh end of the value bar (eg -3 for -3 (= width 1), 0 for 2 (= width 4))
  # value_bar widths here should be the same as abs(x) / x_range
  # a bit more complicated to think about in this scenario, but I think the
  # formulae still work, unchanged from above scenarios
  empty_bar_widths <- (abs(x_min - x_pmin) / x_range)
  expect_equal(empty_bar_widths, c(2, 0, 4, 4, 1) / 6)
  value_bar_widths <- (abs(x) / x_range)
  expect_equal(value_bar_widths, c(2, 4, 1, 2, 3) / 6)
})
