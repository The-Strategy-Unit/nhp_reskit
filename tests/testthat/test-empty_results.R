demo_default <- function(seed = 4821L) {
  list(default = create_demo_default_tbl(seed))
}

test_that("compile_principal_pod_data keeps its output shape when empty", {
  testthat::skip_if_offline()
  demo <- demo_default()
  full <- compile_principal_pod_data(demo)
  # note the assignment sits inside expect_message(), which returns the
  # condition rather than the value of the expression
  expect_message(
    empty <- compile_principal_pod_data(demo, sites = "no_such_site"),
    class = "reskit_no_data"
  )

  expect_identical(nrow(empty), 0L)
  # the contract: an empty result is indistinguishable from a populated one
  # apart from having no rows, so downstream make_* functions cannot break
  expect_identical(names(empty), names(full))
  expect_identical(
    vapply(empty, \(x) class(x)[[1]], character(1)),
    vapply(full, \(x) class(x)[[1]], character(1))
  )
})

test_that("filtering sites before preparation leaves results unchanged", {
  testthat::skip_if_offline()
  demo <- demo_default()
  # the site guard moved ahead of prepare_principal_pod_data(); this holds only
  # because every grouping in that function includes `sitetret`
  expect_identical(
    compile_principal_pod_data(
      demo,
      sites = unique(demo[["default"]][["sitetret"]])
    ),
    compile_principal_pod_data(demo)
  )
})

test_that("a default table with no main measures returns an empty result", {
  testthat::skip_if_offline()
  demo <- demo_default()
  demo[["default"]] <- dplyr::filter(
    demo[["default"]],
    dplyr::if_any("measure", \(x) x == "procedures")
  )
  expect_message(
    empty <- compile_principal_pod_data(demo),
    class = "reskit_no_data"
  )
  expect_identical(nrow(empty), 0L)
  expect_match(no_data_reason(empty), "main-measure")
})

test_that("empty results carry an explanation", {
  testthat::skip_if_offline()
  empty <- suppressMessages(
    compile_principal_pod_data(demo_default(), sites = "no_such_site")
  )
  expect_type(no_data_reason(empty), "character")
  expect_null(no_data_reason(tibble::tibble(a = 1)))
})

test_that("make_principal_pod_table renders a placeholder rather than failing", {
  testthat::skip_if_offline()
  empty <- suppressMessages(
    compile_principal_pod_data(demo_default(), sites = "no_such_site")
  )
  tbl <- expect_no_error(make_principal_pod_table(empty))
  expect_s3_class(tbl, "gt_tbl")
  expect_match(
    gt::as_raw_html(tbl),
    "No principal PoD data",
    fixed = TRUE
  )
})
