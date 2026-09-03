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


test_that("compile_distribution_plot_data keeps its output shape when empty", {
  testthat::skip_if_offline()
  demo <- demo_default()
  full <- compile_distribution_plot_data(demo, "admissions")
  # note the assignment sits inside expect_message(), which returns the
  # condition rather than the value of the expression
  expect_message(
    empty <- demo |>
      compile_distribution_plot_data("admissions", sites = "no_such_site"),
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
test_that("compile_distrib_summary_data keeps its output shape when empty", {
  testthat::skip_if_offline()
  demo <- demo_default()
  full <- compile_distribution_summary_data(demo)
  # note the assignment sits inside expect_message(), which returns the
  # condition rather than the value of the expression
  expect_message(
    empty <- compile_distribution_summary_data(demo, sites = "no_such_site"),
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
  expect_match(no_data_reason(empty), "produced an empty table")
})

test_that("empty results carry an explanation", {
  testthat::skip_if_offline()
  empty <- suppressMessages(
    compile_principal_pod_data(demo_default(), sites = "no_such_site")
  )
  expect_type(no_data_reason(empty), "character")
  expect_null(no_data_reason(tibble::tibble(a = 1)))
})

# LOS data tests

demo_los_tbl <- function(seed = 4821L) {
  list(`tretspef+los_group` = create_demo_tretspef_losgroup_tbl(seed))
}

test_that("compile_principal_los_data keeps its output shape when empty", {
  testthat::skip_if_offline()
  demo <- demo_los_tbl()
  full <- compile_principal_los_data(demo, measure = "admissions")
  # note the assignment sits inside expect_message(), which returns the
  # condition rather than the value of the expression
  expect_message(
    empty <- demo |>
      compile_principal_los_data(measure = "admissions", sites = "fake_site"),
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


# change factor checks

demo_cf_tbl <- function(seed = 4821L) {
  list(step_counts = create_demo_stepcounts_tbl(seed))
}

test_that("compile_change_factor_data keeps its output shape when empty", {
  testthat::skip_if_offline()
  demo <- demo_cf_tbl()
  full <- compile_change_factor_data(demo, "admissions", "ip")
  # note the assignment sits inside expect_message(), which returns the
  # condition rather than the value of the expression
  expect_message(
    empty <- demo |>
      compile_change_factor_data("admissions", "ip", sites = "fake_site"),
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


test_that("compile_tpma_impact_data keeps its output shape when empty", {
  testthat::skip_if_offline()
  demo <- demo_cf_tbl()
  full <- compile_tpma_impact_data(demo, "admissions", "ip")
  # note the assignment sits inside expect_message(), which returns the
  # condition rather than the value of the expression
  expect_message(
    empty <- demo |>
      compile_tpma_impact_data("admissions", "ip", sites = "fake_site"),
    class = "reskit_no_data"
  )

  expect_shape(empty, nrow = 0)
  # the contract: an empty result is indistinguishable from a populated one
  # apart from having no rows, so downstream make_* functions cannot break
  expect_identical(purrr::map_chr(empty, class), purrr::map_chr(full, class))
})


demo_da_tbl <- function(seed = 4821L) {
  list(
    create_demo_sex_agegroup_tbl(seed),
    create_demo_sex_tretspef_tbl(seed)
  ) |>
    rlang::set_names(c("sex+age_group", "sex+tretspef_grouped"))
}


test_that("compile_detailed_activity_data keeps its output shape when empty", {
  testthat::skip_if_offline()
  demo <- demo_da_tbl()
  full <- compile_detailed_activity_data(demo, "admissions")
  # note the assignment sits inside expect_message(), which returns the
  # condition rather than the value of the expression
  expect_message(
    empty <- demo |>
      compile_detailed_activity_data("admissions", sites = "fake_site"),
    class = "reskit_no_data"
  )

  expect_shape(empty, nrow = 0)
  # the contract: an empty result is indistinguishable from a populated one
  # apart from having no rows, so downstream make_* functions cannot break
  expect_identical(purrr::map_chr(empty, class), purrr::map_chr(full, class))

  full2 <- compile_detailed_activity_data(
    demo,
    "admissions",
    aggregation = "tretspef_grouped"
  )
  # note the assignment sits inside expect_message(), which returns the
  # condition rather than the value of the expression
  expect_message(
    empty2 <- demo |>
      compile_detailed_activity_data(
        "admissions",
        aggregation = "tretspef_grouped",
        sites = "fake_site"
      ),
    regexp = "selected sites",
    class = "reskit_no_data"
  )
  expect_message(
    empty3 <- demo |>
      compile_detailed_activity_data(
        "admissions",
        aggregation = "tretspef_grouped",
        activity_type = "op"
      ),
    regexp = "selected measure",
    class = "reskit_no_data"
  )

  expect_shape(empty2, nrow = 0)
  expect_shape(empty3, nrow = 0)
  # the contract: an empty result is indistinguishable from a populated one
  # apart from having no rows, so downstream make_* functions cannot break
  expect_identical(purrr::map_chr(empty2, class), purrr::map_chr(full2, class))
  expect_identical(purrr::map_chr(empty3, class), purrr::map_chr(full2, class))
})


# visualisations

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
