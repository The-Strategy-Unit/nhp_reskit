test_that("compile_high_level_summary does what's expected", {
  results <- readr::read_rds(here::here("test_results.rds"))
  default_tbl <- results[["default"]] |>
    dplyr::select(1:5)
  expect_identical(nrow(default_tbl), 10794L)
  col_names <- c("pod", "sitetret", "measure", "model_run", "value")
  expect_named(default_tbl, col_names)

  measures <- c("beddays", "admissions")
  filt1 <- default_tbl |>
    dplyr::filter(dplyr::if_any("measure", \(x) x %in% {{ measures }})) |>
    expect_no_error()
  expect_identical(nrow(filt1), 4112L)

  measures <- "beddays"
  filt2 <- default_tbl |>
    dplyr::filter(dplyr::if_any("measure", \(x) x %in% {{ measures }})) |>
    expect_no_error()
  expect_identical(nrow(filt2), 2056L)

  aae_out1 <- filt2 |>
    dplyr::mutate(dplyr::across("pod", \(x) sub("^aae.*$", "aae", x))) |>
    expect_no_error()
  # because we already filtered to beddays, there's no aae* pod rows left anyway
  expect_identical(aae_out1, filt2)

  all_bedday_pods <- c(
    "ip_elective_admission",
    "ip_elective_daycase",
    "ip_maternity_admission",
    "ip_non-elective_admission",
    "ip_regular_day_attender"
  )
  expect_identical(unique(aae_out1[["pod"]]), all_bedday_pods)

  test_out1 <- aae_out1 |>
    dplyr::inner_join(get_principal_pods(), c("pod", "measure")) |>
    dplyr::relocate("pod_name") |>
    dplyr::select(!c("pod", "activity_type")) |>
    dplyr::distinct()
  new_names <- setdiff(c("pod_name", col_names, "activity_type_label"), "pod")
  expect_named(test_out1, new_names)

  # all pods ought to be present in the get_principal_pods() lookup, so
  # no rows lost through inner_join()
  expect_identical(nrow(test_out1), nrow(filt2))
})
