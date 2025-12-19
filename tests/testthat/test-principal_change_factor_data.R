test_that("compile_principal_change_factor_data does what we need", {
  skip_on_ci()
  results <- readr::read_rds(here::here("test_results.rds"))
  step_counts_tbl <- results[["step_counts"]] |>
    dplyr::select(1:8)
  expect_shape(step_counts_tbl, nrow = 70656L)
  col_names <- c(
    "activity_type",
    "sitetret",
    "pod",
    "change_factor",
    "strategy",
    "measure",
    "model_run",
    "value"
  )
  expect_named(step_counts_tbl, col_names)

  # test with baseline included
  include_baseline <- TRUE
  keep_measures <- c("admissions", "beddays")

  bsline_filtered <- step_counts_tbl |>
    dplyr::filter(.data[["change_factor"]] != "baseline") |>
    expect_no_error()
  dat_prepared <- if (include_baseline) step_counts_tbl else bsline_filtered
  expect_identical(dat_prepared, step_counts_tbl)

  out1 <- dat_prepared |>
    dplyr::filter(
      dplyr::if_any("measure", \(x) x %in% {{ keep_measures }}) &
        dplyr::if_any("value", \(x) x != 0)
    )
  expect_shape(out1, nrow = 25908L)

  out2 <- out1 |>
    dplyr::mutate(
      dplyr::across("pod", \(x) sub("^aae.*$", "aae", x)),
      dplyr::across("measure", uppercase_init)
    ) |>
    inner_join_for_labels(get_principal_pods()) |>
    relabel_pods() |>
    dplyr::left_join(get_tpma_label_lookup(), "strategy") |>
    dplyr::select(!"strategy") |>
    dplyr::mutate(
      dplyr::across("tpma_label", \(x) dplyr::if_else(is.na(x), "-", x))
    ) |>
    expect_no_error()
  expect_named(
    out2,
    c(
      "pod_label",
      setdiff(col_names, c("pod", "strategy")),
      "activity_type_label",
      "tpma_label"
    )
  )
  expect_shape(out2, nrow = 25908L)

  out3 <- out2 |>
    dplyr::summarise(
      dplyr::across("value", mean),
      .by = change_factor_sort_vars()
    ) |>
    expect_no_error()
  expect_shape(out3, dim = c(110L, 7L))

  # test with baseline excluded
  include_baseline <- FALSE
  dat_prepared <- if (include_baseline) step_counts_tbl else bsline_filtered
  expect_identical(dat_prepared, bsline_filtered)

  out1 <- dat_prepared |>
    dplyr::filter(
      dplyr::if_any("measure", \(x) x %in% {{ keep_measures }}) &
        dplyr::if_any("value", \(x) x != 0)
    )
  expect_shape(out1, nrow = 21812L)

  out2 <- out1 |>
    dplyr::mutate(
      dplyr::across("pod", \(x) sub("^aae.*$", "aae", x)),
      dplyr::across("measure", uppercase_init)
    ) |>
    inner_join_for_labels(get_principal_pods()) |>
    relabel_pods() |>
    dplyr::left_join(get_tpma_label_lookup(), "strategy") |>
    dplyr::select(!"strategy") |>
    dplyr::mutate(
      dplyr::across("tpma_label", \(x) dplyr::if_else(is.na(x), "-", x))
    ) |>
    expect_no_error()
  expect_named(
    out2,
    c(
      "pod_label",
      setdiff(col_names, c("pod", "strategy")),
      "activity_type_label",
      "tpma_label"
    )
  )
  expect_shape(out2, nrow = 21812L)

  out3 <- out2 |>
    dplyr::summarise(
      dplyr::across("value", mean),
      .by = change_factor_sort_vars()
    ) |>
    expect_no_error()
  expect_shape(out3, dim = c(94L, 7L))
})


test_that("main compile function does what is expected", {
  skip_on_ci()
  results <- readr::read_rds(here::here("test_results.rds"))
  step_counts_tbl <- results[["step_counts"]] |>
    dplyr::select(1:8)
  prepared_data <- prepare_principal_cf_data(step_counts_tbl, TRUE)

  activity_types <- unique(prepared_data[["activity_type_label"]])
  pods <- unique(prepared_data[["pod_label"]])
  measures <- unique(prepared_data[["measure"]])

  set.seed(21879)
  activity_type <- sample(activity_types, 1)
  pods <- sort(sample(pods, 3))
  measure <- sample(measures, 1)
  expect_identical(activity_type, "Inpatient")
  expect_identical(
    pods,
    c("Daycase Admission", "Maternity Admission", "Non-Elective Admission")
  )
  expect_identical(measure, "Beddays")

  table_data <- prepared_data |>
    filter_to_selected_sites(sites = NULL) |>
    summarise_for_all_sites() |>
    filter_principal_cf_data(activity_type, pods, measure) |>
    dplyr::summarise(dplyr::across("value", sum), .by = "change_factor") |>
    expect_no_error()
  expect_shape(table_data, dim = c(8, 2))

  table_data_as_factor <- table_data |>
    dplyr::mutate(
      dplyr::across("change_factor", \(x) {
        forcats::fct_reorder(x, .data[["value"]], .desc = TRUE)
      }),
      # baseline may now not be the first factor level, move it back to start
      dplyr::across("change_factor", \(x) forcats::fct_relevel(x, "baseline"))
    ) |>
    dplyr::arrange(dplyr::pick("change_factor")) |>
    expect_no_error()
  expect_identical(
    levels(table_data_as_factor[["change_factor"]])[[1]],
    "baseline"
  )
  expect_identical(
    levels(table_data_as_factor[["change_factor"]])[[2]],
    "demographic_adjustment"
  )
  expect_identical(
    levels(table_data_as_factor[["change_factor"]])[[8]],
    "health_status_adjustment"
  )
})


test_that("see why numbers don't look right", {
  skip_on_ci()
  results <- readr::read_rds(here::here("test_results.rds"))
  step_counts_tbl <- results[["step_counts"]] |>
    dplyr::select(1:8)
  sct <- dplyr::filter(step_counts_tbl, activity_type == "ip")

  exp_pods <- c(
    "ip_elective_admission",
    "ip_elective_daycase",
    "ip_maternity_admission",
    "ip_non-elective_admission",
    "ip_regular_day_attender"
  )
  expect_identical(sort(unique(sct[["pod"]])), exp_pods)
  sct <- dplyr::filter(sct, measure == "beddays")

  sct_data <- sct |>
    dplyr::summarise(
      dplyr::across("value", mean),
      .by = c("change_factor", "pod", "strategy", "sitetret")
    )
  expect_shape(sct_data, dim = c(69, 5))
  sct_data_final <- sct_data |>
    dplyr::summarise(
      dplyr::across("value", sum),
      .by = "change_factor"
    ) |>
    dplyr::arrange(dplyr::desc(dplyr::pick("value"))) |>
    move_baseline_row_to_top() |>
    dplyr::mutate(
      cmvalue = cumsum(.data[["value"]]),
      hidden = dplyr::lag(.data[["cmvalue"]], 1, 0) + pmin(.data[["value"]], 0),
      total = abs(.data[["value"]]) + .data[["hidden"]]
    ) |>
    dplyr::select(!"cmvalue") |>
    dplyr::arrange(dplyr::pick("value"))

  expect_shape(sct_data_final, dim = c(8, 4))

  b_val <- dplyr::filter(sct_data_final, change_factor == "baseline")[["value"]]
  expect_lt(b_val, 3e5)
  expect_gt(b_val, 2e5)
})
