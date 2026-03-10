test_that("compile_change_factor_data does what we need", {
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
      "activity_type",
      "sitetret",
      "pod",
      "pod_label",
      "activity_type_label",
      "change_factor",
      "measure",
      "model_run",
      "value",
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
      "activity_type",
      "sitetret",
      "pod",
      "pod_label",
      "activity_type_label",
      "change_factor",
      "measure",
      "model_run",
      "value",
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
    dplyr::select(seq(8))

  activity_types <- as.character(unique(step_counts_tbl[["activity_type"]]))
  pods <- unique(step_counts_tbl[["pod"]])
  measures <- unique(step_counts_tbl[["measure"]])

  set.seed(21879)
  activity_type <- sample(activity_types, 1)
  measure <- sample(measures, 1)
  expect_identical(activity_type, "ip")
  expect_identical(measure, "admissions")

  table_data <- step_counts_tbl |>
    filter_principal_data(measure, activity_type) |>
    filter_to_selected_sites(sites = NULL) |>
    summarise_for_all_sites() |>
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
