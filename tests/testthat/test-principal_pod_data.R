test_that("compile_principal_pod_data does what we need", {
  skip_on_ci()
  results <- readr::read_rds(here::here("test_results.rds"))
  default_tbl <- results[["default"]] |>
    dplyr::select(1:5)
  expect_shape(default_tbl, nrow = 10794L)
  col_names <- c("pod", "sitetret", "measure", "model_run", "value")
  expect_named(default_tbl, col_names)

  measures <- c(
    "admissions",
    "ambulance",
    "attendances",
    "beddays",
    "tele_attendances",
    "walk-in"
  )

  out1 <- default_tbl |>
    dplyr::filter(
      dplyr::if_any("measure", \(x) x %in% {{ measures }}) &
        dplyr::if_any("pod", \(x) x != "op_procedure")
    ) |>
    dplyr::mutate(dplyr::across("pod", \(x) sub("^aae.*$", "aae", x))) |>
    inner_join_for_labels(get_principal_pods()) |>
    relabel_pods()
  expect_shape(out1, nrow = nrow(dplyr::distinct(out1)))

  col_names1 <- c(
    "pod_label",
    "sitetret",
    "measure",
    "model_run",
    "value",
    "activity_type_label"
  )

  expect_named(out1, col_names1)

  final_activity_types <- c(
    "A&E",
    # was previously just "Inpatient" but now we are adding "Admissions" /
    # "Bed Days" in `relabel_pods()` as this seems OK and useful.
    # But if not a good idea then can be reverted.
    "Inpatient Admissions",
    "Inpatient Bed Days",
    "Outpatient"
  )
  expect_identical(
    sort(unique(as.character(out1[["activity_type_label"]]))),
    final_activity_types
  )

  out2 <- out1 |>
    calculate_principal_stats(default_group_cols("activity_type_label")) |>
    dplyr::filter(dplyr::if_any("stat", \(x) x == "mean")) |>
    dplyr::select(!"stat")

  col_names2 <- c(
    "pod_label",
    "sitetret",
    "activity_type_label",
    "baseline",
    "principal"
  )
  expect_named(out2, col_names2)
  expect_shape(out2, nrow = 29)

  out3 <- out2 |>
    filter_to_selected_sites(sites = NULL) |>
    summarise_for_all_sites() |>
    add_change_cols()

  col_names3 <- c(
    "pod_label",
    "activity_type_label",
    "baseline",
    "principal",
    "change",
    "change_pct"
  )
  expect_named(out3, col_names3)
  expect_shape(out3, dim = c(15, 6))
})
