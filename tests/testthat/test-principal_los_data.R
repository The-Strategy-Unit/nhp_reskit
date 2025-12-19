test_that("compile_main_data does what we need", {
  skip_on_ci()
  results <- readr::read_rds(here::here("test_results.rds"))
  los_tbl <- results[["tretspef+los_group"]] |>
    dplyr::select(1:7)
  expect_shape(los_tbl, nrow = 290667L)
  col_names <- c(
    "pod",
    "sitetret",
    "tretspef",
    "los_group",
    "measure",
    "model_run",
    "value"
  )
  expect_named(los_tbl, col_names)

  measure <- "beddays"

  out1 <- los_tbl |>
    dplyr::filter(
      dplyr::if_any("measure", \(x) x == {{ measure }})
    )
  expect_shape(out1, nrow = 105884)

  out2 <- out1 |>
    inner_join_for_labels(get_principal_pods())

  col_names2 <- c(
    "pod_label",
    "sitetret",
    "tretspef",
    "los_group",
    "measure",
    "model_run",
    "value",
    "activity_type_label"
  )

  expect_named(out2, col_names2)

  out3 <- out2 |>
    relabel_pods() |> # expects activity_type_label col to be present
    dplyr::select(!"activity_type_label") |>
    calculate_principal_stats(default_group_cols("los_group")) |>
    dplyr::filter(dplyr::if_any("stat", \(x) x == "mean")) |>
    dplyr::select(!"stat")

  col_names3 <- c(
    "pod_label",
    "sitetret",
    "los_group",
    "baseline",
    "principal"
  )
  expect_named(out3, col_names3)
  expect_shape(out3, nrow = 29)

  out4 <- out3 |>
    filter_to_selected_sites(sites = NULL) |>
    summarise_for_all_sites() |>
    add_change_cols()

  col_names4 <- c(
    "pod_label",
    "los_group",
    "baseline",
    "principal",
    "change",
    "change_pct"
  )
  expect_named(out4, col_names4)
  expect_shape(out4, dim = c(26, 6))
})
