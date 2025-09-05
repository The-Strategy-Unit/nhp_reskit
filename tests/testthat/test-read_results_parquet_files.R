test_that("get model runs (data_dirs) overview", {
  res_container_name <- Sys.getenv("AZ_RESULTS_CONTAINER")
  if (nzchar(res_container_name)) {
    res <- get_results_container()

    location <- "aggregated-model-results"
    version <- "v3.6"
    scheme <- "RCF"
    scenario <- "test"

    scenario_path <- glue::glue("{location}/{version}/{scheme}/{scenario}")
    expect_true(AzureStor::blob_dir_exists(res, scenario_path))

    data_dirs <- AzureStor::list_blobs(res, scenario_path, recursive = FALSE) |>
      dplyr::pull("name")
    expect_length(data_dirs, 2L) # currently 2 model runs in this scenario dir

    all(purrr::map_lgl(data_dirs, \(x) AzureStor::blob_dir_exists(res, x))) |>
      expect_true()
  }
})


test_that("read parquet files overview", {
  res_container_name <- Sys.getenv("AZ_RESULTS_CONTAINER")
  if (nzchar(res_container_name)) {
    res <- get_results_container()

    location <- "aggregated-model-results"
    version <- "v3.6"
    scheme <- "RCF"
    scenario <- "test"
    scenario_path <- file.path(location, version, scheme, scenario)

    # for this test block we will just use one model run directory
    data_dir <- AzureStor::list_blobs(res, scenario_path, recursive = FALSE) |>
      dplyr::pull("name") |>
      max()

    get_parquet_paths <- function(container, path) {
      AzureStor::list_blobs(container, path, recursive = FALSE) |>
        dplyr::pull("name") |>
        gregv("\\.parquet$")
    }
    parquet_paths <- get_parquet_paths(res, data_dir)

    get_parquet_names <- \(x) sub("^(.*/)([[:graph:]]+)(\\.parquet)$", "\\2", x)
    parquet_names <- get_parquet_names(parquet_paths)

    tbl_names <- c(
      "acuity",
      "age",
      "attendance_category",
      "avoided_activity",
      "default",
      "sex+age_group",
      "sex+tretspef",
      "step_counts",
      "tretspef_raw+los_group",
      "tretspef_raw"
    )
    expect_equal(parquet_names, tbl_names)

    testthat::skip_on_ci()
    parquet_data <- parquet_paths |>
      purrr::map(\(x) azkit::read_azure_parquet(res, x, info = FALSE)) |>
      rlang::set_names(parquet_names)
    expect_true(all(purrr::map_lgl(parquet_data, \(x) inherits(x, "tbl_df"))))
  }
})


test_that("table selection works", {
  tbl_names <- c(
    "acuity",
    "age",
    "attendance_category",
    "avoided_activity",
    "default",
    "sex+age_group",
    "sex+tretspef",
    "step_counts",
    "tretspef_raw+los_group",
    "tretspef_raw"
  )

  tables <- c("default", "acuity")

  check_args <- function(tables = NULL) {
    rlang::arg_match(tables, values = tbl_names, multiple = TRUE)
  }
  expect_equal(check_args(tables), tables)

  tables_wrong <- c("acuity", "default", "random")
  absent <- setdiff(tables_wrong, tbl_names)
  msg <- azkit:::cv_error_msg("Table{?s} {absent} {?is/are} not present")
  azkit:::check_vec(tables_wrong, \(x) x %in% tbl_names, msg) |>
    expect_error("random is not present")

  test_vec <- rlang::set_names(letters[seq(3)], tables_wrong)
  filt_vec <- test_vec[tables] # vector reordered by order of names
  expect_equal(unname(filt_vec), c("b", "a"))
  filt_vec <- test_vec[tables[c(2, 1)]]
  expect_equal(unname(filt_vec), c("a", "b"))

  test_named <- purrr::map(test_vec, toupper) # map retains names
  expect_named(test_named)
})
