test_that("get model runs (data_dirs) overview", {
  skip_on_ci()
  res <- get_results_container()
  expect_s3_class(res, "blob_container")
  root_dir <- Sys.getenv("AZ_RESULTS_DIRECTORY", NA)
  expect_false(is.na(root_dir))
  version <- "v3.6"
  scheme <- "RCF"
  scenario <- "test"

  scenario_path <- file.path(root_dir, version, scheme, scenario)
  expect_true(AzureStor::blob_dir_exists(res, scenario_path))

  data_dirs <- AzureStor::list_blobs(res, scenario_path, recursive = FALSE) |>
    dplyr::pull("name")
  expect_length(data_dirs, 2L) # currently 2 model runs in this scenario dir

  all(purrr::map_lgl(data_dirs, \(x) AzureStor::blob_dir_exists(res, x))) |>
    expect_true()
})


test_that("read parquet files overview", {
  skip_on_ci()
  res <- get_results_container()
  expect_s3_class(res, "blob_container")
  root_dir <- Sys.getenv("AZ_RESULTS_DIRECTORY", NA)
  expect_false(is.na(root_dir))
  version <- "v3.6"
  scheme <- "RCF"
  scenario <- "test"
  scenario_path <- file.path(root_dir, version, scheme, scenario)

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

  parquet_data <- parquet_paths |>
    purrr::map(\(x) azkit::read_azure_parquet(res, x, info = FALSE)) |>
    rlang::set_names(parquet_names)
  expect_true(all(purrr::map_lgl(parquet_data, \(x) inherits(x, "tbl_df"))))
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


test_that("bughunting", {
  skip_on_ci()
  # https://github.com/The-Strategy-Unit/nhp_reskit/pull/24#pullrequestreview-3201907282

  res <- get_results_container()
  expect_s3_class(res, "blob_container")
  root_dir <- Sys.getenv("AZ_RESULTS_DIRECTORY", NA)
  expect_false(is.na(root_dir))

  version <- "v4.0"
  expect_true(rlang::is_string(version))
  scheme <- "RRK"
  expect_true(rlang::is_string(scheme))

  check_grfp_inputs(res, root_dir, version, scheme, NULL, NULL) |>
    expect_true()

  orig_path <- file.path(root_dir, version, scheme)
  folder_name <- AzureStor::list_blobs(res, orig_path, recursive = FALSE) |>
    dplyr::filter(dplyr::if_any("isdir")) |>
    dplyr::pull("name")
  expect_true(rlang::is_scalar_character(folder_name))
  azkit:::check_scalar_type(folder_name, "character", "fail") |>
    expect_no_error()
  azkit:::check_scalar_type(folder_name, "character", "fail") |>
    expect_equal(folder_name) # folder_name should be passed through

  exp_path <- file.path(root_dir, version, scheme, "test-2324-2")

  scen_path <- check_single_subdir(orig_path, "scenario", NULL, res)
  expect_equal(scen_path, paste0(exp_path, "/")) # file.path doesn't add final /

  expect_no_message(check_single_subdir(scen_path, "dttm_stamp", NULL, res))
  check_single_subdir(scen_path, "dttm_stamp", "max", res) |>
    expect_message("Using latest model run")
})
