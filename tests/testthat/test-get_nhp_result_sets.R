test_that("basic outline", {
  skip_on_ci()
  allowed_datasets <- expect_no_error(get_nhp_user_allowed_datasets())
  expect_length(allowed_datasets, 139L)
  allowed_folders_rx <- paste0(allowed_datasets, collapse = "|")
  expect_length(allowed_folders_rx, 1L)
  root_dir <- Sys.getenv("AZ_RESULTS_DIRECTORY")
  version <- "v4.0"
  location <- file.path(root_dir, version)
  allowed_folders_match <- glue::glue("^{location}/({allowed_folders_rx})")

  results_container <- expect_no_error(get_results_container())
  all_json_files <- azkit::list_files(results_container, location, "json")
  # expect_length(all_json_files, 80L)

  all_params_files <- all_json_files |>
    purrr::keep(\(x) grepl(allowed_folders_match, x)) |>
    # probably all the files listed are `params.json` but let's make sure
    purrr::keep(\(x) grepl("params.json$", x))
  # expect_length(all_params_files, 79L)

  metadata_to_tibble <- function(x, container = results_container) {
    tibble::as_tibble_row(AzureStor::get_storage_metadata(container, x))
  }

  combined_metadata_df <- all_params_files |>
    purrr::map_dfr(metadata_to_tibble) |>
    expect_no_error() |>
    dplyr::mutate(
      dplyr::across(c("seed", "model_runs"), as.integer),
      dplyr::across(c("start_year", "end_year"), as.integer),
      dplyr::across(c("health_status_adjustment", "viewable"), as.logical)
    ) |>
    expect_no_warning()

  expect_length(combined_metadata_df, 14L) # ncol
  expect_equal(nrow(combined_metadata_df), length(all_params_files))

  combined_metadata_df1 <- all_params_files |>
    purrr::map_dfr(metadata_to_tibble) |>
    dplyr::mutate(file = all_params_files, .before = 1)

  metadata_to_tibble2 <- function(x, cont = results_container) {
    dplyr::bind_cols(list(file = x, AzureStor::get_storage_metadata(cont, x)))
  }
  combined_metadata_df2 <- all_params_files |>
    purrr::map_dfr(metadata_to_tibble2)
  expect_identical(combined_metadata_df1, combined_metadata_df2)

  read_json_file <- function(filepath) {
    file <- basename(filepath)
    path <- dirname(filepath)
    azkit::read_azure_json(results_container, file, path)
  }
  read_json_file(all_params_files[[1]])
})
