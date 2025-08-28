#' Generates a table of metadata with a row for each scheme (dataset)
#'
#' The row for each scheme represents the preferred run of any multiple runs,
#' according to the preference order defined in `run_stages()`
#'
#' @param groups Used to pass through any specification to the `groups`
#'  argument to [get_nhp_user_allowed_datasets]
#' @returns A tibble
#' @export
compile_tagged_runs_metadata_tbl <- function(groups = NULL) {
  run_metadata_tbl <- compile_run_metadata_tbl(groups)
  run_metadata_tbl |>
    dplyr::filter(dplyr::if_any("run_stage", \(x) x %in% run_stages())) |>
    dplyr::slice_min(match(.data[["run_stage"]], run_stages()), by = "dataset")
}

#' run stages, in order of preference
#' @keywords internal
run_stages <- function() {
  c(
    "final_report_ndg2",
    "final_report_ndg1",
    "intermediate_ndg2",
    "intermediate_ndg1",
    "initial_ndg2",
    "initial_ndg1"
  )
}


#' Read in data from the `params.json` file for each tagged run
#'
#' @param runs_metadata_tbl Tibble of metadata, as produced by
#'  [compile_tagged_runs_metadata_tbl]. If not supplied,
#'  compile_tagged_runs_metadata_tbl() will be run anyway to create the table.
#' @returns A list of run params data, each named with its scheme code
#' @export
read_all_tagged_runs_params <- function(runs_metadata_tbl = NULL) {
  runs_metadata_tbl <- runs_metadata_tbl %||% compile_tagged_runs_metadata_tbl()
  msg <- azkit:::cv_error_msg("Not all files are called {.val params.json}")
  runs_metadata_tbl[["file"]] |>
    azkit:::check_vec(\(x) basename(x) == "params.json", msg)

  results_container <- get_results_container()
  read_params_json <- function(dataset, file, container = results_container) {
    azkit::read_azure_json(container, basename(file), dirname(file)) |>
      rlang::set_names(dataset)
  }
  purrr::pmap(runs_metadata_tbl, read_params_json)
}


#' Compiles a table of metadata parameters for each model run for each scheme
#'
#' This function uses data from the "aggregated-model-results" Azure folder.
#' @inheritParams compile_tagged_runs_metadata_tbl
#' @returns A tibble
#' @export
compile_run_metadata_tbl <- function(groups = NULL) {
  allowed_datasets <- get_nhp_user_allowed_datasets(groups)
  allowed_folders_rx <- paste0(allowed_datasets, collapse = "|")

  location <- "aggregated-model-results"

  results_container <- get_results_container()
  all_json_files <- azkit::list_files(results_container, location, "json")
  all_params_files <- all_json_files |>
    purrr::keep(\(x) gregg(x, "^{location}/.*/({allowed_folders_rx})")) |>
    purrr::keep(\(x) grepl("params.json$", x))

  metadata_to_tibble <- function(x, contnr = results_container) {
    dplyr::bind_cols(list(AzureStor::get_storage_metadata(contnr, x), file = x))
  }
  all_params_files |>
    purrr::map_dfr(metadata_to_tibble) |>
    dplyr::mutate(
      dplyr::across(c("seed", "model_runs"), as.integer),
      dplyr::across(c("start_year", "end_year"), as.integer),
      dplyr::across(c("health_status_adjustment", "viewable"), as.logical)
    )
}

#' grepl a glued regex
#' @description Facilitates using regex in search/filter patterns, and puts the
#'  arguments "the right way round" (x first, then pattern), unlike `grepl()`
#' @keywords internal
gregg <- \(x, rx, g = parent.frame()) grepl(glue::glue_data(g, rx), x)
