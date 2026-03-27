#' Read a selection of (or all) parquet files in an Azure directory
#'
#' @param container An Azure container.
#' @param path string. Path to an Azure directory of results data. Potentially
#'  produced by [get_results_dir_path]
#' @param tables character vector. `NULL`, the default, results in all available
#'  parquet files in the `path` folder being read in. If you wish only to read
#'  in a subset of the files, specify their names here, without the ".parquet"
#'  file extension
#' @examples
#' \dontrun{
#'   data <- azkit::get_container("data_container") |>
#'     read_results_parquet_files("data/dev/national/test", "acuity")
#'
#'   data_list <- azkit::get_container("data_container") |>
#'     get_results_dir_path("files", version = "v4.0", scheme = "national") |>
#'     read_results_parquet_files()
#' }
#' @returns A named list of tibbles
#' @export
read_results_parquet_files <- function(container, path, tables = NULL) {
  stopifnot("container not found" = inherits(container, "blob_container"))
  stopifnot("`path` must be a single string" = rlang::is_string(path))

  parquet_paths <- azkit::list_files(container, path, "parquet")
  parquet_names <- sub("\\.parquet$", "", basename(parquet_paths))
  selected_tables <- tables %||% parquet_names

  absent <- setdiff(selected_tables, parquet_names) # nolint
  msg <- azkit::cv_error_msg("Table{?s} {.val {absent}} not found")
  azkit::check_vec(selected_tables, \(x) x %in% parquet_names, msg)

  selected_tables |>
    purrr::map_chr(\(x) gregv(parquet_paths, "{x}.parquet$")) |>
    rlang::set_names(selected_tables) |>
    purrr::map(\(path) azkit::read_azure_parquet(container, path))
}


#' Return a path to a specific results directory
#'
#' Provides some logic and checks to assist the user in locating a valid
#'  directory from which to read in NHP results data
#'
#' @param path string. The path to the directory within the container, where the
#'  desired results data is stored
#' @param version string. The NHP model version in the format "v3.6", or "dev"
#' @param scheme string. The code/name of the NHP scheme. May be "national" etc
#' @param scenario string. If `NULL`, the default, and only a single scenario is
#'  available within the particular combination of `version` and `scheme`, this
#'  single path will be followed. However if multiple scenarios are available,
#'  an error will be thrown. The user may avert this by simply supplying an
#'  existent scenario name
#' @param dttm_stamp string. If `NULL` (the default) and only a single model
#'  run directory is available within the particular combination of `version`
#'  `scheme` and `scenario`, this path will be returned. If `NULL`, and
#'  multiple model runs are found, an error will be thrown. Alternatively, the
#'  date-time stamp of the desired model run can be supplied. This is expected
#'  to be in the format 'YYYYMMDD_HHMMSS', and must of course match a
#'  subdirectory of the selected 'scenario' directory, else an error will be
#'  thrown. A third alternative is to supply the string `"max"`, which will
#'  return the path to the most recent model run, where multiple runs are found
#' @inheritParams read_results_parquet_files
#'
#' @returns A path to an Azure blob storage directory, as a string
#' @export
get_results_dir_path <- function(
  container,
  path,
  version,
  scheme,
  scenario = NULL,
  dttm_stamp = NULL
) {
  check_grdp_inputs(container, path, version, scheme, scenario, dttm_stamp)

  # if 'scenario' is NULL then we try to return a single scenario subdir if poss
  get_scenario <- function() {
    file.path(path, version, scheme) |>
      check_single_subdir("scenario", dttm_stamp, container)
  }
  scenario <- scenario %||% basename(get_scenario())

  # create new path including 'scenario' and pull out a model run subdir if poss
  file.path(path, version, scheme, scenario) |>
    check_single_subdir("dttm_stamp", dttm_stamp, container)
}


#' Aims to return one (1) sub-directory from the given path.
#'
#' Conducts various checks and logic to assist the user. Can either return a
#' scenario directory or a model run (`dttm_stamp`) directory
#'
#' @param path A single string created by 'file.path': a folder to search within
#' @param level Either "scenario" or "dttm_stamp"
#' @inheritParams get_results_dir_path
#' @returns A path to an Azure blob storage directory
#' @keywords internal
check_single_subdir <- function(path, level, dttm_stamp, container) {
  # error message if `path` not found
  msg1 <- azkit::ct_error_msg("{.path {path}} not found.")
  azkit::check_that(path, \(x) AzureStor::blob_dir_exists(container, x), msg1)

  dir_name <- AzureStor::list_blobs(container, path, recursive = FALSE) |>
    dplyr::filter(dplyr::if_any("isdir")) |>
    dplyr::pull("name")
  if (length(dir_name) == 0) {
    cli::cli_abort("No sub-folders found at {.path {path}}.")
  }
  cur_dir <- basename(path) # nolint
  max_dttm <- max(dir_name)

  # construct error messages if length(dir_name) > 1
  dir_names <- cli::cli_vec(basename(dir_name), list(`vec-trunc` = 2)) # nolint

  if (level == "scenario" || is.null(dttm_stamp)) {
    # if length(folder_name) == 1 then return it, otherwise throw an error
    msg2 <- azkit::cst_error_msg(c(
      "The folder {.val {cur_dir}} contains more than 1 sub-folder.",
      "i" = "The sub-folders are: {.val {dir_names}}. ",
      ">" = "You need to specify {.arg {level}}."
    ))
    azkit::check_scalar_type(dir_name, "character", msg2)
  } else if (dttm_stamp == "max") {
    # if we find multiple model runs, "max" means return the dir of most recent
    cli::cli_alert_info("Using latest model run {.val {max_dttm}}.")
    max_dttm
  } else {
    # the user has supplied a particular dttm_stamp so we try to return that one
    dir_name <- gregv(dir_name, "{dttm_stamp}(/)?$")
    msg3 <- azkit::cst_error_msg("No {.val {dttm_stamp}} directory found.")
    azkit::check_scalar_type(dir_name, "string", msg3)
  }
}


#' Various checks on the input parameters to [get_results_dir_path]
#'
#' Pulled out to a discrete function, just for neatness' sake
#' @keywords internal
check_grdp_inputs <- function(cn, root_dir, version, scheme, scenario, stamp) {
  stopifnot("container not found" = inherits(cn, "blob_container"))
  stopifnot("`root_dir` must be a single string" = rlang::is_string(root_dir))
  stopifnot("`version` must be a single string" = rlang::is_string(version))
  stopifnot("`scheme` must be a single string" = rlang::is_string(scheme))
  stopifnot(rlang::is_scalar_character(scenario) || is.null(scenario))
  stopifnot(rlang::is_scalar_character(stamp) || is.null(stamp))
  TRUE
}
