#' Read a selection of (or all) parquet files in an Azure directory
#'
#' @param path string. Path to an Azure directory of results data. As produced
#'  by [get_results_folder_path()]
#' @param tables character vector. `NULL`, the default, results in all available
#'  parquet files in the `path` folder being read in. If you wish only to read
#'  in a subset of the files, specify these here, without the ".parquet" file
#'  extension
#' @param container Azure container. Uses [get_results_container()] by default
#' @examples
#' \dontrun{
#'   data <- read_results_parquet_files("data/dev/national/test", "acuity")
#'
#'   get_results_folder_path(version = "v4.0", scheme = "national") |>
#'     read_results_parquet_files()
#' }
#' @returns A named list of tibbles
#' @export
read_results_parquet_files <- function(path, tables = NULL, container = NULL) {
  stopifnot("`path` must be a single string" = rlang::is_string(path))
  container <- container %||% get_results_container()
  stopifnot("container not found" = inherits(container, "blob_container"))
  tables <- tables %||% all_parquet_names()
  tables <- rlang::arg_match(tables, all_parquet_names(), multiple = TRUE)

  file_paths <- AzureStor::list_blobs(container, path, recursive = FALSE) |>
    dplyr::pull("name") |>
    gregv("\\.parquet$")
  parquet_names <- sub("^(.*/)([[:graph:]]+)(\\.parquet)$", "\\2", file_paths)

  absent <- setdiff(tables, parquet_names)
  msg <- azkit:::cv_error_msg("Table{?s} {.val {absent}} {?is/are} not present")
  azkit:::check_vec(tables, \(x) x %in% parquet_names, msg)

  named_paths <- rlang::set_names(file_paths, parquet_names)
  named_paths[tables] |>
    purrr::map(\(p) azkit::read_azure_parquet(container, p))
}


# fmt: skip
all_parquet_names <- function() {
  c(
    "acuity", "age", "attendance_category", "avoided_activity", "default",
    "sex+age_group", "sex+tretspef", "step_counts", "tretspef_raw+los_group", "tretspef_raw"
  )
}


#' Specify a results folder path
#'
#' Provides some logic and checks to assist the user in locating
#'  a valid folder from which to read in NHP results data
#'
#' @param version string. The NHP model version in the format "v3.6", or "dev"
#' @param scheme string. The code/name of the NHP scheme. May be "national" etc
#' @param scenario string. If `NULL`, the default, and only a single scenario is
#'  available within the particular combination of `version` and `scheme`, this
#'  single folder path will be followed. However if multiple scenarios are
#'  available, an error will be thrown. The user may avert this by simply
#'  supplying an existent scenario name
#' @param dttm_stamp string. If `NULL` (the default) and only a single model
#'  run directory is available within the particular combination of `version`
#'  `scheme` and `scenario`, this folder path will be returned. If `NULL`, and
#'  multiple model runs are found, an error will be thrown. Alternatively, the
#'  date-time stamp of the desired model run can be supplied. This is expected
#'  to be in the format 'YYYYMMDD_HHMMSS', and must of course match a
#'  subdirectory of the selected 'scenario' folder, else an error will be
#'  thrown. A third alternative is to supply the string `"max"`, which will
#'  return the path to the most recent model run, where multiple runs are found
#' @param root_dir string. The name of the root folder within the container
#'  where the all desired results data is stored. Uses the value of the
#'  environment variable "AZ_RESULTS_DIRECTORY" by default
#' @inheritParams read_results_parquet_files
#'
#' @returns A path to an Azure blob storage directory, as a string
#' @export
get_results_folder_path <- function(
  version,
  scheme,
  scenario = NULL,
  dttm_stamp = NULL,
  container = NULL,
  root_dir = NULL
) {
  container <- container %||% get_results_container()
  root_dir <- root_dir %||% Sys.getenv("AZ_RESULTS_DIRECTORY", NA)
  check_grfp_inputs(container, root_dir, version, scheme, scenario, dttm_stamp)

  # if 'scenario' is NULL then we try to return a single scenario subdir if poss
  get_scenario <- function() {
    file.path(root_dir, version, scheme) |>
      check_single_subdir("scenario", dttm_stamp, container)
  }
  scenario <- scenario %||% basename(get_scenario())

  # create new path including 'scenario' and pull out a model run subdir if poss
  file.path(root_dir, version, scheme, scenario) |>
    check_single_subdir("dttm_stamp", dttm_stamp, container)
}


#' Aims to return one (1) sub-directory from the given path.
#'
#' Conducts various checks and logic to assist the user. Can either return a
#' scenario directory or a model run (`dttm_stamp`) directory
#'
#' @param path A single string created by 'file.path': a folder to search within
#' @param level Either "scenario" or "dttm_stamp"
#' @inheritParams get_results_folder_path
#' @returns A path to an Azure blob storage directory
#' @keywords internal
check_single_subdir <- function(path, level, dttm_stamp, container) {
  # error message if `path` not found
  msg1 <- azkit:::cv_error_msg("{.path {path}} not found.")
  azkit:::check_vec(path, \(x) AzureStor::blob_dir_exists(container, x), msg1)
  # error message if length(folder_name) == 0
  msg2 <- azkit:::cv_error_msg("No sub-folders found at {.path {path}}.")

  folder_name <- AzureStor::list_blobs(container, path, recursive = FALSE) |>
    dplyr::filter(dplyr::if_any("isdir")) |>
    dplyr::pull("name") |>
    azkit:::check_vec(rlang::is_character, msg2)
  cur_dir <- basename(path)
  max_dttm <- max(folder_name)

  # construct error messages if length(folder_name) > 1
  folder_names <- cli::cli_vec(basename(folder_name), list(`vec-trunc` = 2))
  msg3 <- "The folder {.val {cur_dir}} contains more than 1 sub-folder. "
  msg4 <- c(msg3, "The sub-folders are: {.val {folder_names}}. ")
  msg5 <- azkit:::cst_error_msg(c(msg4, "You need to specify {.arg {level}}."))

  if (level == "scenario" || is.null(dttm_stamp)) {
    # if length(folder_name) == 1 then return it, otherwise throw an error
    azkit:::check_scalar_type(folder_name, "character", msg5)
  } else if (dttm_stamp == "max") {
    # if we find multiple model runs, "max" means return the dir of most recent
    cli::cli_alert_info("Using latest model run {.val {max_dttm}}.")
    max_dttm
  } else {
    # the user has supplied a particular dttm_stamp so we try to return that one
    folder_name <- gregv(folder_name, "{dttm_stamp}(/)?$")
    msg6 <- azkit:::cst_error_msg("No {.val {dttm_stamp}} folder found.")
    azkit:::check_scalar_type(folder_name, "string", msg6)
  }
}


#' Various checks on the input parameters to [get_results_folder_path()]
#'
#' Pulled out to a discrete function, just for neatness' sake
#' @keywords internal
check_grfp_inputs <- function(cn, root_dir, version, scheme, scenario, stamp) {
  stopifnot("container not found" = inherits(cn, "blob_container"))
  stopifnot("`root_dir` must be a single string" = rlang::is_string(root_dir))
  stopifnot("`version` must be a single string" = rlang::is_string(version))
  stopifnot("`scheme` must be a single string" = rlang::is_string(scheme))
  stopifnot(rlang::is_scalar_character(scenario) || is.null(scenario))
  stopifnot(rlang::is_scalar_character(stamp) || is.null(stamp))
  return(TRUE)
}
