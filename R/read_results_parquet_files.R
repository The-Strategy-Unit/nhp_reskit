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
  msg1 <- azkit:::cv_error_msg("{.path {path}} not found")
  azkit:::check_vec(path, \(x) AzureStor::blob_dir_exists(container, x), msg1)
  # error message if length(folder_name) == 0
  msg2 <- azkit:::cv_error_msg("No sub-folders found at {.path {path}}")

  folder_name <- AzureStor::list_blobs(container, path, recursive = FALSE) |>
    dplyr::filter(dplyr::if_any("isdir")) |>
    dplyr::pull("name") |>
    azkit:::check_vec(rlang::is_character, msg2)
  cur_dir <- basename(path)
  max_dttm <- max(folder_name)

  # construct error messages if length(folder_name) > 1
  folder_names <- cli::cli_vec(basename(folder_name), list(`vec-trunc` = 2))
  msg3 <- "The folder {.val {cur_dir}} contains more than 1 sub-folder.\n"
  msg4 <- c(msg3, "The sub-folders are: {folder_names}...\n")
  msg5 <- cli::cli_text(msg4, "You will need to specify the {.arg {level}}")

  if (level == "scenario" || is.null(dttm_stamp)) {
    # if length(folder_name) == 1 then return it, otherwise throw an error
    azkit:::check_scalar_type(folder_name, "character", msg5)
  } else if (dttm_stamp == "max") {
    # if we find multiple model runs, "max" means return the dir of most recent
    msg6 <- cli::cli_text(msg4, "Using the most recent model run '{max_dttm}'")
    cli::cli_alert_info(msg6)
    max_dttm
  } else {
    # the user has supplied a particular dttm_stamp so we try to return that one
    folder_name <- gregv(folder_name, "{dttm_stamp}(/)?$")
    msg7 <- azkit:::cst_error_msg("No {.val {dttm_stamp}} folder found")
    azkit:::check_scalar_type(folder_name, "string", msg7)
  }
}


#' Various checks on the input parameters to [get_results_folder_path()]
#'
#' Pulled out to a discrete function, just for neatness' sake
#' @keywords internal
check_grfp_inputs <- function(cn, root_dir, version, scheme, scenario, stamp) {
  stopifnot("container not found" = inherits(cn, "container"))
  stopifnot("`root_dir` must be a single string" = rlang::is_string(root_dir))
  stopifnot("`version` must be a single string" = rlang::is_string(version))
  stopifnot("`scheme` must be a single string" = rlang::is_string(scheme))
  stopifnot(rlang::is_scalar_character(scenario) || is.null(scenario))
  stopifnot(rlang::is_scalar_character(stamp) || is.null(stamp))
  return(NULL)
}
