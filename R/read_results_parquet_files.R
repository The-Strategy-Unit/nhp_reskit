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
