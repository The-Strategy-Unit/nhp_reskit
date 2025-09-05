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
