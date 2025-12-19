#' Get an Azure storage container for supporting data
#'
#' @param scv_name The name of the *environment variable* that stores the
#'  name of the supporting data container. "AZ_SUPPORT_CONTAINER" by default.
#' @returns An Azure storage container
#' @examples \dontrun{  get_support_container() }
#' @export
get_support_container <- function(scv_name = "AZ_SUPPORT_CONTAINER") {
  not_found_msg <- "variable {.envvar {scv_name}} not found"
  Sys.getenv(scv_name, NA) |>
    azkit:::check_scalar_type("string", not_found_msg) |>
    azkit::get_container() |>
    azkit::check_container_class()
}


#' Get an Azure storage container for results data
#'
#' @param rcv_name The name of the *environment variable* that stores the
#'  name of the results container. "AZ_RESULTS_CONTAINER" by default.
#' @returns An Azure storage container
#' @examples \dontrun{  get_results_container() }
#' @export
get_results_container <- function(rcv_name = "AZ_RESULTS_CONTAINER") {
  not_found_msg <- "variable {.envvar {rcv_name}} not found"
  Sys.getenv(rcv_name, NA) |>
    azkit:::check_scalar_type("string", not_found_msg) |>
    azkit::get_container() |>
    azkit::check_container_class()
}
