#' Read in a file from the NHP Outputs app GitHub repo
#'
#' @param file string. The name of the file to read in
#' @param folder string. The folder where the file is located. `"inst"` by
#'   default. Set to `""` to use the root folder of the repo.
#' @returns The file contents as a stream, to be passed to a reader function
#' @keywords internal
get_outputs_gh_file <- function(file, folder = "inst") {
  httr2::request("https://api.github.com") |>
    httr2::req_url_path_append("repos") |>
    httr2::req_url_path_append("The-Strategy-Unit") |>
    httr2::req_url_path_append("nhp_outputs") |>
    httr2::req_url_path_append("contents") |>
    httr2::req_url_path_append(folder) |>
    httr2::req_url_path_append(file) |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json() |>
    purrr::pluck("content") |>
    base64enc::base64decode()
}


#' Read in a file from the TPMAs GitHub repo
#'
#' @param file string. The name of the file to read in
#' @param folder string. The folder where the file is located. `"reference"` by
#'   default. Set to `""` to use the root folder of the repo.
#' @returns The file contents as a stream, to be passed to a reader function
#' @keywords internal
get_tpmas_gh_file <- function(file, folder = "reference") {
  httr2::request("https://api.github.com") |>
    httr2::req_url_path_append("repos") |>
    httr2::req_url_path_append("The-Strategy-Unit") |>
    httr2::req_url_path_append("TPMAs") |>
    httr2::req_url_path_append("contents") |>
    httr2::req_url_path_append(folder) |>
    httr2::req_url_path_append(file) |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json() |>
    purrr::pluck("content") |>
    base64enc::base64decode()
}


possibly_get_outputs_gh_file <- \(...) purrr::possibly(get_outputs_gh_file)(...)
possibly_get_tpmas_gh_file <- \(...) purrr::possibly(get_tpmas_gh_file)(...)
