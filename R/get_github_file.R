#' Read in a file from a Strategy Unit GitHub repo
#'
#' @param repo string. The name of the repository in which to find the file
#' @param folder string. The folder where the file is located. Set to `""` to
#'   use the root folder of the repo.
#' @param file string. The name of the file to read in
#' @returns The file contents as a stream, to be passed to a reader function
#' @keywords internal
get_su_gh_file <- function(repo, folder, file) {
  httr2::request("https://api.github.com") |>
    httr2::req_url_path_append("repos") |>
    httr2::req_url_path_append("The-Strategy-Unit") |>
    httr2::req_url_path_append(repo) |>
    httr2::req_url_path_append("contents") |>
    httr2::req_url_path_append(folder) |>
    httr2::req_url_path_append(file) |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json() |>
    purrr::pluck("content") |>
    base64enc::base64decode()
}


#' Read in a file from the NHP Outputs app GitHub repo
#'
#' @param ... Pass the name of the file in via `...`
#' @keywords internal
get_outputs_gh_file <- function(...) {
  purrr::partial(get_su_gh_file, repo = "nhp_outputs", folder = "inst")(...)
}


#' Read in a file from the TPMAs GitHub repo
#'
#' @inheritParams get_outputs_gh_file
#' @keywords internal
get_tpmas_gh_file <- function(...) {
  purrr::partial(get_su_gh_file, repo = "TPMAs", folder = "reference")(...)
}

possibly_get_outputs_gh_file <- \(...) purrr::possibly(get_outputs_gh_file)(...)
possibly_get_tpmas_gh_file <- \(...) purrr::possibly(get_tpmas_gh_file)(...)
