read_zipped_results_params <- function(container, path) {
  container |>
    azkit::read_azure_jsongz(path, opts = list(obj_of_arrs_to_df = FALSE)) |>
    purrr::pluck("params") |>
    purrr::modify_tree(leaf = \(x) if (length(x) > 1) as.list(x) else x)
}

read_results_params <- function(container, path) {
  container |>
    azkit::read_azure_json(path, opts = list(obj_of_arrs_to_df = FALSE)) |>
    purrr::modify_tree(leaf = \(x) if (length(x) > 1) as.list(x) else x)
}


read_zipped_results_params <- function(container, path) {
  azkit::read_azure_file(container, path) |>
    jsonlite::parse_gzjson_raw(simplifyVector = FALSE) |>
    purrr::pluck("params")
}

read_results_params <- function(container, path) {
  withr::with_tempfile("dl", {
    AzureStor::download_blob(container, path, dest = dl)
    jsonlite::fromJSON(dl, simplifyVector = FALSE)
  })
}
