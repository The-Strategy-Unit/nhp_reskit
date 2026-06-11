#' Read a selection of (or all) parquet files in an Azure directory
#'
#' @param container An Azure container.
#' @param path string. Path to an Azure directory of model results data.
#'  Potentially pulled from a field in a table of model runs metadata
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
#'     read_results_parquet_files(path = "files/v4.0/national")
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
