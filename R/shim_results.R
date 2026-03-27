#' Wrangle tables from old-style results to match format of new-style results
#'
#' @param results List of old style NHP results tables
#' @returns List of NHP results tables in the "new" format
#' @export
shim_results <- function(results) {
  has_baseline_col <- \(df) "baseline" %in% colnames(df)
  results |>
    purrr::pluck("results") |> # Pluck to drop params
    purrr::modify_at("step_counts", shim_basic) |>
    purrr::modify_if(has_baseline_col, shim_with_baseline) |>
    purrr::map(ensure_character_cols)
}

shim_basic <- function(df) {
  df |>
    dplyr::select(!tidyselect::any_of("value")) |>
    tidyr::unnest_longer("model_runs", "value", "model_run")
}


shim_with_baseline <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across("model_runs", \(x) purrr::map2(.data[["baseline"]], x, c)),
      .keep = "unused"
    ) |>
    shim_basic() |>
    dplyr::mutate(dplyr::across("model_run", \(x) x - 1L))
}


ensure_character_cols <- function(df) {
  these <- c("age_group", "attendance_category", "tretspef", "tretspef_grouped", "los_group")
  dplyr::mutate(df, dplyr::across(tidyselect::any_of(these), as.character))
}
