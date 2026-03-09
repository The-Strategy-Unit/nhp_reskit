#' shim_results
#' @param results List of old style NHP results tables
#' @returns List of NHP results tables in the "new" format
#' @export
shim_results <- function(results) {
  results |>
    purrr::pluck("results") |> # Pluck to drop params |>
    shim_default() |>
    shim_step_counts() |>
    shim_tretspef_los() |>
    shim_sex_age_group() |>
    shim_sex_tretspef_grouped() |>
    purrr::keep_at(c(
      "default",
      "step_counts",
      "sex+tretspef_grouped",
      "sex+age_group",
      "tretspef+los_group"
    )) # only keep shimmed results
}

shim_default <- function(results) {
  purrr::modify_at(results, "default", \(df) {
    df |>
      dplyr::mutate(
        model_runs = purrr::map2(.data$baseline, .data$model_runs, c)
      ) |>
      dplyr::select(c("pod", "sitetret", "measure", value = "model_runs")) |>
      dplyr::mutate(
        model_run = purrr::map(.data$value, \(x) seq_along(x) - 1)
      ) |>
      tidyr::unnest(cols = c("model_run", "value"))
  })
}

shim_step_counts <- function(results) {
  purrr::modify_at(results, "step_counts", \(df) {
    df |>
      dplyr::select(c(
        "pod",
        "change_factor",
        "strategy",
        "sitetret",
        "activity_type",
        "measure",
        value = "model_runs"
      )) |>
      dplyr::mutate(
        model_run = purrr::map(.data$value, \(x) seq_along(x))
      ) |>
      tidyr::unnest(cols = c("model_run", "value"))
  })
}


shim_sex_tretspef_grouped <- function(results) {
  purrr::modify_at(results, "sex+tretspef_grouped", \(df) {
    df |>
      dplyr::mutate(
        model_runs = purrr::map2(.data$baseline, .data$model_runs, c)
      ) |>
      dplyr::select(c(
        "pod",
        "sitetret",
        "sex",
        "tretspef_grouped",
        "measure",
        value = "model_runs"
      )) |>
      dplyr::mutate(
        model_run = purrr::map(.data$value, \(x) seq_along(x) - 1)
      ) |>
      tidyr::unnest(cols = c("model_run", "value"))
  })
}

shim_sex_age_group <- function(results) {
  purrr::modify_at(results, "sex+age_group", \(df) {
    df |>
      dplyr::mutate(
        model_runs = purrr::map2(.data$baseline, .data$model_runs, c)
      ) |>
      dplyr::select(c(
        "pod",
        "sitetret",
        "sex",
        "age_group",
        "measure",
        value = "model_runs"
      )) |>
      dplyr::mutate(
        model_run = purrr::map(.data$value, \(x) seq_along(x) - 1)
      ) |>
      dplyr::mutate(age_group = as.character(.data$age_group)) |>
      tidyr::unnest(cols = c("model_run", "value"))
  })
}

shim_tretspef_los <- function(results) {
  purrr::modify_at(results, "tretspef+los_group", \(df) {
    df |>
      dplyr::mutate(
        model_runs = purrr::map2(.data$baseline, .data$model_runs, c)
      ) |>
      dplyr::select(c(
        "pod",
        "sitetret",
        "tretspef",
        "los_group",
        "measure",
        value = "model_runs"
      )) |>
      dplyr::mutate(
        model_run = purrr::map(.data$value, \(x) seq_along(x) - 1)
      ) |>
      dplyr::mutate(los_group = as.character(.data$los_group)) |>
      tidyr::unnest(cols = c("model_run", "value"))
  })
}
