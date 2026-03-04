#' Prepare data from the `sex+age_group` or `sex+tretspef_grouped` table
#'
#' @param results List of NHP results tables
#' @param tretspef_lookup A tibble, or a function that returns a tibble,
#'  containing a `code` column (used as a key for joining to the tretspef
#'  table) and a `tretspef` column that provides friendly labels for specialties
#' @param aggregation string. One of "age_group" or "tretspef"
#' @inheritParams compile_change_factor_data
#' @export
compile_detailed_activity_data <- function(
  results,
  measure,
  tretspef_lookup = get_tretspef_lookup(),
  activity_type = c("ip", "op", "aae"),
  aggregation = c("age_group", "tretspef"),
  pods = NULL,
  sites = NULL
) {
  activity_type <- rlang::arg_match(activity_type)
  aggregation <- rlang::arg_match(aggregation)
  if (aggregation == "age_group") {
    init_data <- prepare_age_group_data(results)
  } else {
    # aggregation == "tretspef"
    init_data <- prepare_tretspef_data(results, tretspef_lookup)
  }

  init_data |>
    prepare_detailed_activity_data(aggregation) |>
    dplyr::mutate(activity_type = sub("^([a-z]*).*", "\\1", .data[["pod"]])) |>
    filter_principal_data(measure, activity_type, pods) |>
    filter_to_selected_sites(sites) |>
    summarise_for_all_sites() |>
    summarise_for_all_pods(aggregation) |>
    add_change_cols() |>
    dplyr::arrange(dplyr::pick(tidyselect::all_of(c("sex", aggregation))))
}

#' Prepare data from the 'sex+age_group' results table
#'
#' @inheritParams compile_detailed_activity_data
#' @keywords internal
prepare_age_group_data <- function(results) {
  init_tbl <- results[["sex+age_group"]]
  age_groups <- unique(init_tbl[["age_group"]])
  init_digits <- as.integer(sub("^(\\d+)(.*)", "\\1", age_groups))
  age_groups_ordered <- age_groups[match(sort(init_digits), init_digits)]
  init_tbl |>
    dplyr::mutate(
      # correctly sort age group factor levels numerically
      dplyr::across("age_group", \(x) forcats::fct(x, age_groups_ordered))
    )
}


#' Prepare data from the 'sex+tretspef_grouped' results table
#'
#' @inheritParams compile_detailed_activity_data
#' @keywords internal
prepare_tretspef_data <- function(results, tretspef_lookup) {
  results[["sex+tretspef_grouped"]] |>
    dplyr::rename(code = "tretspef_grouped") |>
    dplyr::left_join(tretspef_lookup, "code") |>
    dplyr::mutate(
      dplyr::across("tretspef", \(x) dplyr::coalesce(x, .data[["code"]]))
    ) |>
    dplyr::select(!"code")
}


#' Data preparation step for 'activity in detail' table
#'
#' @returns A tibble
#' @keywords internal
prepare_detailed_activity_data <- function(init_data, aggregation) {
  init_data |>
    dplyr::mutate(
      dplyr::across("sex", convert_sex_codes),
      dplyr::across("sex", \(x) forcats::fct(x, c("Female", "Male"))),
      dplyr::across("pod", \(x) sub("^aae.*$", "aae", x))
    ) |>
    inner_join_for_labels(get_principal_pods()) |>
    relabel_pods() |>
    calculate_principal_stats(detailed_activity_sort_vars(aggregation)) |>
    keep_mean_only()
}


detailed_activity_sort_vars <- function(aggregation) {
  # fmt: skip
  c(
    "activity_type_label", "pod_label", "measure", "sitetret", "sex",
    aggregation, "model_run"
  )
}


summarise_for_all_pods <- function(tbl, aggregation) {
  tbl |>
    dplyr::summarise(
      dplyr::across(tidyselect::where(is.numeric), sum),
      .by = tidyselect::all_of(c("sex", aggregation))
    )
}


#' Prepare a site-level summary of detailed activity results by PoD and LoS
#'
#' Intended to be used to create a table to be exported to .csv/.xlsx
#' @inheritParams compile_detailed_activity_data
#' @returns A tibble
#' @export
export_detailed_activity_data <- function(
  results,
  tretspef_lookup = get_tretspef_lookup(),
  aggregation = c("age_group", "tretspef"),
  sites = NULL
) {
  aggregation <- rlang::arg_match(aggregation)
  if (aggregation == "age_group") {
    init_data <- prepare_age_group_data(results)
  } else {
    # aggregation == "tretspef"
    init_data <- prepare_tretspef_data(results, tretspef_lookup)
  }
  sort_cols <- c("sex", "activity_type_label", "pod_label", aggregation)
  init_data |>
    filter_to_selected_sites(sites) |>
    prepare_detailed_activity_data(aggregation) |>
    add_change_cols() |>
    dplyr::arrange(dplyr::pick(tidyselect::all_of(sort_cols)))
}
