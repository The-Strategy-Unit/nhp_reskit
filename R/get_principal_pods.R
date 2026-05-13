#' Prepare a lookup table with activity type labels and PoD labels for each PoD
#'
#' @param use_local logical. Whether to use a local file (internal to the
#'  reskit package) or attempt to pull the lookup file from GitHub. Default is
#'  `FALSE`, meaning it will use the GitHub route. If you want to always use a
#'  local file (for example, for fully offline working), you can set the
#'  `reskit.local.lookups` option to `TRUE` using
#'  `options(reskit.local.lookups = TRUE)` or `withr::with_options()`
#' @returns A tibble
#' @export
get_detailed_pods <- function(use_local = FALSE) {
  use_local <- getOption("reskit.local.lookups") %||% use_local
  yaml_data <- system.file("pod_measures.yml", package = "reskit") |>
    yaml12::read_yaml()
  yaml_data_from_gh <- NULL
  if (!use_local) {
    yaml_raw <- possibly_get_outputs_gh_file("golem-config.yml")
    if (!is.null(yaml_raw)) {
      yaml_data_from_gh <- yaml12::parse_yaml(readr::read_lines(yaml_raw)) |>
        purrr::pluck("default")
    }
  }
  yaml_data <- yaml_data_from_gh %||% yaml_data
  yaml_data |>
    purrr::pluck("pod_measures") |>
    purrr::map(list_to_tbl) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      dplyr::across("pod_label", forcats::fct_inorder),
      dplyr::across("activity_type_label", \(x) sub("patients$", "patient", x)),
      dplyr::across("activity_type_label", \(x) {
        forcats::fct(x, levels = c("Inpatient", "Outpatient", "A&E"))
      })
    )
}


#' Prepare a lookup table with activity type labels and PoD labels for each PoD
#'
#' This function condenses all A&E activity to a single category - compare
#'  `get_detailed_pods()` which _keeps_ all A&E categories
#' @rdname get_detailed_pods
#' @returns A tibble
#' @export
get_principal_pods <- function(use_local = FALSE) {
  get_detailed_pods(use_local) |>
    dplyr::filter(dplyr::if_any("activity_type_label", \(x) x != "A&E")) |>
    dplyr::add_row(
      activity_type_label = "A&E",
      pod = "aae",
      pod_label = "A&E Arrivals"
    ) |>
    dplyr::mutate(
      dplyr::across("pod_label", forcats::fct_inorder),
      dplyr::across("activity_type_label", \(x) {
        forcats::fct(x, levels = c("Inpatient", "Outpatient", "A&E"))
      })
    )
}


#' Helper function to extract the required data fields from a list (from YAML)
#' @keywords internal
list_to_tbl <- function(lst) {
  tibble::tibble(
    # Initially we pulled 'activity_type' (eg ip, aae) and 'measure' (eg
    # admission, attendances) fields from the yaml data, but found these were
    # not needed in the data preparation pipeline, and also they messed up
    # joins, so now we just return the three columns actually used
    activity_type_label = lst[["name"]],
    pod = names(lst[["pods"]]),
    pod_label = purrr::map_chr(unname(lst[["pods"]]), "name")
  )
}
