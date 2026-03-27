#' Prepare a lookup table with activity type labels and PoD labels for each PoD
#'
#' @returns A tibble
#' @export
get_detailed_pods <- function() {
  system.file("pod_measures.yml", package = "reskit") |>
    yaml::read_yaml() |>
    purrr::pluck("pod_measures") |>
    purrr::map(list_to_tbl) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      dplyr::across("pod_label", forcats::fct_inorder),
      dplyr::across("activity_type_label", \(x) {
        forcats::fct(x, levels = c("Inpatient", "Outpatient", "A&E"))
      })
    )
}


#' Prepare a lookup table with activity type labels and PoD labels for each PoD
#'
#' This function condenses all A&E activity to a single category - compare
#'  `get_detailed_pods()` which keeps all A&E categories
#' @rdname get_detailed_pods
#' @returns A tibble
#' @export
get_principal_pods <- function() {
  get_detailed_pods() |>
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
