#' Prepare a lookup table with activity type labels and PoD labels for each PoD
#' @returns A tibble
#' @keywords internal
get_principal_pods <- function() {
  aae_row <- tibble::tibble_row(
    activity_type_label = "A&E",
    pod = "aae",
    pod_label = "A&E Attendance"
  )
  get_pod_measures_lst() |>
    purrr::pluck("pod_measures") |> # with current structure of pod_measures.yml
    purrr::discard_at("aae") |> # excluded here and replaced beloww/ custom row
    purrr::map(list_to_tbl) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(aae_row) |>
    # check this is correct (effectively this is a check on the yaml structure):
    dplyr::distinct() |>
    dplyr::mutate(
      dplyr::across("pod_label", forcats::fct_inorder),
      dplyr::across("activity_type_label", \(x) {
        forcats::fct(x, levels = c("Inpatient", "Outpatient", "A&E"))
      })
    )
}

#' Helper function to extract the reuiqred data fields from a list (from YAML)
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

#' Read in list of PoD labelling data from local yaml file
#' @keywords internal
get_pod_measures_lst <- \() yaml::read_yaml(here::here("inst/pod_measures.yml"))
