get_principal_pods <- function() {
  aae_row <- tibble::tibble_row(
    activity_type_label = "A&E",
    pod = "aae",
    pod_name = "A&E Attendance"
  )
  get_pod_measures_lst() |>
    purrr::pluck("pod_measures") |> # with current structure of pod_measures.yml
    purrr::discard_at("aae") |> # excluded here and replaced w/ custom row below
    purrr::map(list_to_tbl) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(aae_row) |>
    # check this is correct (effectively this is a check on the yaml structure):
    dplyr::distinct() |>
    dplyr::mutate(dplyr::across("pod_name", forcats::fct_inorder))
}


list_to_tbl <- function(lst) {
  tibble::tibble(
    activity_type_label = lst[["name"]],
    pod = names(lst[["pods"]]),
    pod_name = purrr::map_chr(lst[["pods"]], "name")
  )
}


get_pod_measures_lst <- \() yaml::read_yaml(here::here("inst/pod_measures.yml"))
