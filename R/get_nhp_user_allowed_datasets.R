#' Return a vector of results datasets that the user is allowed to access
#'
#' @param groups A vector of groups that the user belongs to. `NULL` by default.
#' @returns A character vector
#' @export
get_nhp_user_allowed_datasets <- function(groups = NULL) {
  providers_vec <- get_providers()
  filter_groups <- \(x) grepv("^nhp_provider_", x)
  trim_group_names <- \(x) sub("^nhp_provider_", "", x)

  if (is.null(groups) || any(c("nhp_devs", "nhp_power_users") %in% groups)) {
    c("synthetic", providers_vec)
  } else {
    filtered_groups <- trim_group_names(filter_groups(groups))
    c("synthetic", intersect(providers_vec, filtered_groups))
  }
}

#' Return a vector of provider codes from the supporting data container
#' @returns A character vector
#' @export
get_providers <- function() {
  get_support_container() |>
    azkit::read_azure_json("providers")
}
