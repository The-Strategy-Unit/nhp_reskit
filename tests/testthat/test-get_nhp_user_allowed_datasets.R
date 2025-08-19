test_that("we can get the providers json", {
  support_container_name <- Sys.getenv("AZ_SUPPORT_CONTAINER")
  # only run the tests if this is found (i.e. locally not in GH Actions)
  if (nzchar(support_container_name)) {
    providers <- azkit::get_container(support_container_name) |>
      azkit::read_azure_json("providers")
    expect_true(rlang::is_bare_character(providers, 138L)) # no attributes
    # test whole function
    expect_length(get_providers(), 138L)
  }
})
