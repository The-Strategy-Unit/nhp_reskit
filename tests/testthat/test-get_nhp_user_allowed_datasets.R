test_that("we can get the providers json", {
  skip_on_ci()
  providers <- azkit::get_container(Sys.getenv("AZ_SUPPORT_CONTAINER")) |>
    azkit::read_azure_json("providers")
  expect_true(rlang::is_bare_character(providers, 138L)) # no attributes
  # test whole function
  expect_length(get_providers(), 138L)
})
