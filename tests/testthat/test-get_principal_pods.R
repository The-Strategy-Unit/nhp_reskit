test_that("test to specify expected dims of pods lookup", {
  gpp_out <- get_principal_pods()
  expect_named(gpp_out, c("activity_type_label", "pod", "pod_label"))
  expect_identical(nrow(gpp_out), 10L)
  expect_identical(
    sort(unique(as.character(gpp_out[["activity_type_label"]]))),
    c("A&E", "Inpatient", "Outpatient")
  )
})
