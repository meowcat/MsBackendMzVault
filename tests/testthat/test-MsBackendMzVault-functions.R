




test_that("Counting records works with and without subsetting", {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("data/tiny-massbank.db", package = "MsBackendMzVault")
  )

  expect_equal(
    MsBackendMzVault:::get_filtered_spectrumtable_count(be),
    252
  )

  be@filters <- list(id = c(3,5,7))

  expect_equal(
    MsBackendMzVault:::get_filtered_spectrumtable_count(be),
    3
  )

})
