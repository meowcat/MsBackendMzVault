
test_that("Loading an mzVault library works", {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("data/tiny-massbank.db", package = "MsBackendMzVault")
  )
  expect_true(validObject(be))

})
