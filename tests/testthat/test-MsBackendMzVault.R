
test_that("Loading an mzVault library works", {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("data/tiny-massbank.db", package = "MsBackendMzVault")
  )
  expect_true(validObject(be))

})


test_that("Length works with and without subsetting", {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("data/tiny-massbank.db", package = "MsBackendMzVault")
  )

  expect_equal(
    length(be),
    252
  )

  be@filters <- list(id = c(3,5,7))

  expect_equal(
    length(be),
    3
  )

})
