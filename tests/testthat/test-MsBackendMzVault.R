
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

test_that("spectraData gives expected results",  {
  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("data/tiny-massbank.db", package = "MsBackendMzVault")
  )

  # Full spectradata
  sd_full <- spectraData(be)
  sv_full <- spectraVariables(be)

  expect_equal(
    colnames(sd_full),
    sv_full
  )
  expect_equal(
    nrow(sd_full),
    length(be)
  )

  # Subset of columns
  sv_subset <- sv_full[c(1,4,7)]
  sd_subset <- spectraData(be, columns = sv_subset)

  expect_equal(
    colnames(sd_subset),
    sv_subset
  )
  expect_equal(
    nrow(sd_subset),
    length(be)
  )

  # coreSpectraVariables and their types
  sv_core <- coreSpectraVariables()
  sd_core <- spectraData(be, columns = names(sv_core))
  expect_equal(
    colnames(sd_core),
    names(sv_core)
  )

  # check all column types
  for(colname in names(sv_core))
    expect_true(
      is(sd_core[[colname]], sv_core[[colname]])
    )


  # check that calling nonexistent columns fails
  sv_gugus <- c( names(sv_core)[1:5], "this_is_not_a_column", "that_is_not_a_column")
  expect_error(spectraData(be, columns = sv_gugus))
  sv_gugus <- c( names(sv_core)[1:5], "this_is_not_a_column")
  expect_error(spectraData(be, columns = sv_gugus))
  sv_gugus <- c("this_is_not_a_column")
  expect_error(spectraData(be, columns = sv_gugus))

  # be_small <- be[c(55,66,77)]
  # sd_subset_small <- spectraData(be_small, columns = sv_subset)
  # expect_equal(
  #   nrow(sd_subset_small),
  #   3
  # )



})
