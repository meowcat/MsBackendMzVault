
test_that("Loading an mzVault library works", {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("test-data/tiny-massbank.db", package = "MsBackendMzVault")
  )
  expect_true(validObject(be))

})


test_that("Length works with and without subsetting", {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("test-data/tiny-massbank.db", package = "MsBackendMzVault")
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
    file = system.file("test-data/tiny-massbank.db", package = "MsBackendMzVault")
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

test_that("Handling null and zero values in mz fields works", {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("test-data/tiny-massbank.db", package = "MsBackendMzVault")
  )

  # Spectra with NULL and zero-length mz entries
  be_zerolength <- be_with_zero_and_null(be, to_zero = c(2, 3))
  expect_no_error(spectraData(be_zerolength))
  expect_equal(
    be_zerolength[c(2,3)]$mz,
    IRanges::NumericList(numeric(), numeric(), compress = FALSE)
  )
  expect_true(
    all(lengths(be_zerolength[-c(2,3)]$mz) > 0)
  )

  be_nulls <- be_with_zero_and_null(be, to_null = c(6,8))
  expect_no_error(spectraData(be_nulls))
  expect_equal(
    be_nulls[c(6,8)]$mz,
    IRanges::NumericList(numeric(), numeric(), compress = FALSE)
  )
  expect_true(
    all(lengths(be_nulls[-c(6,8)]$mz) > 0)
  )

})


test_that("Subsetting operator [ works", {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("test-data/tiny-massbank.db", package = "MsBackendMzVault")
  )

  selected_ids <- c(5,6,7,8,5,7,8,6,9)
  be_sub <- be[selected_ids]

  expect_equal(
    be_sub$acquisitionNum,
    selected_ids
  )

  # sub-subset based on index (should NOT return SpectrumID 2,4,5 but 6,8,5)
  subselect_ids <- c(2,4,5)
  subselected_ids <- selected_ids[subselect_ids]
  be_subsub <- be_sub[subselect_ids]

  expect_equal(
    be_subsub$acquisitionNum,
    subselected_ids
  )

  # sub-subset+duplicate based on index
  subselect_ids <- c(5, 5, 2, 4, 5)
  subselected_ids <- selected_ids[subselect_ids]
  be_subsub <- be_sub[subselect_ids]

  expect_equal(
    be_subsub$acquisitionNum,
    subselected_ids
  )

  # What happens if we select a spectrum outside of the range?
  # Expecting an error
  expect_error(be_sub[c(0,4)])
  expect_error(be_sub[c(1,111)])

  # subsetting by logical vector
  # too short should fail
  expect_error(be_sub[c(TRUE, TRUE, FALSE)])
  be_sublogical <- be_sub[selected_ids < 8]
  expect_equal(
    be_sublogical$acquisitionNum,
    selected_ids[selected_ids < 8]
  )


})



test_that("Operator $ works", {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("test-data/tiny-massbank.db", package = "MsBackendMzVault")
  )

  sd <- spectraData(be)

  for(var in names(coreSpectraVariables())) {
    expect_equal(
      `$`(be, !!var),
      sd[,var]
      )
  }

  for(var in spectraVariables(be)) {
    expect_equal(
      `$`(be, !!var),
      sd[,var]
    )
  }

  expect_error(
    be$hahaha_not_var
  )

})

test_that("lengths() works", {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("test-data/tiny-massbank.db", package = "MsBackendMzVault")
  )

  expect_equal(
    lengths(be),
    be$mz |> purrr::map_int(length)
  )

  be_sub <- be[c(66,55,44)]
  expect_equal(
    lengths(be_sub),
    be_sub$mz |> purrr::map_int(length)
  )

  be_empty <- be[c()]
  expect_equal(
    lengths(be_empty),
    integer()
  )

})


test_that("isEmpty works",  {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("test-data/tiny-massbank.db", package = "MsBackendMzVault")
  )

  be_z <- be_with_zero_and_null(be, to_zero = 4, to_null = 2)

  be_zz <- be_z[c(2,4)]

  expect_equal(
    isEmpty(be_zz),
    c(TRUE, TRUE)
  )

  be_zx <- be_z[c(1,2,4,3)]
  expect_equal(
    isEmpty(be_zx),
    c(FALSE, TRUE, TRUE, FALSE)
  )

})


test_that("tic works",  {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("test-data/tiny-massbank.db", package = "MsBackendMzVault")
  )

  be_z <- be_with_zero_and_null(be, to_zero = 4, to_null = 2)
  be_zz <- be_with_zero_and_null(be, to_zero = 4, to_null = 2, col = "blobIntensity")

  be_zzz <- be_zz[c(2,4)]

  expect_equal(
    tic(be_zzz),
    c(0, 0)
  )

  be_zx <- be_z[c(1,2,4,3)]
  expect_equal(
    tic(be_zx),
    c(188108, 56568, 30880, 57784)
  )

})


test_that("bulk accessor methods work", {

  be <- backendInitialize(
    MsBackendMzVault(),
    file = system.file("test-data/tiny-massbank.db", package = "MsBackendMzVault")
  )

  accessorMethods <- c(
    "acquisitionNum", "centroided", "collisionEnergy", "dataOrigin",
    "intensity", "isolationWindowLowerMz", "isolationWindowTargetMz",
    "isolationWindowUpperMz", "msLevel", "mz" ,"polarity", "precScanNum",
    "precursorCharge", "precursorIntensity", "precursorMz", "rtime",
    "scanIndex", "smoothed")
  sd <- spectraData(be)

  for(method in accessorMethods) {
    expect_equal(
      do.call(method, list(be)),
      sd[, method]
    )

  }

})
