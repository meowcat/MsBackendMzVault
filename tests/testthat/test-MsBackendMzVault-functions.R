




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


test_that("Processing the column mapping works",  {

  # loading with an empty object should work if nothing in the mapping
  # requires the object
  column_map <- list(
    full_specification = list(col = "inputcol", read_fun = ~ .x + 1),
    char_for_col= "inputcol",
    using_constant = constant(55),
    function_along_acquisitionNum = ~ .x - 1,
    constant_NA = NA
  )
  processed_column_map <- load_spectravariables_mapping(
    object = NA,
    mapping = column_map)

  # check the full_specification variant
  expect_equal(
    processed_column_map$full_specification$col,
    "inputcol"
  )
  expect_equal(
    processed_column_map$full_specification$read_fun(c(1,2,3)),
    c(2,3,4)
  )

})
