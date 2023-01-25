




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

  ### check the full_specification variant
  # col
  expect_equal(
    processed_column_map$full_specification$col,
    "inputcol"
  )
  # read_fun
  expect_equal(
    processed_column_map$full_specification$read_fun(c(1,2,3)),
    c(2,3,4)
  )

  ### check the char_for_col variant
  # col
  expect_equal(
    processed_column_map$char_for_col$col,
    "inputcol"
  )
  # read_fun
  expect_equal(
    processed_column_map$char_for_col$read_fun(c(1,2,3)),
    c(1,2,3)
  )

  ### check the constant(v) variant
  # col
  expect_equal(
    processed_column_map$using_constant$col,
    "acquisitionNum"
  )
  # read_fun
  expect_equal(
    processed_column_map$using_constant$read_fun(c(22,33,11)),
    c(55,55,55)
  )

  ### check the function_along_acquisitionNum variant
  # col
  expect_equal(
    processed_column_map$function_along_acquisitionNum$col,
    "acquisitionNum"
  )
  # read_fun
  expect_equal(
    processed_column_map$function_along_acquisitionNum$read_fun(c(22,33,11)),
    c(21,32,10)
  )

  ### check the NA variant
  # col
  expect_equal(
    processed_column_map$constant_NA$col,
    "acquisitionNum"
  )
  # read_fun
  expect_equal(
    processed_column_map$constant_NA$read_fun(c(22,33,11,44)),
    c(NA, NA, NA, NA)
  )

})
