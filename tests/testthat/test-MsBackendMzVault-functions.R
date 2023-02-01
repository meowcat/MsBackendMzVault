




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
    "SpectrumId"
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
    "SpectrumId"
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
    "SpectrumId"
  )
  # read_fun
  expect_equal(
    processed_column_map$constant_NA$read_fun(c(22,33,11,44)),
    c(NA, NA, NA, NA)
  )

})

test_that("replacing NaN with NA works", {
  x1 <- c(1,2,NaN,3,4)

  y1 <- fast_replace_nan(x1)

  expect_equal(
    y1,
    c(1,2,NA,3,4)
  )
})


test_that("parsing collision energies works", {

  ce <- c("35% (nominal)", "60 % (nominal)", "85% (nominal)",
    "20% (nominal)",  "50 eV", "45 (nominal)", "90 % (nominal)",
    "20% (nominal)", "CE30",  "10 eV", "25% (nominal)", "45 eV",
    "15 % (nominal)", "45 % (nominal)",  "CE75",  "Ramp 17.4-26.1 eV",
    NA, "gugus", "")

  ce_parsed <- c(35, 60, 85, 20, 50, 45, 90, 20, 30, 10,
                 25, 45, 15, 45, 75,  21.75, NA, NA, NA)

  expect_equal(
    mzvault_read_collisionenergy(ce),
    ce_parsed
  )
})



test_that("parsing polarities works", {

  polarities <- c(
    "P", "p", "POSITIVE", "+", "1", "positive", "POS", "pos",
    "neg", "0", "-1", "negative", "NEG", NA
  )
  polarities_parsed <- c(
    1L,1L,1L,1L,1L,1L,1L,1L,0L,0L,0L,0L,0L,NA_integer_
  )

  expect_equal(
    mzvault_read_polarity(polarities),
    polarities_parsed
  )
})
