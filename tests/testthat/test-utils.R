

test_that("reading blob works", {
  data_parsed <- c(123.45,234.56,345.67)

  data_raw <- as.raw(c(
    0xcd, 0xcc, 0xcc, 0xcc, 0xcc, 0xdc, 0x5e, 0x40,
    0x52, 0xb8, 0x1e, 0x85, 0xeb, 0x51, 0x6d, 0x40,
    0x1f, 0x85, 0xeb, 0x51, 0xb8, 0x9a, 0x75, 0x40)
  )

  expect_equal(
    read_blob(data_raw),
    data_parsed
  )

})

test_that("writing blob works", {
  data_parsed <- c(123.45,234.56,345.67)

  data_raw <- as.raw(c(
    0xcd, 0xcc, 0xcc, 0xcc, 0xcc, 0xdc, 0x5e, 0x40,
    0x52, 0xb8, 0x1e, 0x85, 0xeb, 0x51, 0x6d, 0x40,
    0x1f, 0x85, 0xeb, 0x51, 0xb8, 0x9a, 0x75, 0x40)
  )

  expect_equal(
    write_blob(data_parsed),
    data_raw
  )
})


test_that("reading blobs works", {

  data_raw <- list(
    as.raw(c(
    0xcd, 0xcc, 0xcc, 0xcc, 0xcc, 0xdc, 0x5e, 0x40,
    0x52, 0xb8, 0x1e, 0x85, 0xeb, 0x51, 0x6d, 0x40)),
    as.raw(c(
      0xcd, 0xcc, 0xcc, 0xcc, 0xcc, 0xdc, 0x5e, 0x40,
      0x52, 0xb8, 0x1e, 0x85, 0xeb, 0x51, 0x6d, 0x40,
      0x1f, 0x85, 0xeb, 0x51, 0xb8, 0x9a, 0x75, 0x40)),
    as.raw(c(
      0x1f, 0x85, 0xeb, 0x51, 0xb8, 0x9a, 0x75, 0x40,
      0xcd, 0xcc, 0xcc, 0xcc, 0xcc, 0xdc, 0x5e, 0x40
    ))
  )

  data_parsed <- list(
    c(123.45,234.56),
    c(123.45,234.56,345.67),
    c(345.67,123.45)
  )

  expect_equal(
    read_blobs(data_raw),
    data_parsed
  )

  expect_equal(
    read_blobs_numericlist(data_raw),
    IRanges::NumericList(data_parsed, compress = FALSE)
  )

})

test_that("writing blobs works", {

  data_raw <- list(
    as.raw(c(
      0xcd, 0xcc, 0xcc, 0xcc, 0xcc, 0xdc, 0x5e, 0x40,
      0x52, 0xb8, 0x1e, 0x85, 0xeb, 0x51, 0x6d, 0x40)),
    as.raw(c(
      0xcd, 0xcc, 0xcc, 0xcc, 0xcc, 0xdc, 0x5e, 0x40,
      0x52, 0xb8, 0x1e, 0x85, 0xeb, 0x51, 0x6d, 0x40,
      0x1f, 0x85, 0xeb, 0x51, 0xb8, 0x9a, 0x75, 0x40)),
    as.raw(c(
      0x1f, 0x85, 0xeb, 0x51, 0xb8, 0x9a, 0x75, 0x40,
      0xcd, 0xcc, 0xcc, 0xcc, 0xcc, 0xdc, 0x5e, 0x40
    ))
  )

  data_parsed <- list(
    c(123.45,234.56),
    c(123.45,234.56,345.67),
    c(345.67,123.45)
  )

  expect_equal(
    write_blobs(data_parsed),
    data_raw
  )

  data_parsed_numericlist <- IRanges::NumericList(data_parsed)
  expect_equal(
    write_blobs(data_parsed_numericlist),
    data_raw
  )

})
