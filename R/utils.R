
#' Read a vector of doubles from raw bytes
#'
read_blob <- function(blob) {
  blob_con <- rawConnection(blob)
  data <- readBin(blob_con, "numeric", n = length(blob) / 8)
  close(blob_con)
  return(data)
}


#' Get length (in decoded elements) for a blob
#'
length_blob <- function(blob) length(blob) / 8


#' Get lengths (in decoded elements) for a list of blobs
#'
lengths_blob <- function(blobs) purrr::map_int(blobs, length_blob)

#' Write a vector of doubles to raw bytes
#'
write_blob <- function(data) {
  stopifnot(is.numeric(data))
  blob_con <- rawConnection(raw(0), "r+")
  writeBin(data, blob_con)
  blob <- rawConnectionValue(blob_con)
  close(blob_con)
  return(blob)
}


#' Read vectors of doubles from a list of raw byte vectors
#'
read_blobs <- function(blobs) {
  purrr::map(.x = blobs, .f = read_blob)
}

#' Read a NumericList from a list fo raw byte vectors
read_blobs_numericlist <- function(blobs, compress = FALSE) {
  purrr::map(.x = blobs, .f = read_blob) |>
    IRanges::NumericList(compress = compress)
}


#' Write vectors of doubles to a list of raw byte vectors
#'
write_blobs <- function(blobs) {
  purrr::map(.x = blobs, .f = write_blob)
}
