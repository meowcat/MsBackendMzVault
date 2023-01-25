
#' Read a vector of doubles from raw bytes
#'
read_blob <- function(blob) {
  blob_con <- rawConnection(blob)
  data <- readBin(blob_con, "numeric", n = length(blob) / 4)
  close(blob_con)
  return(data)
}


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


#' Write vectors of doubles to a list of raw byte vectors
#'
write_blobs <- function(blobs) {
  purrr::map(.x = blobs, .f = write_blob)
}
