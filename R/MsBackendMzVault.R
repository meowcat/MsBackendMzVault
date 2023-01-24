
#' MsBackendMzVault
#'
#'
#'
setClass("MsBackendMzVault",
         contains = "MsBackend",
         slots = c(
           file = "character",
           con = "SQLiteConnection"
         ))


setMethod("backendInitialize",
          "MsBackendMzVault",
          function(object, file) {
            if(!fs::file_exists(file))
              stop("'file' needs to point to an MzVault library")
            object@file <- file
            object@con <-
              DBI::dbConnect(
                RSQLite::SQLite(),
                file)
            validObject(object)
            object
          })

setMethod("length",
          "MsBackendMzVault",
          function(x) {
            0
          })


MsBackendMzVault <- function() {
  new("MsBackendMzVault")
}

setValidity("MsBackendMzVault",
            function(object) {
              DBI::dbIsValid(object@con)
              NULL
            })
