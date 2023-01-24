

#' MsBackendMzVault
#'
#'
#'
#' @importClassesFrom Spectra MsBackend
#' @importClassesFrom RSQLite SQLiteConnection
setClass("MsBackendMzVault",
         contains = "MsBackend",
         slots = c(
           file = "character",
           con = "SQLiteConnection",
           filters = "list"
         ))

#' @importMethodsFrom Spectra backendInitialize
setMethod("backendInitialize",
          "MsBackendMzVault",
          function(object, file) {
            if(!fs::file_exists(file))
              stop("'file' needs to point to an mzVault library")
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

#' Report data storage to be the source SQLite database
#' @importMethodsFrom Spectra dataStorage
setMethod("dataStorage",
          "MsBackendMzVault",
          function(object) {
            rep(object@file, length(object))
          })

MsBackendMzVault <- function() {
  new("MsBackendMzVault")
}

setValidity("MsBackendMzVault",
            function(object) {
              DBI::dbIsValid(object@con)
              NULL
            })
