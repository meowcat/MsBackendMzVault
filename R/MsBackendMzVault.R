

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
           filters = "list",
           implicitIsolationWidth = "numeric",
           mapping = "list"
         ))

#' @param file the mzVault SQLite library to load
#' @param implicitIsolationWidth The assumed isolation width for precursor ions
#' @importMethodsFrom Spectra backendInitialize
setMethod("backendInitialize",
          "MsBackendMzVault",
          function(object,
                   file,
                   implicitIsolationWidth = 1) {
            if(!fs::file_exists(file))
              stop("'file' needs to point to an mzVault library")
            object@file <- file
            object@con <-
              DBI::dbConnect(
                RSQLite::SQLite(),
                file)
            object@implicitIsolationWidth <- implicitIsolationWidth
            object@mapping <- load_spectravariables_mapping(object)
            validObject(object)
            object
          })

setMethod("length",
          "MsBackendMzVault",
          function(x) {
            get_filtered_spectrumtable_count(x)
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


#' @importMethodsFrom Spectra spectraVariables
setMethod("spectraVariables",
          "MsBackendMzVault",
          function(object) {
            names(object@mapping)
          })

#' @importMethodsFrom Spectra spectraData
setMethod("spectraData",
          "MsBackendMzVault",
          function(object, columns = spectraVariables(object)) {
            # translate column mapping to SQL
            if(!all(columns %in% names(object@mapping)))
              stop("Not all requested columns are provided by this backend.")
            mapping <- object@mapping[columns]
            # Select mapped columns
            mapping_select <- mapping |>
              purrr::map("col")

            tbl_filt <- get_filtered_spectrumtable(object) |>
              join_compoundtable(object) |>
              dplyr::select(!!!mapping_select)

            # Apply specified read_fun to each column
            tbl_df <- tbl_filt |>
              dplyr::as_tibble() |>
              dplyr::mutate(
                dplyr::across(
                  names(mapping),
                  ~ mapping[[dplyr::cur_column()]]$read_fun(.x)
              )
            )

            # Convert to S4Vectors::DataFrame
            tbl_s4df <- tbl_df |> as("DataFrame")
            if("mz" %in% colnames(tbl_s4df))
              tbl_s4df$mz <- as(tbl_s4df$mz, "NumericList")
            if("intensity" %in% colnames(tbl_s4df))
              tbl_s4df$intensity <- as(tbl_s4df$intensity, "NumericList")
            tbl_s4df
          })


#' @importMethodsFrom Spectra peaksVariables
setMethod("peaksVariables",
          "MsBackendMzVault",
          function(object) c("mz", "intensity")
)


#' @importMethodsFrom Spectra peaksData
setMethod("peaksData",
          "MsBackendMzVault",
          function(object, columns = c("mz", "intensity")) {
            undef_cols <- setdiff(columns, peaksVariables(object))
            if(length(undef_cols) > 0)
              stop("Not all selected peaksVariables are available for MsBackendMzVault")
            spectraData(object, columns) |>
              as_tibble() |>
              purrr::pmap(
                .f = function(...)
                  matrix(c(...), ncol = ...length()) |>
                  magrittr::set_colnames(...names())
              )
          }
)


setMethod("$",
          "MsBackendMzVault",
          function(x, name) {
            spectraData(x, columns = name)[, 1L]
          })

setMethod(`[`,
          "MsBackendMzVault",
          function(x, i, j, ..., drop=FALSE) {
            if(!missing(j))
              stop("Parameter j not supported")
            if(...length() > 0)
              stop("Parameters ... not supported")
            if(drop)
              stop("Parameter drop not supported")
            i_ <- MsCoreUtils::i2index(i, length(x))
            # If the spectrum is already subsetted, then subset based on
            # the currently present SpectrumId
            # (not based on a subset of the currently subsetted SpectrumIds,
            # because further filters may have shifted this)
            # Note: this clears all filtering done "softly" via SQL
            current_ids <- get_filtered_spectrumids(x)
            x@filters <- list(
              id = current_ids[i]
            )
            x
          })


#' @importMethodsFrom Spectra lengths
setMethod("lengths",
          "MsBackendMzVault",
          function(x, use.names = FALSE) {
            if(use.names & !is.null(names(x)))
              stop("MsBackendMzVault does not support names")
            get_filtered_spectrumtable(x) |>
              dplyr::pull(blobMass) |>
              lengths_blob()
          })
