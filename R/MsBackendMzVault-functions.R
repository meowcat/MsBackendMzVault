

#' Process filter to predicates
#'
#' Converts the parameters set in a backend's filter
#' to dplyr predicates to be applied with reduce().
#'
#' @param filter  a list of filter statements to be processed
process_filter_to_predicate <- function(filter) {
  condition <- list()
  for(filter_name in names(filter)) {
    filter_data <- filter[[filter_name]]
    if(filter_name == "id") {
      condition$id <- function(.x, object) {
        id_filter <-
          dplyr::tibble(SpectrumId = filter_data) %>%
          dplyr::copy_to(object@con, ., temporary = TRUE, overwrite = TRUE)
        dplyr::left_join(
          id_filter,
          .x,
          by = "SpectrumId")
      }
    }
  }
  condition
}

#' Get SpectrumTable filtered by specified predicates
#'
#' @param object  a MsBackendMzVault object
#' @param filters  a list of filters to filter the table by. Where are the predicates documented?
get_filtered_spectrumtable <- function(object, filters = object@filters) {
  filters_ <- process_filter_to_predicate(filters)
  tbl_spectrum <- dplyr::tbl(object@con, "SpectrumTable")

  filters_ |> purrr::reduce(
    .f = function(tbl_in, filter_step)
      tbl_in %>% filter_step(object),
    .init = tbl_spectrum
  )
}


#' Extend SpectrumTable by CompoundTable data
#' 
#' @param tbl_spectrum  a SpectrumTables
#' @param object  a MsBackendMzVault object to load compound data from
join_compoundtable  <- function(tbl_spectrum, object) {
  tbl_compounds <- dplyr::tbl(object@con, "CompoundTable")
  tbl_joined <- tbl_spectrum |>
    dplyr::left_join(tbl_compounds, by = "CompoundId")
  # print(tbl_joined |> dplyr::show_query())
  tbl_joined
}


#' Get number of rows in filtered SpectrumTable
#' 
#' @param object  a MsBackendMzVault object
#'
get_filtered_spectrumtable_count <- function(object) {
  object |>
    get_filtered_spectrumtable() |>
    dplyr::count() |>
    dplyr::pull(n)
}


get_filtered_spectrumids <- function(object) {
  object |>
    get_filtered_spectrumtable() |>
    dplyr::pull("SpectrumId")
}

#' Map spectraVariables to data from mzVault
#' 
#' @param object  a MsBackendMzVault object
#'
#'
get_default_spectravariables_mapping <- function(object)
  list(
    msLevel = constant(2L),
    rtime = "RetentionTime",
    acquisitionNum = "SpectrumId",
    scanIndex = "ScanNumber",
    mz = list(
      col = "blobMass",
      read_fun = read_blobs
    ),
    intensity = list(
      col = "blobIntensity",
      read_fun = read_blobs
    ),
    dataStorage = constant(object@file),
    dataOrigin = constant(object@file),
    centroided = constant(TRUE),
    smoothed = constant(FALSE),
    polarity = list(
      col = "Polarity",
      read_fun = mzvault_read_polarity
      ),
    precScanNum = "CompoundId",
    precursorMz = "PrecursorMass",
    precursorIntensity = NA_real_,
    precursorCharge = NA_integer_,
    collisionEnergy = list(
      col = "CollisionEnergy",
      read_fun = mzvault_read_collisionenergy
    ),
    isolationWindowLowerMz = list(
      col = "PrecursorMass",
      read_fun = ~ .x - object@implicitIsolationWidth / 2),
    isolationWindowTargetMz = "PrecursorMass",
    isolationWindowUpperMz = list(
      col = "PrecursorMass",
      read_fun = ~.x + object@implicitIsolationWidth / 2
      )
  )

#' Load a spectraVariables mapping
#'
#' Loads a spectraVariables mapping definition and transforms shorthand notation
#' into full definition.
#'
#' @param object a MsBackendMzVault object
#' @param mapping a list of definitions to transform database fields to SpectraVariables
#' * the name is the spectraVariable to be assigned
#' * the value is a list with elements `list(col = "", fun = identity)`
#'   where `col` is the source column and `read_fun` is a function transforming
#'   the source data into the spectraVariable data.
#'   Up to now we abstain from:
#'   * backmapping for writing (TODO when writing is implemented)
#'   * complex non-1:1 mapping as done in SpectraMapping (a non-goal).
#' * If the value is a `character` instead of a `list`, it is interpreted as
#'   `list(col = value, read_fun = identity)`
#' * If the value is a `function` instead of a `list`, it is interpreted as
#'   `list(col = "acquistionNum", read_fun = value)`
#' * The `read_fun = constant(v)` represents `function(x) rep(v, length(x))`
#' * It is typically used as `value`, resulting in
#'  `list(col = acquisitionNum, read_fun = function(x) rep(v, length(x))`
#' * The value `NA` represents `constant(NA)`
load_spectravariables_mapping <- function(
    object,
    mapping = get_default_spectravariables_mapping(object)) {
  mapping_processed <-
    purrr::map(
      .x = mapping,
      .f = function(entry) {
        if(is.character(entry)) {
          return(list(
            col = entry,
            read_fun = identity
          ))
        } else if(is.function(entry)) {
          return(list(
            col = "SpectrumId",
            read_fun = entry
          ))
        } else if(purrr::is_formula(entry)) {
          return(list(
            col = "SpectrumId",
            read_fun = purrr::as_mapper(entry)
          ))
        } else if(is.list(entry)) {
          if(!("col" %in% names(entry))) {
            stop("A mapping definition needs to define the source column with `col`")
          }
          if(!("read_fun" %in% names(entry))) {
            stop("A mapping definition needs to define the transformation function with `read_fun`")
          }
          return(list(
            col = entry$col,
            read_fun = purrr::as_mapper(entry$read_fun)
          ))
        } else if(is.na(entry)) {
          return(list(
            col = "SpectrumId",
            read_fun = constant(entry) # note: not constant(NA) because we want
            # to preserve the type, e.g. NA_integer_, NA_real_
          ))
        }
      })
  return(mapping_processed)
}

#' Constant value for field mapping
#' 
#' @export
#' 
#' 
#' @param v a constant value to fill the field with
constant <- function(v) {
  function(.x) rep(v, length(.x))
}

#° Read collision energy from mzVault field
#'
#' Extract any number from the `CollisionEnergy` text field.
#' If multiple are found, they are averaged (e.g. for "Ramp 33.3-44-4 eV")
#' 
#' @param x a collisionEnergy field to parse
mzvault_read_collisionenergy <- function(x) {
  stringr::str_match_all(x, "([0-9.]+)") |>
    purrr::map(~ .x[,2]) |>
    purrr::map(as.numeric) |>
    purrr::map_dbl(~ mean(.x, na.rm=TRUE)) |>
    fast_replace_nan()
}

#' Read polarity from mzVault field
#' 
#' @param x a polarity field  
mzvault_read_polarity <- function(x) {
  positive_tags <- c(
    "P", "p", "POSITIVE", "+", "1", "positive", "POS", "pos"
  )
  purrr::map_lgl(
    .x = x,
    .f = ~ dplyr::if_else(
      is.na(.x),
      rlang::na_lgl,
      .x %in% positive_tags
    )
  ) |>
  as.integer()
}


#' Replace NaN
#'
#' @param data the data
#' @param replace the replacement, typically `NA`
#'
#' @return data with replacement
#' @export
#'
#' @examples
#'
#' x1 <- c(1,2,NaN,3,4)
#'
#' y1 <- fast_replace_nan(x1)
#'
#'
fast_replace_nan <- function(data, replace = NA) {
  stopifnot(length(replace) == 1)
  stopifnot(is.numeric(data))
  data[is.nan(data)] <- replace
  data
}

#' Get database connection
#'
#' @param object a `MsBackendMzVault` object
#' @importFrom DBI dbIsValid
get_db_con <- function(object) {
  if(dbIsValid(object@con))
    return(con)
  DBI::dbConnect(
    RSQLite::SQLite(),
    object@file)
}
