

#' Process filter to predicates
#'
#' Converts the parameters set in a backend's filter
#' to dplyr predicates to be applied with reduce().
#'
process_filter_to_predicate <- function(filter) {
  condition <- list()
  for(filter_name in names(filter)) {
    filter_data <- filter[[filter_name]]
    if(filter_name == "id") {
      condition$id <- function(.x) dplyr::filter(.x, SpectrumId %in% filter_data)
    }
  }
  condition
}

#' Get SpectrumTable filtered by specified predicates
#'
get_filtered_spectrumtable <- function(object, filters = object@filters) {
  filters_ <- process_filter_to_predicate(filters)
  tbl_spectrum <- dplyr::tbl(object@con, "SpectrumTable")

  filters_ |> purrr::reduce(
    .f = function(tbl_in, filter_step)
      tbl_in %>% filter_step(),
    .init = tbl_spectrum
  )
}


#' Get number of rows in filtered SpectrumTable
#'
get_filtered_spectrumtable_count <- function(object) {
  object |>
    get_filtered_spectrumtable() |>
    dplyr::count() |>
    dplyr::pull(n)
}


#'
#'
#'
#' purrr::reduce(.init = tbl_spectrz)
#'
#'
#'
#'
#' sql_conditional_query <- function(query, where) {
#'   if(length(where$vars) == 0)
#'     return(query)
#'   glue::glue("{query} WHERE {where$condition}")
#' }
#'
#'
#' # where <- sql_build_where(list(id = c(3L,4L,5L)))
#' #
#' # q <- glue::glue("SELECT * FROM SpectrumTable WHERE {where$condition}")
#' # c <- DBI::dbSendQuery(db, q)
#' # dbBind(c, where$vars)
#' # dbFetch(c)
#'
#'
#' #'
#' #'
#' get_length <- function(object) {
#'   where <- sql_build_where(object@filter)
#'   query <- sql_conditional_query("SELECT COUNT(*) AS cnt FROM SpectrumTable", where)
#'   cat(deparse(where))
#'   result <- DBI::dbSendQuery(db, query)
#'   if(length(where$vars) > 0)
#'     dbBind(result, where$vars)
#'   res <- dbFetch(result)
#'   dbClearResult(result)
#'   res$cnt
#' }
#'
#' be <- backendInitialize(
#'   MsBackendMzVault(),
#'   file = "inst/data/tiny-massbank.db"
#'   )
#' get_length(be)
#'
#' be@filter <- list(id = c(2L,3L,4L))
#' get_length(be)
#'
#' sp <- Spectra("inst/data/tiny-massbank.db", source=MsBackendMzVault())
