
library(Spectra)
library(MsBackendMzVault)

#' Generate a backend with zeroed and nulled values for testing
be_with_zero_and_null <- function(be, to_zero = c(), to_null = c(),
                                  col = "blobMass") {
  # Copy to a memory DB and empty some spectra
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  RSQLite::sqliteCopyDatabase(be@con, con)

  DBI::dbExecute(
    con,
    glue::glue("UPDATE SpectrumTable SET {col} = NULL WHERE SpectrumId = ?"),
    params = list(to_null)
  )

  DBI::dbExecute(
    con,
    glue::glue("UPDATE SpectrumTable SET {col} = x'' WHERE SpectrumId = ?"),
    params = list(to_zero)
  )

  be@con <- con
  be
}
