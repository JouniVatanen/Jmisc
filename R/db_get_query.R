#' db_get_query
#'
#' Get query results and close connection safely
#' @param x Choose string from which to calculate.
#' @param sql sql query or sql file
#' @param dsn if NULL then use password etc.
#' @param encoding database encoding
#' @param locale locale settings for sql file
#' @keywords DBI, odbc, database
#' @export
#' @importFrom odbc odbc
#' @import DBI

db_get_query <- function(
  sql, dsn = NULL, encoding = "utf-8", locale = locale()) {

  # Create connection
  con <- dbConnect(odbc::odbc(), dsn = dsn, encoding = encoding)

  # Disconnect on exit
  on.exit(dbDisconnect(con))

  # Read sql file
  sql <- sql %>%
    read_lines(locale = locale) %>%
    glue_collapse("\n")

  # Get data from EetuStage with parameters
  query <- dbGetQuery(con, sql)

  return(query)
}
