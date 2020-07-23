#' db_get_query
#'
#' Get query results and close connection safely

#' @param sql Sql query or sql file
#' @param drv Driver for database e.g. odbc()
#' @param ... Connection parameters like dns or server, username and password
#' also extra parameters like encoding.
#' @param params List of parameters to replace question marks in sql query.
#' Default NULL
#' @keywords DBI, odbc, database, dbGetQuery
#' @export
#' @importFrom odbc odbc
#' @importFrom fs path_ext
#' @importFrom readr read_lines
#' @importFrom glue glue_collapse
#' @import DBI

db_get_query <- function(
  sql, drv = odbc::odbc(), ..., params = NULL) {

  # Create connection
  con <- dbConnect(drv = drv, ...)

  # Disconnect on exit
  on.exit(dbDisconnect(con))

  # If sql is sql file, then parse the file to sql
  if (tolower(path_ext(sql)) == "sql") {
    sql <- glue_collapse(read_lines(sql), "\n")
  }

  # Return query with params, if defined
  if (!is.null(params)) {
    dbGetQuery(con, sql, params = params)
  } else {
    # Return query without params
    dbGetQuery(con, sql)
  }


}
