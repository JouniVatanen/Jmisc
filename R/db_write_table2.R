#' db_write_table2
#'
#' Write table to database
#' @param data Data.
#' @param con Connection string.
#' @param catalog Catalog name.
#' @param schema Scema name.
#' @param table Table name.
#' @param fields Named vector of column names and datatypes. If null, then
#' automatic fetch by DBI::dbDataType.
#' @param to_nvarchar Convert varchar datatype to nvarchar for easier encoding.
#' @param overwrite Overwrite table.
#' @param append Append table.
#' @param unique_cols Columns, which makes appended data unique. You can use
#' tidyselect styled helpers like starts_with(). Default: everything().
#' @param to_utf16 Convert to UTF-16LE, which is required for proper Microsoft
#' SQL Server encoding.
#' @keywords write, sql, database
#' @export
#' @importFrom DBI dbDataType Id dbExistsTable dbRemoveTable dbCreateTable dbAppendTable
#' @importFrom stringi stri_encode
#' @importFrom dplyr mutate_if distinct
#' @importFrom data.table rbindlist
#' @importFrom rlang enquo
#' @import tidyselect

# Write table to database
db_write_table2 <- function(
  data, con, catalog, schema, table, fields = NULL, to_nvarchar = TRUE,
  overwrite = TRUE,  append = !overwrite, unique_cols = everything(),
  to_utf16 = TRUE, temporary = FALSE, logging = TRUE) {


  # Stop if fields is not a named character vector or null
  if (!is_named_vector(fields, "character") & !is.null(fields)) {
    stop("Fields is not a named character vector or null.")
  }

  # Define table catalog and schema
  table_id <- Id(
    catalog = catalog,
    schema = schema,
    table = table)

  # Fetch old data from database, if append is TRUE
  if (append) {

    # Fetch old data
    data_old <- dbGetQuery(
      con, paste0("SELECT * FROM ", paste(catalog, schema, table, sep = ".")))

    # Unite dataframes
    data_stacked <- rbindlist(list(data_old, data), fill = TRUE)

    # Choose distinct rows
    # Return unique rows comparing selected variables if null, then compare everyhing
    data <- distinct_at(data_stacked, vars(!!!enquo(unique_cols)), .keep_all = TRUE)
  }

  # Replace all varchar with nvarchar, if fields is missing
  if (to_nvarchar) {
    # FIXME: dbDataType gives warning with NA charvar datatypes
    fields_auto <- gsub("^varchar", "nvarchar", dbDataType(con, data))
  } else {
    fields_auto <- dbDataType(con, data)
  }

  # Replace some datatypes, if fields is not null
  if (!is.null(fields)) {
    # Replace fields_auto values by fields values, where names match
    fields_auto[names(fields)] <- fields[!is.na(fields[names(fields_auto)])]
  }

  # Convert character data to UTF-16LE
  #https://stackoverflow.com/questions/48105277/writing-unicode-from-r-to-sql-server
  data <- data %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, ~stri_encode(., to = "UTF-16LE", to_raw = TRUE))

  # Remove table if it exists
  if (dbExistsTable(con, table_id)) {
    dbRemoveTable(con, table_id)
  }

  # create table
  dbCreateTable(con, table_id, fields_auto)

  # Append to table
  dbAppendTable(con, table_id, data)

  # Check that all the rows were written and log number of rows
  if (logging) {
    # Count rows in the database
    db_count_rows <- dbGetQuery(
      con,
      paste0("SELECT count(*) FROM ", paste(catalog, schema, table, sep = ".")))

    # Check if all the rows were loaded to database
    if (db_count_rows != nrow(data)) {
      stop("The data was not loaded to the database!")
    } else {
      cat(paste0("Uploaded ", db_count_rows, " rows to the database."))
    }
  }

}
