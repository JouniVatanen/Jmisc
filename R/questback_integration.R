#' qb_get_responses
#'
#' Get responses to a survey from Questback Essentials.
#' @param quest_id Quest id from Questback Essentials.
#' @param sid Quest security id from Questback Essentials.
#' @param filename Filename to save the imported data.
#' @param username Username for Questback Essentials.
#' @param password Password for Questback Essentials.
#' @param last_updated If time is less than chosen, then do not execute.
#' Possible time choises are secs, mins, hours, days and weeks. Default is 12 hours.
#' @param latest_days Not yet implemented, because IntegrationUtility demands
#' dates to be in form YYYY-MM-DD.
#' @keywords questback essentials, survey
#' @export
#' @importFrom fs path_abs
#' @importFrom readr write_excel_csv2

# Get responses from Questback Essentials using IntegrationUtility.exe
qb_get_responses <- function(
  quest_id, sid, filename, username, password, last_updated = "12 hours",
  latest_days = NULL) {

  # Ensure that IntegrationUtility.exe is found
  if (Sys.which("IntegrationUtility") == "") {
    stop("IntegrationUtility.exe is not found. Add it to your PATH variable.")
  }

  # Split last_updated to time unit and amount
  time_amount <- as.numeric(strsplit(last_updated, " ")[[1]][1])
  time_unit <- strsplit(last_updated, " ")[[1]][2]

  # Stop if not
  stopifnot(
    is.null(latest_days),
    time_unit %in% c("secs", "mins", "hours", "days", "weeks"),
    is.numeric(time_amount))

  # Calculate when the file was last updated
  file_updated <- difftime(Sys.time(), file.info(filename)$mtime, units = time_unit)

  if (!file.exists(filename) | (file_updated > time_amount)) {
    # Create cmd
    cmd <- sprintf(
      paste(
        "IntegrationUtility.exe",
        "-Command:ImportResponses",
        "-ExportFile:%s",
        "-ExportFormat:csv",
        "-ExportDelimiter:tab",
        "-IncludeEmailOrPhone:true",
        "-DataFormat:text",
        "-UserName:%s",
        "-Password:%s",
        "-QuestId:%s",
        "-SecurityLock:%s"),
        # FIXME: Dates works only wih American style system settings e.g. 2020-01-30
        #"-FromNDaysAgo:20"),
      path_abs(filename), username, password, quest_id, sid)

    # Activate the command
    shell(cmd)
  }
}

#' qb_send_invitees
#'
#' Send invitees to a survey from Questback Essentials.
#' @param data Data to import the invitees from.
#' @param quest_id Quest id from Questback Essentials.
#' @param sid Quest security id from Questback Essentials.
#' @param username Username for Questback Essentials.
#' @param password Password for Questback Essentials.
#' @param filename Filename to save the data temporarely.
#' @param transfer_dir Directory where to saave the imported data.
#' @param email Name of the email field.
#' @param allow_duplicate Allow to import duplicate email fields.
#' @keywords questback essentials, survey
#' @export
#' @importFrom fs path_abs
#' @importFrom readr write_excel_csv2

# Send invitees to Questback Essentials using IntegrationUtility.exe
qb_send_invitees <- function(
  data, quest_id, sid, username, password, filename = tempfile(),
   transfer_dir = NULL, email = "email", allow_duplicate = FALSE) {

  # Ensure that IntegrationUtility.exe is found
  if (Sys.which("IntegrationUtility") == "") {
    stop("IntegrationUtility.exe is not found. Add it to your PATH variable.")
  }

  # Allow duplicate or not
  allow_duplicate <- ifelse(allow_duplicate, "true", "false")

  # Write to temp file
  write_excel_csv2(data, filename, na = "")

  if (file.exists(filename)) {
    # Create cmd
    cmd <- sprintf(
      paste(
        "IntegrationUtility.exe",
        "-Command:ImportInvitees",
        "-ImportFile:%s",
        "-UserName:%s",
        "-Password:%s",
        "-QuestId:%s",
        "-SecurityLock:%s",
        "-TransferDirectory:%s",
        "-ImportDelimiter:semicolon",
        "-Email:%s",
        "-AllowDuplicates:%s",
        "-SendEmail:true"),
      path_abs(filename), username, password, quest_id,
      sid, path_abs(transfer_dir), email, allow_duplicate)

    # Activate the command
    shell(cmd)
  }
}
