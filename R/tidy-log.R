#' Tidy up a log created by futile.logger
#'
#' The messages you have written using [futile.logger] should have the
#' following format: "message: detail", e.g.
#' \code{flog.info("Rows in data: %s", nrow(data))}.
#' Or they can be a message without detail, e.g.
#' \code{flog.warn("Data file not found")}.
#'
#' Ensure that there is only one `": "` in the message, as this string is used
#' to split the columns. This function will separate the log into four columns:
#' * `message_type`
#' * `timestamp`
#' * `message`
#' * `detail`
#'
#' If more than one `": "` is used, everything after the first colon will end up
#' in the detail column. If you have not included `": "` in your log message,
#' the detail column will be populated with `NA`.
#'
#' @param log_path the file path where the futile logger log is saved
#' @param export_path the path to write the tidy log to; the default is the
#' current location of the log, i.e., will overwrite the current log
#' @param export logical; if `TRUE` (default) will export the tidied log as a csv
#' file, otherwise will return it to the console
#' @param delim the delimiter used in the futile logger log; the default, `"|"`,
#' should match any futile logger log, but the option has been given in case
#' futile.logger changes
#'
#' @importFrom readr read_delim write_csv
#' @importFrom dplyr %>% mutate
#' @importFrom tidyr separate
#' @importFrom stringr str_replace_all
#' @importFrom lubridate as_datetime
#'
#' @author Farm Business Survey team ([fbs.queries@defra.gov.uk](mailto:fbs.queries@defra.gov.uk))
#'
#' @export

tidy_log <- function(log_path, export_path = log_path, export = TRUE, delim = "|") {

  log <- read_delim(log_path, delim, col_names = c("message"), col_types = "c") %>%

    # Convert square brackets to colons so columns can be split in one step
    # (also separate uses regex to split, and square brackets behave weirdly)
    mutate(message = str_replace_all(message, "\\s\\[", ": ")) %>%
    mutate(message = str_replace_all(message, "\\]", ":")) %>%

    # Split the columns
    separate("message", c("message_type", "timestamp", "message", "detail"),
             sep = ": ", extra = "merge", fill = "right") %>%

    # Convert the timestamp column to type datetime
    mutate(timestamp = as_datetime(timestamp))

  # Write the tibble to a CSV file
  if (export == TRUE) {

    write_csv(log, export_path)

  } else {

    return(log)

  }
}
