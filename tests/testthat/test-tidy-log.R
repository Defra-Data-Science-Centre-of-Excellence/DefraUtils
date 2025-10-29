library(futile.logger)

# Set the threshold to debug so that all messages get added to the log
flog.threshold(TRACE)

# Set the log to append to a file
log_file <- tempfile()
flog.appender(appender.file(log_file))

# Create some log messages
flog.trace("Started testing mtcars data")
flog.debug("mtcars data found: %s", exists("mtcars"))
flog.info("Number of rows in cars: %s", nrow(mtcars))
flog.warn("Number of cars with mpg > 30: %s", nrow(dplyr::filter(mtcars, mpg > 30)))
flog.error("Number of cars with mpg > 50: %s", nrow(dplyr::filter(mtcars, mpg > 50)))
flog.fatal("Car name column found: %s", "car" %in% colnames(mtcars))

test_that("format before tidy_log is as expected", {
  unformatted <- read.csv(log_file)
  expect_equal(ncol(unformatted), 1)
  expect_true(all(stringr::str_detect(
    unformatted[,1],
    "^[A-Z]+ \\[\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\] .+")))
})

test_that("tidy_log return expected output", {
  formatted <- tidy_log(log_file, export = FALSE)
  expect_equal(sapply(colnames(formatted), \(x) class(formatted[[x]])[1]),
               c(message_type = "character", timestamp = "POSIXct",
                 message = "character", detail = "character"))
  expect_equal(formatted$message_type,
               c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"))
})

test_that("tidy_log saves output as expected", {
  new_file <- tempfile()
  on.exit(unlink(new_file))
  expect_false(file.exists(new_file))
  tidy_log(log_file, export_path = new_file)
  expect_true(file.exists(new_file))
})

unlink(log_file)
