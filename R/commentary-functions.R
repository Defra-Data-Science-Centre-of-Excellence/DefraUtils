#' @title Get differences in text for statistics commentary
#'
#' @description
#' There are a six output options to choose from:
#' 1. Past tense, displays the percentage change as text as well as the
#' `curr_rnd` value, e.g. `"increased by 10% to 100"`
#' 2. Present tense, displays the percentage change as text as well as the
#' `curr_rnd` value, e.g. `"increasing by 10% to 100"`
#' 3. Reformats option 2 to show the `curr_rnd` value first, e.g. `"100, an increase of 10%"`
#' 4. Uses 'higher'/'lower' to show only the percentage change, e.g. `"10% higher"`;
#' no need to set the `curr_rnd` argument
#' 5. Uses 'rise'/'fall' to show only the percentage change, e.g. `"a rise of 10%"`;
#' no need to set the `curr_rnd` argument
#' 6. Uses 'rose'/'fell' to show only the percentage change, e.g. `"rose by 10%"`;
#' no need to set the `curr_rnd` argument
#'
#' @importFrom dplyr case_when
#' @importFrom stringr str_replace str_remove_all
#' @importFrom rlang parse_expr
#' @importFrom xfun numbers_to_words
#'
#' @param option_num Numeric; the text option to return
#' @param pc Column containing the proportional difference between the two
#' values you are comparing
#' @param pc_rnd The rounded values from the column defined in the `pc` argument
#' @param curr_rnd Column containing the rounded values for the most recent data
#' @param points Logical, default = `FALSE`; is the difference in percentage
#' points rather than %?
#'
#' @examples
#' get_diff_in_words(1, .1, "10%", curr_rnd = "100")
#' get_diff_in_words(2, .1, "10%", curr_rnd = "100")
#' get_diff_in_words(3, .1, "10%", curr_rnd = "100")
#' get_diff_in_words(4, .1, "10%")
#' get_diff_in_words(5, .1, "10%")
#' get_diff_in_words(6, .1, "10%")
#'
#' @family commentary functions
#'
#' @export
get_diff_in_words <- function(option_num, pc, pc_rnd, curr_rnd = NULL, points = FALSE) {

  curr_rnd <- str_replace(curr_rnd, "-", "a loss of ")

  # If the only numbers in pc_rnd are 0, but pc != 0 there was only a marginal change
  no_change <- function(pc_check, pc_rnd_check) {
    pc_check != 0 & gsub("(\\d)\\1+", "\\1", str_remove_all(pc_rnd_check, "\\D")) == "0"
  }

  option_1 <- case_when(
    pc > 0 & no_change(pc, pc_rnd)    ~ paste("marginally increased to", curr_rnd),
    pc < 0 & no_change(pc, pc_rnd)    ~ paste("marginally decreased to", curr_rnd),
    pc > 10  & !points &
      pc %% floor(pc) != 0            ~ paste0("increased more than ", floor(pc) + 1,
                                               "-fold to ", curr_rnd),
    pc > 10  & !points                ~ paste0("increased ", floor(pc) + 1, "-fold to ", curr_rnd),
    pc > 4   & !points &
      pc %% floor(pc) != 0            ~ paste0("increased more than ", numbers_to_words(floor(pc) + 1),
                                               "-fold to ", curr_rnd),
    pc >= 4  & !points                ~ paste0("increased ", numbers_to_words(floor(pc) + 1),
                                               "-fold to ", curr_rnd),
    pc == 3  & !points                ~ paste("quadrupled to", curr_rnd),
    pc > 3   & !points                ~ paste("more than quadrupled to", curr_rnd),
    pc == 3  & !points                ~ paste("quadrupled to", curr_rnd),
    pc > 2.7 & !points                ~ paste("almost quadrupled to", curr_rnd),
    pc > 2   & !points                ~ paste("more than tripled to", curr_rnd),
    pc == 2  & !points                ~ paste("tripled to", curr_rnd),
    pc > 1.7 & !points                ~ paste("almost tripled to", curr_rnd),
    pc > 1   & !points                ~ paste("more than doubled to", curr_rnd),
    pc == 1  & !points                ~ paste("doubled to", curr_rnd),
    pc > 0.9 & !points                ~ paste("almost doubled to", curr_rnd),
    pc == 0                           ~ paste("did not change from", curr_rnd),
    pc > 0                            ~ paste("increased by", pc_rnd, "to", curr_rnd),
    pc > -0.77 & pc < -0.73 & !points ~ paste("fell by three quarters to", curr_rnd),
    pc > -0.8  & pc < -0.7  & !points ~ paste("fell by around three quarters to", curr_rnd),
    pc > -0.52 & pc < -0.48 & !points ~ paste("halved to", curr_rnd),
    pc > -0.6  & pc < -0.4  & !points ~ paste("fell by around half to", curr_rnd),
    pc < 0                            ~ paste("decreased by", pc_rnd, "to", curr_rnd))

  option_2 <- str_replace(option_1, "ed ", "ing ") |>
    str_replace("did not change", "not changing") |>
    str_replace("fell", "falling")

  option_3 <- case_when(
    no_change(pc, pc_rnd) ~ paste0(curr_rnd, ", a negligible change"),
    pc >= 1 & !points     ~ paste0(curr_rnd, ", a considerable increase"),
    pc > 0                ~ paste0(curr_rnd, ", an increase of ", pc_rnd),
    pc == 0               ~ paste0(curr_rnd, ", no change"),
    pc < 0                ~ paste0(curr_rnd, ", a decrease of ", pc_rnd)
  )

  option_4 <- case_when(
    pc > 0 & no_change(pc, pc_rnd)          ~ "marginally higher",
    pc < 0 & no_change(pc, pc_rnd)          ~ "marginally lower",
    pc >= 4 & !points                       ~ "considerably higher",
    pc >= 1 & str_detect(option_1, "led")   ~ str_replace(option_1, "led .*", "le"),
    pc > 0                                  ~ paste(pc_rnd, "higher"),
    pc == 0                                 ~ "the same as",
    pc < 0 & str_detect(option_1, "halved") ~ "half",
    pc < 0                                  ~ paste(pc_rnd, "lower"),
    TRUE                                    ~ ""
  )

  option_5 <- case_when(
    no_change(pc, pc_rnd) ~ "a negligible change",
    pc >= 1 & !points     ~ "a considerable rise",
    pc > 0                ~ paste("a rise of", pc_rnd),
    pc == 0               ~ "no change",
    pc < 0                ~ paste("a fall of", pc_rnd)
  )

  option_6 <- case_when(
    pc > 0 & no_change(pc, pc_rnd)          ~ "marginally rose",
    pc < 0 & no_change(pc, pc_rnd)          ~ "marginally fell",
    pc >= 4 & !points                       ~ "considerably rose",
    pc >= 1 & str_detect(option_1, "led")   ~ str_replace(option_1, "led .*", "led"),
    pc > 0                                  ~ paste("rose by", pc_rnd),
    pc == 0                                 ~ "did not change",
    pc < 0 & str_detect(option_1, "halved") ~ "halved",
    pc < 0                                  ~ paste("fell by", pc_rnd),
    TRUE                                    ~ ""
  )

  word_diff <- get(paste0("option_", option_num))

  return(word_diff)

}


#' @title Get percentage in text
#'
#' @description
#' Converts percentages to text, e.g. `0.25` will be converted to `"a quarter"`
#'
#' @importFrom dplyr case_when
#'
#' @param pc Numeric; The percentage as a proportion, e.g. `0.1` for a value of 10%
#' @param pc_rnd String; What to show if `pc` isn't converted to text, e.g. `"10%"`
#'
#' @family commentary functions
#'
#' @export
get_pc_in_words <- function(pc, pc_rnd) {

  case_when(pc >= 0.995 & pc <= 1     ~ "all",
            pc >= 0.97  & pc <  0.995 ~ "almost all",
            pc >= 0.745 & pc <  0.755 ~ "three quarters",
            pc >= 0.73  & pc <= 0.77  ~ "around three quarters",
            pc >= 0.655 & pc <  0.665 ~ "two thirds",
            pc >= 0.64  & pc <= 0.68  ~ "around two thirds",
            pc >  0.505 & pc <= 0.54  ~ "just over half",
            pc >= 0.495 & pc <  0.505 ~ "half",
            pc >= 0.46  & pc <  0.495 ~ "just under half",
            pc >= 0.325 & pc <  0.335 ~ "a third",
            pc >= 0.31  & pc <= 0.35  ~ "around a third",
            pc >= 0.245 & pc <  0.255 ~ "a quarter",
            pc >= 0.23  & pc <= 0.27  ~ "around a quarter",
            pc >= 0.195 & pc <  0.205 ~ "a fifth",
            pc >= 0.18  & pc <= 0.22  ~ "around a fifth",
            TRUE ~ pc_rnd)

}


#' @title Compare two year's worth of data
#'
#' @description
#' Data must contain the following four columns: survey_year, grouping_factor,
#' group, value
#'
#' @importFrom dplyr filter group_by mutate select any_of case_when rename_with
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_wider
#' @importFrom readr parse_number
#' @importFrom rlang sym
#'
#' @param df A data frame
#' @param year_1 First year to include in the comparison
#' @param year_2 Second year to include in the comparison
#' @param year_col Default = `"survey_year"`; The column with the years in
#' @param grouping_col Default = `NULL`; The column with the grouping variables
#' in - only required if you want this column returned
#' @param group_col Default = `NULL`; The column with the group variables in -
#' only required if you want this column returned
#' @param value_col The column with the values in
#' @param group_levels Default = `NULL`; If you have a 'group' column that you are
#' using in the commentary, you may want to edit the levels of this column so that
#' they read better in text, e.g. "LFA grazing livestock" rather than "Grazing
#' livestock (LFA)"; to do this, set the current levels of the group column here,
#' and the labels you want with `group_labels`
#' @param group_labels Default = `group_levels`; The new level labels for the group column
#' @param price_type Default = `NULL`; If the data contains both real and
#' current prices, which should be included? To include both keep as `NULL`.
#' This requires the data to contain a column called 'prices'.
#' @param extra_vars String; Extra variable(s) to select, e.g. band
#' @param diff String; Either `"percent"` (default; for difference in %) or
#' `"points"` (for difference in percentage points)
#' @param round_to passed to [fbs_round()]
#' @param prefix Default = `""`; If you are calculating percentage change, you
#' can add a prefix to the returned values, e.g. `"£"`
#'
#' @family commentary functions
#'
#' @export
compare_two_years <- function(df, year_1, year_2, year_col = "survey_year",
                              grouping_col = "grouping_factor", group_col = "group",
                              group_levels = NULL, group_labels = group_levels,
                              value_col = "value", price_type = NULL, extra_vars = NULL,
                              diff = "percent", round_to = NULL, prefix = "") {

  # Where current and real prices are present, filter for just one of these
  if (!is.null(price_type) & "prices" %in% colnames(df)) {
    df <- filter(df, prices == price_type)
  }

  # For FBS data: where multiple typologies are present for a year, choose the
  # rows with the later typology in each year
  if ("typology" %in% colnames(df)) {
    df <- group_by(df, !!sym(year_col)) %>%
      filter(typology == max(typology))
  }

  # If required, relevel the group factor
  tidy_df <- if (!is.null(group_levels)) {
    mutate(df,
           group_text = factor(!! sym(group_col), levels = group_levels,
                               labels = group_labels))
  } else {
    df
  }

  # Filter for specified years, select only required columns, and calculate difference
  get_diff <- tidy_df %>%
    filter(!!sym(year_col) %in% c(year_1, year_2)) %>%
    select(any_of(c(year_col, grouping_col, group_col, "group_text", extra_vars, value_col))) %>%
    pivot_wider(names_from = !!sym(year_col), values_from = all_of(value_col)) %>%
    rename_with(~c("prev", "curr"), all_of(as.character(c(year_1, year_2)))) %>%
    mutate(diff = curr - prev)

  # Add rounded figures for use in commentary
  round_diff <- if (diff == "percent") {

    round_prefix <- function(value, round_to, scale = "none") {
      divide_by <- case_when(scale == "t" ~ 1e3,
                             scale == "m" ~ 1e6,
                             TRUE ~ 1)
      suffix <- case_when(scale == "t" ~ " thousand",
                          scale == "m" ~ " million",
                          TRUE ~ "")
      paste0(prefix, str_remove(fbs_round(value / divide_by, round_to = round_to),
                                "\\.0$"), suffix)
    }

    # have to use rowwise otherwise the if_else doesn't work
    rowwise(get_diff) %>%
      mutate(prev_rnd    = round_prefix(prev, round_to),
             prev_rnd_t  = round_prefix(prev, if_else(prev < 100e3, 0.1, 1), "t"),
             prev_rnd_m  = round_prefix(prev, 0.1, "m"),
             curr_rnd    = round_prefix(curr, round_to),
             curr_rnd_t  = round_prefix(curr, if_else(prev < 100e3, 0.1, 1), "t"),
             curr_rnd_m  = round_prefix(curr, 0.1, "m"),
             diff_pc     = diff / prev,
             diff_pc_rnd = paste0(str_remove(fbs_round(abs(diff_pc) * 100, 1), "\\.0+$"), "%")) %>%
      ungroup()

  } else if (diff == "points") {

    round_val <- if (is.null(round_to)) 1 else round_to

    mutate(get_diff,
           prev_rnd = paste0(fbs_round(prev * 100, round_val), "%"),
           curr_rnd = paste0(fbs_round(curr * 100, round_val), "%"),
           diff_rnd = str_remove(fbs_round(abs(diff) * 100, round_val), "\\.0+$")) %>%
      mutate(diff_rnd = paste(diff_rnd, if_else(parse_number(diff_rnd) == 1,
                                                "percentage point", "percentage points")))


  }

  return(round_diff)

}


#' @title Create a list to insert into markdown text
#'
#' @description Takes a vector of strings and collapses them into a list; the
#' last item is connected to the list with 'and', the rest of the items are
#' separated by commas
#'
#' @importFrom stringr str_to_lower str_to_upper str_to_sentence str_to_title
#'
#' @param string_vector A string vector
#' @param oxford_comma Logical (default = `FALSE`); should the last item be
#' connected using an Oxford comma?
#' @param descriptor A descriptor to add to the end of the list; if supplied,
#' provide an object of length 1 (if always should be the same word), 2 (if you
#' just require singular and plural options - in that order), or 3 (if you
#' require options for one, two or more than two items - in that order)
#' @param last_connector What word should connect the last two items? Default = `"and"`
#' @param case Option to change the case of the output using [stringr::case()]
#'
#' @examples
#' create_list(string_vector = c("apple", "banana", "pear"), descriptor = c("fruit", "fruits"))
#' create_list(string_vector = c("apple", "banana"), descriptor = c("fruit", "fruits"))
#' create_list(string_vector = c("apple", "banana", "pear"), oxford_comma = T, descriptor = c("fruit", "fruits"))
#'
#' @family commentary functions
#'
#' @export
create_list <- function(string_vector, oxford_comma = FALSE,
                        descriptor = NULL, last_connector = "and",
                        case = c("original", "lower", "upper", "sentence", "title")) {

  case <- rlang::arg_match(case)

  last_item <- string_vector[length(string_vector)]
  other_items <- paste(string_vector[1:(length(string_vector) - 1)], collapse = ", ")

  last_connector <- if_else(oxford_comma, paste0(", ", last_connector, " "),
                            paste0(" ", last_connector, " "))

  descriptor <- if (is.null(descriptor)) { ""
  } else if (length(descriptor) == 1) {
    descriptor
  } else if (length(descriptor) == 2) {
    if_else(length(string_vector) == 1, descriptor[1], descriptor[2])
  } else if (length(descriptor) == 3) {
    if_else(length(string_vector) %in% 1:2, descriptor[length(string_vector)], descriptor[3])
  }

  item_list <- if_else(length(string_vector) == 1, last_item,
                       paste0(other_items, last_connector, last_item))

  final_string <- if_else(descriptor == "", item_list, paste(item_list, descriptor))

  if (case != "original") {
    final_string <- eval(rlang::parse_expr(paste0("str_to_", case, "(final_string)")))
  }

  return(final_string)

}


#' @title Get full URL for file attachments or links on gov.uk
#'
#' @param web_address Webpage to look for the file on
#' @param file_type File extension to search for (e.g. "xlsx", "ods"); default
#' = `NULL`, leave as NULL if searching for a link rather than a file
#' @param file_number If there are multiple files of the same type as
#' `file_type` on the page, which one to return
#' @param search_term Regex; Optional, used by `str_subset` to filter on link URLs
#' @param find_csv_preview Logical, default = `FALSE`; when searching for a CSV
#' on gov.uk, this will return the link to the CSV preview when set to TRUE, and
#' will return the link to the actual CSV when set to FALSE
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr
#' @importFrom stringr str_subset
#'
#' @family commentary functions
#'
#' @export
get_address <- function(web_address, file_type = NULL, file_number = 1,
                        search_term = NULL, find_csv_preview = F) {

  tmp <- tempfile()

  download.file(web_address, destfile = tmp, quiet = TRUE)

  page <- xml2::read_html(tmp)

  files <- page %>%
    # find all links
    rvest::html_nodes("a") %>%
    # get the url
    rvest::html_attr("href") %>%
    # if provided, subset for relevant file types
    { if (!is.null(file_type)) str_subset(., paste0("\\.", tolower(file_type), "$")) else . } %>%
    unique()

  # When searching for a CSV on gov.uk, CSV previews will also be returned
  # This is an issue if you are looking for, say, the second CSV file on the page,
  # but the second CSV link would be the preview of the first CSV file
  # This will either return only the CSV previews or remove the CSV preview links
  if (find_csv_preview) {
    files <- str_subset(files, "csv-preview")
  } else {
    files <- str_subset(files, "csv-preview", negate = T)
  }

  file <- if (!is.null(search_term)) {
    # filter for files with the search term
    str_subset(files, search_term)[[file_number]]
  } else {
    files[[file_number]]
  }

  return(file)

}
