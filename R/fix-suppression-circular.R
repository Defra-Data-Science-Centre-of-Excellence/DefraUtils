#' Fix suppression in tables where totals can be used to calculate suppressed values in multiple dimensions
#'
#' This function finds which figures need to be secondarily suppress and
#' overwrites their sample size to zero - any subsequent suppression code will
#' then suppress these figures.
#'
#' This uses [fix_suppression()] but works in multiple dimensions, looping until
#' there are no more figures which need to be suppressed.
#'
#' Sometimes this function can oversuppress, therefore, it also outputs an excel
#' file for manual checking. This file has the unsuppressed data in twice so that
#' can manually suppress one while comparing it to the unsuppressed data. The file
#' also has conditional formatting - values between 1 and 4 are highlighted in red
#' and values between 5 and 14 are highlighted in yellow.
#'
#' @importFrom dplyr %>% mutate group_by summarise bind_rows distinct left_join filter between full_join rename all_of across
#' @importFrom openxlsx createWorkbook writeDataTable conditionalFormatting createStyle writeData saveWorkbook addWorksheet
#'
#' @param df Data to fix
#' @param groups A list of grouping dimensions, e.g. `list("group", c("band", "grouping_factor"))`
#' would group by the `group` column in the first dimension and by the `band` and
#' `grouping_factor` columns in the second dimension
#' @param sample_size_col Default = `"sample_size"`; The name of the sample size column
#' @param export_path File path; where the checking files should be saved
#' @param file_name What should the checking file be called? Don't include file extension
#' @param pivot_col String; If you want the manual checking table to be pivoted, which
#' column should it be pivoted on?
#' @param save_excel_file, default is TRUE, set to FALSE when one does not wish to save an excel file
#'
#' @seealso [fix_suppression()]
#'
#' @export

fix_suppression_circular <- function(df, 
                                     groups,
                                     sample_size_col = "sample_size", 
                                     export_path,
                                     file_name,
                                     pivot_col = NULL,
                                     save_excel_file = TRUE) {

  groups_list <- if (is.list(groups)) {
    set_names(groups, paste0("grouping_", 1:length(groups)))
  } else {
    list(grouping_1 = groups)
  }

  # Check if any groups need extra suppression
  check_suppression <- function(g, d) {

    initial <- mutate(d, suppress = dplyr::between(!! sym(sample_size_col), 1, 4))
    
    # total pop of group, this is so that we can later determine if
    # all the pop of a group is suppressed and also checks to see if there are any groups
    # that are represented by a single data point
    total_pop <- group_by(initial, across(all_of(g))) %>%
      summarise(total_p = sum(sample_size_og), row_n = n(), .groups = "drop")
    

    # Get the number of suppressed values in each group
    count_suppressed <- group_by(initial, across(all_of(g))) %>%
      summarise(n = sum(suppress), .groups = "drop") %>%
      mutate(supp_type = "n_obs")

    # Get the total of the suppressed values in each group (e.g. the sample size)
    total_suppressed <- group_by(initial, across(all_of(g))) %>%
      filter(suppress) %>%
      summarise(n = sum(sample_size_og), .groups = "drop") %>%
      mutate(supp_type = "total")

    # Get the total number of rows (with samples) in each group
    # This allows us to know when we have suppressed the entire group
    total_in_group <- group_by(initial, across(all_of(g))) %>%
      mutate(zeros = !! sym(sample_size_col) == 0) %>%
      summarise(zeros = sum(zeros), total = n(), .groups = "drop")

    # if one one_unsuppressed_and_rest_zeros = TRUE, suppress a zero !!!

    dplyr::full_join(count_suppressed, total_in_group, by = g) %>%
      mutate(one_suppressed = n == 1, # only one number suppressed therefore another needs suppression
             unsuppressed = total - n, # just counts the number of unsuppressed 0's in a set
             one_unsuppressed_and_rest_zeros = zeros > 1 &
               unsuppressed == 1 &
               zeros + unsuppressed == total,
             to_fix_1 = one_suppressed,
             to_fix_2 = one_unsuppressed_and_rest_zeros) %>%
      filter(to_fix_1 | to_fix_2) %>%
      select(colnames(total_suppressed)) %>%
      bind_rows(filter(total_suppressed, (n < 5 ))
      ) %>%
      left_join(total_pop, by = g) %>%
      filter(row_n != 1) %>%
      filter(total_p != n) %>%
      select(-c(row_n,total_p)) %>%
      distinct(across(1:(ncol(.) - 2)), .keep_all = T)
    
  }

  suppressed_data <- mutate(df, sample_size_og = !! sym(sample_size_col))
  to_fix <- lapply(groups_list, check_suppression, suppressed_data)

  # If there are any groups that need extra suppression, start the loop
  loop <- 0

  # Keep the loop going until no more suppression is required
  while (any(lapply(to_fix, nrow) > 0)) {

    loop <- loop + 1
    print(paste("Suppression loop", loop))

    # Do extra suppression for each group
    for (i in 1:length(groups_list)) {

      if (nrow(to_fix[[i]]) > 0) {

        fix_col <- left_join(to_fix[[i]], suppressed_data, by = groups_list[[i]]) %>%
          group_by(across(all_of(groups_list[[i]]))) %>%
          # We want to show true zeroes unless only one is suppressed and rest are zero
          # If that is the case, suppress an adjacent zero
          mutate(zero = sum(!! sym(sample_size_col) == 0),
                 total = n(),
                 zero_fix_lag = sample_size_og == 0 & total - zero == n & lag(sample_size_og) > 0,
                 zero_fix_lead = sample_size_og == 0 & total - zero == n & lead(sample_size_og) > 0) %>%
          rowwise() %>%
          mutate(zero_fix_lead_check = isTRUE(zero_fix_lead) & sum(zero_fix_lag + zero_fix_lead, na.rm = T) > 1,
                 zero_fix = zero_fix_lag | zero_fix_lead_check) %>%
          group_by(across(all_of(groups_list[[i]]))) %>%
          arrange(!! sym(sample_size_col), .by_group = TRUE) %>%
          filter(!! sym(sample_size_col) != 0 | zero_fix) %>%
          select(-starts_with("zero")) %>%
          mutate(sample_size_new = if_else(
            # If it's the fewest or next fewest number of farms, return 1
            lag(!! sym(sample_size_col)) < 5 | is.na(lag(!! sym(sample_size_col))), 1,
            # Otherwise return the original value
            !! sym(sample_size_col))) %>%
          ungroup() %>%
          select(-c(n, supp_type))

        # Join on the new sample size column and replace the original
        # values where relevant
        suppressed_data <- suppressed_data %>%
          left_join(fix_col, by = colnames(suppressed_data)) %>%
          mutate(sample_size_1 = if_else(is.na(sample_size_new),
                                         !! sym(sample_size_col),
                                         sample_size_new)) %>%
          select(-all_of(c(sample_size_col, "sample_size_new"))) %>%
          dplyr::rename({{sample_size_col}} := sample_size_1) %>%
          select(all_of(colnames(suppressed_data)))

        rm(fix_col)

      }

      # Check if any groups that *still* need suppression#
      to_fix <- lapply(groups_list, check_suppression, suppressed_data)

    }

  }

  # Create excel file for manual checking
  
  if (save_excel_file == TRUE) {
  unsuppressed_df <- select(df, all_of(c(unname(unlist(groups_list)), sample_size_col))) %>%
    { if (!is.null(pivot_col)) pivot_wider(., names_from = all_of(pivot_col),
                                           values_from = all_of(sample_size_col)) else . }

  suppressed_df <- select(suppressed_data, -sample_size_og) %>%
    select(all_of(c(unname(unlist(groups_list)), sample_size_col))) %>%
    { if (!is.null(pivot_col)) pivot_wider(., names_from = all_of(pivot_col),
                                           values_from = all_of(sample_size_col)) else . }

  wb <- createWorkbook()
  addWorksheet(wb = wb, sheetName = "Sheet1")

  # Write first unsuppressed table - to compare to
  writeData(wb = wb, sheet = 1, x = "Unsuppressed table: leave this table as-is, use it for comparison")
  writeDataTable(wb = wb, sheet = 1, x = unsuppressed_df, startRow = 3,
                 tableStyle = "none", withFilter = FALSE, bandedRows = FALSE)

  # Write second unsuppressed table - to compare to
  writeData(wb = wb, sheet = 1, startCol = ncol(unsuppressed_df) + 2,
            x = "In this table, replace values with '1' if they should be suppressed")
  writeDataTable(wb = wb, sheet = 1, x = unsuppressed_df, startRow = 3,
                 startCol = ncol(unsuppressed_df) + 2,
                 tableStyle = "none", withFilter = FALSE, bandedRows = FALSE)

  # Write suppressed table - to compare to
  writeData(wb = wb, sheet = 1, startCol = (ncol(unsuppressed_df) * 2) + 3,
            x = paste("Suppressed table: compare with previous table, highlight",
                      "any cells that shouldn't have been suppressed"))
  writeDataTable(wb = wb, sheet = 1, x = suppressed_df, startRow = 3,
                 startCol = (ncol(unsuppressed_df) * 2) + 3,
                 tableStyle = "none", withFilter = FALSE, bandedRows = FALSE)

  # Add conditional formatting
  conditionalFormatting(wb = wb, sheet = 1, cols = 1:((ncol(unsuppressed_df) * 3) + 3),
                        rows = 1:(nrow(unsuppressed_df) + 3),
                        type = "between", rule = c(1, 4),
                        style = createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"))
  conditionalFormatting(wb = wb, sheet = 1, cols = 1:((ncol(unsuppressed_df) * 3) + 3),
                        rows = 1:(nrow(unsuppressed_df) + 3),
                        type = "between", rule = c(5, 14),
                        style = createStyle(fontColour = "#9C5700", bgFill = "#FFEB9C"))

  # Save workbook
  checking_path <- file.path(export_path, "circular-suppression-checks")

  if (!dir.exists(checking_path)) { dir.create(checking_path, recursive = T) }

  saveWorkbook(wb = wb, overwrite = T,
               file.path(checking_path, paste0("check_suppression_", file_name, ".xlsx")))
  
  }
  
  return(select(suppressed_data, -sample_size_og))
  
}
