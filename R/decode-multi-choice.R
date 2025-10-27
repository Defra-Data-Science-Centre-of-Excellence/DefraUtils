# Functions to deal with multiple choice questions

#' Decompose multiple choice value
#'
#' When a survey, such as the Farm Business Survey, requires all answers to be
#' input as numbers, a geometric sequence (1, 2, 4, 8, 16, etc.) is used to code
#' multiple choice answers.
#' For example, the respondent choosing the first (code 1) and third (code 4)
#' options would be input as 5.
#' This method means that any combination of choices can be decoded into its
#' constituent parts.
#' This function decodes these answers by taking an input value and
#' returning separate values for each answer.
#'
#' @param n Value to decompose
#'
#' @export
#'
#' @family functions for decoding multiple choice answers
decompose_multi_choice_value <- function(n) {

  powers <- integer(0)

  while (n > 0) {
    power <- floor(log2(n))
    powers <- c(powers, 2^power)
    n <- n - 2^power
  }

  powers <- sort(powers)
  result <- paste(powers, collapse = "; ")

  return(result)

}

#' Apply decompose_multi_choice_value to a column
#'
#' When a survey, such as the Farm Business Survey, requires all answers to be
#' input as numbers, a geometric sequence (1, 2, 4, 8, 16, etc.) is used to code
#' multiple choice answers.
#' For example, the respondent choosing the first (code 1) and third (code 4)
#' options would be input as 5.
#' This method means that any combination of choices can be decoded into its
#' constituent parts.
#' This function decodes these answers by taking an input column of values and
#' returning separate values for each answer.
#'
#' @param column Column to decompose
#'
#' @export
#'
#' @family functions for decoding multiple choice answers
decompose_multi_choice_column <- function(column) {
  decoded <- lapply(column, decompose_multi_choice_value)
  return(decoded)
}

#' Decode a multiple choice column
#'
#' When a survey, such as the Farm Business Survey, requires all answers to be
#' input as numbers, a geometric sequence (1, 2, 4, 8, 16, etc.) is used to code
#' multiple choice answers.
#' For example, the respondent choosing the first (code 1) and third (code 4)
#' options would be input as 5.
#' This method means that any combination of choices can be decoded into its
#' constituent parts. Currently the maximum number of options for multiple 
#' choice is 12, 13 when including no answer.
#' This function decodes these answers by taking an input dataset with a column
#' of values and returning separate columns for each answer.
#'
#' @importFrom dplyr %>% select mutate na_if filter arrange left_join
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang set_names
#' @importFrom tibble tibble
#'
#'
#' @param input_data Dataset
#' @param var String; Column to decode
#' @param id_cols vector of strings, columns with unique identifiers
#' @param codes List; a list containing dataframes, the dataframes
#'  must be in the same order as the columns to decode, each dataframe must have
#'  a 2 columns one named `code` and the other named `description`. Default is 
#'  null.
#' @param colnames_option String; one of two options `code_numbers` or 
#' `code_descriptions`, default is `code_numbers`, the outputted colnames 
#' either be the code number or the names from the description column from
#'  imputed code dataframes.
#' @param data_option String; one of three options `binary`, `code_numbers`
#'  or `code_descriptions`, default is `code_numbers`, determines the format of 
#'  outputted decoded columns, they can be either in binary format, the original
#'  codes or the names from the description from the code dataframes.
#'  
#' @export
#'
#' @family functions for decoding multiple choice answers
decode_multi_choice_column <- function(input_data,
                                       var,
                                       id_cols,
                                       codes = NULL, 
                                       colnames_option = "code_numbers", 
                                       data_option = "code_numbers") {
  
  # This turns a single codes dataframe into a list, will still need to 
  if (is.data.frame(codes) == TRUE) {
    codes <- list(codes)
  }
  
  #### Error detection section ####  
  
  if (is.null(codes) == FALSE & is.list(codes) == FALSE){
    
    stop("codes needs to be a list of dataframes")
    
  }  else if (length(var) != length(codes) & !is.null(codes)) {
    
    stop("the list of codes must be the same length as the number of variables inputted")
    
  }
  
  if(!is.null(codes) & (
    lapply(codes, colnames) %>%
    lapply(setdiff, y= c("code", "description")) %>%
    unlist() %>%
    length() != 0 |
    any(lapply(codes, ncol) != 2)
    )
  ) {
    stop("All the tables in the list of codes must have 2 collumns, one named `code` and the other named `description`")
  }
  
  if(colnames_option %in% c("code_numbers","code_descriptions") == FALSE) {
    
    stop("`colnames_option` needs to be either `code_numbers` or `code_descriptions`")
    
  } else if (colnames_option == "code_descriptions" &  is.null(codes)) {
    
    stop("When using the `code_descriptions` as a colnames_option, you must have a list of codes, as this is where it gets data to name the columns")
    
  }
  
  if(data_option %in% c("binary","code_numbers","code_descriptions") == FALSE) {
    
    stop("`data_option` needs to be either `binary`, `code_numbers` or `code_descriptions`")
    
  } else if (data_option == "code_descriptions" &  is.null(codes)) {
    
    stop("When using the `code_descriptions` as a data_option, you must have a list of codes, as this is where it gets data to name the columns")
    
  }
  
  #### Proper analysis section ####
  
  name_for_0 <- "No answer selected"
  
  full_decoded_data <- input_data
  
  for (z in 1:length(var)){
    
    decoded_data <- input_data %>%
      select(all_of(id_cols), all_of(var[z])) %>%
      mutate(chosen_codes = decompose_multi_choice_column(.[[var[z]]])) %>%
      mutate(`0` = ifelse(!!sym(var[[z]]) == 0, 1, 0),
             `1` = ifelse(grepl("\\b1\\b", chosen_codes), 1, 0),
             `2` = ifelse(grepl("\\b2\\b", chosen_codes), 1, 0),
             `4` = ifelse(grepl("\\b4\\b", chosen_codes), 1, 0),
             `8` = ifelse(grepl("\\b8\\b", chosen_codes), 1, 0),
             `16` = ifelse(grepl("\\b16\\b", chosen_codes), 1, 0),
             `32` = ifelse(grepl("\\b32\\b", chosen_codes), 1, 0),
             `64` = ifelse(grepl("\\b64\\b", chosen_codes), 1, 0),
             `128` = ifelse(grepl("\\b128\\b", chosen_codes), 1, 0),
             `256` = ifelse(grepl("\\b256\\b", chosen_codes), 1, 0),
             `512` = ifelse(grepl("\\b512\\b", chosen_codes), 1, 0),
             `1024` = ifelse(grepl("\\b1024\\b", chosen_codes), 1, 0),
             `2048` = ifelse(grepl("\\b2048\\b", chosen_codes), 1, 0),
             across(c("0", "1", "2", "4", "8", "16", "32", "64",
                      "128", "256", "512", "1024", "2048"),
                    ~as.numeric(na_if(.x, 0)))) %>%
      select(-all_of(c(var[z], "chosen_codes"))) %>%
      pivot_longer(c("0", "1", "2", "4", "8", "16", "32", "64",
                     "128", "256", "512", "1024", "2048"),
                   names_to = "code") %>%
      filter(!is.na(value)) %>%
      select(-value) %>%
      mutate(code = as.numeric(code)) %>%
      arrange(code)
    
    # adding code description
    if (is.null(codes) == TRUE){
      
      decoded_data_tagged <- decoded_data
      
    } else {
      
      check_if_code_has_0 <- nrow(codes[[z]] %>% filter(code == 0)) > 0
      
      decoded_data_tagged <- decoded_data %>%
        filter(code %in% c(0,unique(codes[[z]] %>% select(code) %>% pull()))) %>%
        left_join(codes[[z]], by = "code")
      
      if (check_if_code_has_0 == FALSE) {
        decoded_data_tagged <- decoded_data_tagged %>%
          mutate(description = case_when(code == 0  ~ name_for_0,
                                         TRUE ~ description)
          )
      }
      
      if(nrow(decoded_data_tagged) < nrow(decoded_data) ){
        warning("The decoder detected unique codes that were not in the list of codes provided, please check data quality of either your input data or your list of codes")
      }
      
    }
    
    ### colname option template
    
    # This template is being created in case some options from the possible codes are not selected
    # This only applies if a list of codes was provided, as if no list is available there would be
    # no way of knowing for sure what is missing
    
    if (is.null(codes) == FALSE){
      
      if (colnames_option == "code_descriptions") {
        
        if (check_if_code_has_0 == FALSE){
          
          template_colname_options <- dplyr::bind_cols(
            input_data %>% select(all_of(id_cols)) %>% head(1) %>%
              mutate("test" = 10),
            codes[[z]] %>%
              pivot_wider(names_from = description, values_from = code)
          ) %>%
            dplyr::rename( !!sym(name_for_0) := test)
          
        } else {
          
          template_colname_options <- dplyr::bind_cols(
            input_data %>% select(all_of(id_cols)) %>% head(1),
            codes[[z]] %>%
              pivot_wider(names_from = description, values_from = code)
          )
        }
      } else {
        
        if (check_if_code_has_0 == FALSE){
          
          template_colname_options <- dplyr::bind_cols(
            input_data %>% select(all_of(id_cols)) %>% head(1),
            codes[[z]] %>%
              select(-description) %>%
              pivot_wider(names_from = code, values_from = code)
          )
          
        } else {
          
          template_colname_options <- dplyr::bind_cols(
            input_data %>% select(all_of(id_cols)) %>% head(1),
            codes[[z]] %>%
              select(-description) %>%
              pivot_wider(names_from = code, values_from = code)
          )
        }
      }
    }
    
    
    ### colname option
    
    if (colnames_option == "code_descriptions") {
      
      decoded_data_named_cols <- bind_rows(template_colname_options[-1,],
                                           decoded_data_tagged %>%
                                             pivot_wider(names_from = description, values_from = code)
      ) %>%
        set_names(c(id_cols, paste0(var[z], "__", colnames(.)[(length(id_cols)+1):ncol(.)])))
      
    } else { #this is the default option "code_numbers"
      
      # If there is a codes list but they still want to use numbers for their column names
      # It will still use the template so the its possible to know if any options were not selected
      if (is.null(codes) == FALSE){
        
        decoded_data_named_cols <- bind_rows(template_colname_options[-1,],
                                             decoded_data_tagged %>%
                                               select(all_of(id_cols),code) %>%
                                               pivot_wider(names_from = code, values_from = code)
        ) %>%
          set_names(c(id_cols, paste0(var[z], "__", colnames(.)[(length(id_cols)+1):ncol(.)])))
        # If no list of codes was given, it will just list code that were present in the data
      } else {
        
        decoded_data_named_cols <- decoded_data_tagged %>%
          select(all_of(id_cols),code) %>%
          pivot_wider(names_from = code, values_from = code) %>%
          set_names(c(id_cols, paste0(var[z], "__", colnames(.)[(length(id_cols)+1):ncol(.)])))
        
      }
      
    }
    
    full_decoded_data_collumns_named <- full_decoded_data %>%
      left_join(decoded_data_named_cols, by = id_cols)
    
    ### data_option
    
    colnames_temp_2 <- full_decoded_data_collumns_named %>% select(-colnames(full_decoded_data)) %>% colnames()
    
    if (data_option == "code_descriptions") {
      
      full_decoded_data_collumns_and_data_named <- full_decoded_data_collumns_named
      
      if (check_if_code_has_0 == FALSE){
        
        data_descriptions <- c(name_for_0 ,codes[[z]]$description)
        
      } else {
        
        data_descriptions <- c(codes[[z]]$description)
        
      }
      
      for (i in 1: length(colnames_temp_2)){
        
        full_decoded_data_collumns_and_data_named <- full_decoded_data_collumns_and_data_named %>%
          mutate( !!sym(colnames_temp_2[[i]]) := case_when( is.na(!!sym(colnames_temp_2[[i]])) ~ "not selected",
                                                            TRUE ~ paste(data_descriptions[i])
                                                            )
                  )
        }
      
    } else if (data_option == "binary") {
      
      full_decoded_data_collumns_and_data_named <- full_decoded_data_collumns_named
      
      for (i in 1: length(colnames_temp_2)){
        
        full_decoded_data_collumns_and_data_named <- full_decoded_data_collumns_and_data_named %>%
          mutate( !!sym(colnames_temp_2[[i]]) := case_when( is.na(!!sym(colnames_temp_2[[i]])) ~ 0,
                                                            TRUE ~ 1)
          )
      }
      
    } else { #this is the default option "code_numbers"
      
      full_decoded_data_collumns_and_data_named <- full_decoded_data_collumns_named
    }
    
    full_decoded_data <- full_decoded_data_collumns_and_data_named
    
  }
  
  return(full_decoded_data)
  
}

