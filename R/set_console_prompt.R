#' @title Add my R console prompt.
#'
#' @author Josh Moatt
#'
#' @description Function to change the R console prompt. The default prompt is
#'   ">". This function will update the prompt so that if you are in a Git
#'   project, the prompt will display the acitve branch, e.g. "[@main]>". If you
#'   are not in a Git enabled branch, the prompt will revert to the default ">".
#'   Prompts can be manually edited using `usethis::edit_r_profile()`. Has two
#'   scopes, "user" will apply this generally to RStudio, creating a user R
#'   profile file, "project" will create a project specific R profile file.
#'
#' @param scope string. "user" sets the prompt globally, "project" sets it just
#'   for the active project.
#'
#' @return Altered R prompt
#'
#' @examples
#' \dontrun{
#' # set user prompt
#' set_console_prompt("user")
#' 
#' # set project prompt
#' set_console_prompt("project")
#' 
#'
#' @export
set_console_prompt <- function(scope) {
  # check correct scope selected
  if (!scope %in% c("user", "project")){
    cli::cli_alert_danger("Invalid scope selected")
    cli::cli_alert_info('Set scope to either "user" or "project"')
    stop("Cancelled: Invalid scope selected!", call = FALSE)
  }
  
  # set path based on scope. 
  if (scope == "user") {
    rprofile_path <- "~/.Rprofile"
  } else if (scope == "project") {
    rprofile_path <- "./.Rprofile"
  }
  
  # read current profile (if present)
  current_profile <- readLines(rprofile_path)
  
  # check for existing prompt in profile
  if (
    any(grepl("\\.First <- function\\(", current_profile)) &&
      any(grepl("my_prompt <- function\\(", current_profile))
    ) {
    cli::cli_alert_danger("Note: custom profile already present\n")
    cli::cli_alert_info("Edit using `usethis::edit_r_profile()` and try again.")
    stop("Cancelled: Prompt already present!", call = FALSE)
  } else if (prompt == "git") {
    my_prompt <- '.First <- function() {
  my_prompt <- function(...) {
    git_branch <- suppressWarnings(
      system(
        "git rev-parse --abbrev-ref HEAD",
        ignore.stderr = TRUE,
        intern = TRUE
      )
    )
    # branch name
    if (length(git_branch) != 0) {
      console_msg <- paste0("[@", git_branch, "]> ")
    } else {
      console_msg  <- "> "
    }
    # set console_msg to prompt
    options(prompt = console_msg)
    invisible(TRUE)
  }
  my_prompt()
  addTaskCallback(my_prompt)
}
'
  }
# write to file
cat(my_prompt, file = rprofile_path, append = TRUE)

cli::cli_alert_info("Restart R for prompt changes to take effect")

}