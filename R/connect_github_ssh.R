#' @title Connect RStudio to your GitHub account using SSH
#'
#' @author Josh Moatt
#'
#' @description Connect RStudio to your GitHub account using SSH - this is the
#'   preferred method of connecting RStudio and GitHub on the DASH platform. The
#'   user will only need to supply their GitHub username and email address, then
#'   the function will set their credentials and generate an SSH key. Once the
#'   SSH key has been added to GitHub, the function does the final bits of
#'   set-up. All steps and outputs are printed in the RStudio console rather
#'   than the terminal, which offers a more user friendly experience. This
#'   method of connection is only recommended for the DASH platform - for
#'   connecting local RStudio installs to GitHub, see `connect_github_pat`.
#'
#' @details The suggested method for connecting RStudio and GitHub on the DASH
#'   platform is via SSH. The alternative is via Personal Access Token, but this
#'   method can be a bit clunky and frustrating on the DASH platform. With SSH
#'   providing a much smoother user experience.
#'
#'   This function is designed to simplify the connection process and avoid the
#'   user having to interact with the terminal - which many beginners to Git
#'   find intimidating. Instead, all prompts and outputs are returned in the
#'   RStudio console.
#'
#'   The users calls the function and gives their user credentials (username and
#'   email), the function will set these and print an output to confirm they
#'   have been set. The function will then create a hidden SSH folder and
#'   sub-folder for "id_ed25519" (~/.ssh/id_ed25519). Once done, it will then
#'   generate your SSH key and save it to this folder. The SSH key will then be
#'   printed in the console. The function then adds GitHub as a known host.
#'
#'   At this point, the function pauses and asks the user to confirm they have
#'   added the SSH key to GitHub. The user can then copy the SSH key from the
#'   console, got to GitHub -> settings -> SSH and GPG keys and add the SSH key.
#'
#'   Once added, the user can respond to the prompt and the function will finish
#'   establishing the connection. If all works as expected, the message "Hi
#'   username! You've successfully authenticated, but GitHub does not provide
#'   shell access" will be printed in the console.
#'
#'   This method of connection is only recommended for the DASH platform - for
#'   connecting local RStudio installs to GitHub, see `connect_github_pat`.
#'
#' @param username user's GitHub username
#'
#' @param email user's email address
#'
#' @examples
#' \dontrun{
#' # Set GitHub credentials
#' connect_github_ssh(
#'   username = "my_github_username",
#'   email = "my_email"
#' )
#' }
#'
#' @export
connect_github_ssh <- function(
  username,
  email
) {
  # Git config
  system(paste0('git config --global user.name "', username, '"'))
  system(paste0('git config --global user.email "', email, '"'))

  # check config
  cli::cli_text("")
  cli::cli_alert_success("GitHub credentials set to:")
  system("git config --global --list")
  cli::cli_text("")

  # Set SSH key path
  ssh_key_path <- "~/.ssh/id_ed25519"

  # Create SSH directory
  dir.create("~/.ssh", showWarnings = FALSE, recursive = TRUE)

  # Generate SSH key (no prompts)
  system(
    sprintf('ssh-keygen -t ed25519 -C "%s" -f %s -N ""', email, ssh_key_path)
  )

  # messages
  cli::cli_text("")
  cli::cli_alert_success("SSH token created")
  cli::cli_alert_info("Now copy your SSH token below and add it to your SSH keys at https://github.com/settings/keys :")
  cli::cli_text("")

  # Show public key (for adding to GitHub)
  system("cat ~/.ssh/id_ed25519.pub")

  # Message
  cli::cli_text("")
  cli::cli_alert_success("Adding github.com to known hosts")

  # Add GitHub to known_hosts
  system("ssh-keyscan github.com >> ~/.ssh/known_hosts")

  {
    tmp <- readline("Have you added your SSH token on GitHub? y/n \n")
    if (tmp == "y") {
      # connect GitHub and RStudio
      cli::cli_alert_success("Connected with GitHub")
      system("ssh -T git@github.com")
    } else {
      cli::cli_alert_danger("SSH token not added to GitHub. Add now and then complete manually \n")
      cli::cli_alert_info("To complete manually, run `ssh -T git@github.com` in the terminal once SSH token added to GitHub \n")
    }
  }
}
