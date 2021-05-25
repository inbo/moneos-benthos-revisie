find_file <- function(...) {
  rprojroot::find_root_file(...,
                            criterion = rprojroot::is_git_root)
}
