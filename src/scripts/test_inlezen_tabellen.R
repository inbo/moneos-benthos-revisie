library(readxl)
library(rprojroot)
library(fs)
library(purrr)
source(find_root_file("src/utils/utility_functions.R",
                      criterion = is_git_root))

data_path <- get_map_data_moneos()

paths <- fs::dir_ls(data_path, regexp = "xlsx$")

load_excel <- function(path) {
  name <- path %>%
    path_file() %>%
    path_ext_remove() %>%
    tolower()
  assign(name, readxl::read_excel(path), inherits = TRUE)
}

walk(paths, load_excel)
