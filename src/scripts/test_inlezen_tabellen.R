library(readxl)
library(rprojroot)
library(fs)
library(purrr)
library(tidyverse)
source(find_root_file("src/utils/utility_functions.R",
  criterion = is_git_root
))

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

sp2018_metadata %>%
  rename(ecotoop_gepland = `ecotoop gepland`) %>%
  mutate(jaar = 2018) -> sp2018_metadata
sp2019_metadata %>%
  rename(datum = Datum) %>%
  mutate(
    jaar = 2019,
    ecotoop_gepland = NA
  ) %>%
  bind_rows(sp2018_metadata) %>%
  mutate(
    campagne = as.factor(campagne),
    staalcode = as.factor(substr(staal, 1, nchar(staal) - 3)),
    tidaal = as.factor(tidaal),
    geomorf = as.factor(geomorf),
    ecotoop_gepland = as.factor(ecotoop_gepland),
    ecotoop_werkelijk = recode(as.factor(ecotoop_werkelijk),
                               "zeer diep subtidaal" = "diep subtidaal"),
    SalZone = as.factor(SalZone),
    Vallei_deel = as.factor(Vallei_deel),
    Omessegment = as.factor(Omessegment),
    KRWzone = as.factor(KRWzone),
    jaar = as.factor(jaar)
  ) -> metadata
sp2018_benthos %>%
  mutate(jaar = 2018) %>%
  dplyr::filter(!is.na(staal)) -> sp2018_benthos
sp2019_benthos %>%
  mutate(jaar = 2019) %>%
  dplyr::filter(!is.na(staal)) %>%
  bind_rows(sp2018_benthos) %>%
  mutate(
    staalcode = as.factor(substr(staal, 1, nchar(staal) - 3)),
    soort = as.factor(soort),
    jaar = as.factor(jaar)
  ) -> benthos
rm(sp2018_benthos, sp2019_benthos, sp2018_metadata, sp2019_metadata)
# check voor dubbels in the metadata
a <- which(duplicated(metadata[, c("staal", "staalcode", "jaar")]))
a <- c(a, which(duplicated(metadata[, c("staal", "staalcode", "jaar")],
  fromLast = TRUE
)))
# Staal DI_SP_04 staat voor beide jaren twee keer in de metadata met lichtjes
# andere X-Y coordinaten en ander ecotoop_werkelijk. Dit dient uitgeklaard te
# worden want anders krijgen we dubbels in onderstaande left join.
# left join kan niet op datum want de datum in de stalen en in de metadata 
# verschilt redelijk vaak.
benthos %>%
  left_join(metadata, by = c("staal", "staalcode", "jaar")) ->
benthos
