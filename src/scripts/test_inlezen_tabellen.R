library(readxl)
library(rprojroot)
library(fs)
library(purrr)
library(tidyverse)
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

sp2018_metadata %>%
  rename(ecotoop_gepland = `ecotoop gepland`) %>%
  mutate(jaar = 2018) -> sp2018_metadata
sp2019_metadata %>%
  rename(datum = Datum) %>%
  mutate(jaar = 2019,
         ecotoop_gepland = NA) %>%
  bind_rows(sp2018_metadata) %>%
  mutate(campagne = as.factor(campagne),
         staalcode = as.factor(substr(staal,1,nchar(staal)-3)),#lijkt steeds een code en daarna een volgnummer
         tidaal = as.factor(tidaal),
         geomorf = as.factor(geomorf),
         ecotoop_gepland = as.factor(ecotoop_gepland),
         ecotoop_werkelijk = as.factor(ecotoop_werkelijk),
         SalZone = as.factor(SalZone),
         Vallei_deel = as.factor(Vallei_deel),
         Omessegment = as.factor(Omessegment),
         KRWzone = as.factor(KRWzone),
         year = as.factor(year)) -> metadata
sp2018_benthos %>%
  mutate(year = 2018) ->sp2018_benthos
sp2019_benthos %>%
  mutate(year = 2019) %>%
  bind_rows(sp2018_benthos) %>%
  mutate(staalcode = as.factor(substr(staal,1,nchar(staal)-3)),
         soort = as.factor(soort)) -> benthos
rm(sp2018_benthos, sp2019_benthos, sp2018_metadata, sp2019_metadata)


