library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rprojroot)
source(find_root_file("src/utils/utility_functions.R",
                      criterion = is_git_root))

data_path <- get_map_data_moneos()
gdb_path <- file.path(data_path, "GIS/MONEOS.gdb")

layers <- st_layers(gdb_path)

layers

ecotoop <- read_sf(gdb_path,
                   layer = layers$name[1]) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(ecotoop_werkelijk = recode(as.factor(Ecotoop),
                                    "breuksteen" = "hard substraat",
                                    "hard antropogeen" = "hard substraat",
                                    "hoog slik"= "hoge slikzone",
                                    "middelhoog slik" = "middelhoge slikzone",
                                    "laag slik" = "lage slikzone",
                                    "intertidaal" = "slik"),
         KRWzone = recode(as.factor(KRWzone),
                          "TijarmZwijnaarde" = "Tijarm")
         )

random_points_2020 <- read_sf(gdb_path,
                      layer = layers$name[2])

raaien <- read_sf(gdb_path,
                  layer = layers$name[3])

schelde_contour <- read_sf(gdb_path,
                           layer = layers$name[4])

ecotoop %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = raaien, colour = "red", alpha = 0.3) +
  geom_sf(data = random_points_2020, colour = "blue", alpha = 0.3)

ecotoop %>%
  st_drop_geometry() %>%
  distinct(KRWzone, Naam, Ecotoop, SalZone, NrRandomPunten)


ecotoop_aparte_polygonen <- ecotoop %>%
  select(-starts_with("Shape"), -NrRandomPunten) %>%
  st_cast("POLYGON")

ecotoop_aparte_polygonen %>%
  st_drop_geometry() %>%
  count(Ecotoop, SalZone, Naam, KRWzone) %>%
  arrange(-n)
