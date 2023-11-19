# packages ---------------------------------------------------------------------

library(INLA)
library(haven)
library(knitr)
library(tidyverse)

# import datasets --------------------------------------------------------------

PM_AQS_2018 <- read_sas("PM_AQS_2018.sas7bdat")
grid <- read_sas("grid_model_pm_o3_2018.sas7bdat")

# match each PM2.5 monitor to the nearest grid cell ----------------------------

grid$Grid_lon_new <- grid$Grid_lon-360

PM_AQS_2018 %>%
  dplyr::select(LAT_AQS, LON_AQS, Date) -> key

grid %>% distinct(Grid_lat, Grid_lon_new) -> distinct_grid_lat_lon

PM_AQS_2018 %>%
  distinct(LAT_AQS, LON_AQS) %>%
  cross_join(distinct_grid_lat_lon) %>%
  mutate(euclid_dist = sqrt((Grid_lat-LAT_AQS)^2+(Grid_lon_new-LON_AQS)^2)) %>%
  group_by(LAT_AQS, LON_AQS) %>%
  slice_min(euclid_dist) -> PM_grid_pairs

PM_AQS_2018 %>%
  left_join(PM_grid_pairs) %>%
  left_join(grid, by = c("Date" = "date", 
                         "Grid_lat" = "Grid_lat", 
                         "Grid_lon_new" = "Grid_lon_new")) %>%
  dplyr::select(Date, PM_AQS, PM25_TOT_NCAR, LAT_AQS, LON_AQS, Grid_lat, Grid_lon, 
                AQS_Site_id) -> PM_grid

