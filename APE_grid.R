# Packages ---------------------------------------------------------------------

library(INLA)
library(haven)
library(knitr)
library(tidyverse)
library(sf)

# Date range -------------------------------------------------------------------

Date.start <- "2018-01-01"
Date.end <- "2018-12-31"

# Import datasets --------------------------------------------------------------

grid <- read_sas("grid_model_pm_o3_2018.sas7bdat")
dat <- read_sas("Relationship_File_AQS_Model_PM.sas7bdat") 

# to see whether datasets are correctly imported
print(dim(grid))
print(dim(dat))

# Get distrinct grid locations -------------------------------------------------

grid$Grid_lon_new <- grid$Grid_lon-360
grid %>% distinct(Grid_lat, Grid_lon_new) -> distinct_grid_lat_lon

# Non Euclid -> Euclid locations in km -----------------------------------------
# Source: Define coordinates.R

## Grid ------------------------------------------------------------------------
geo.x = mapply ( function (x,y){c(x,y)}, 
                 x=distinct_grid_lat_lon$Grid_lon_new, 
                 y=distinct_grid_lat_lon$Grid_lat, SIMPLIFY=FALSE)
geo.x = lapply (geo.x, st_point)
geo.x = st_sfc(geo.x, crs = 4326) 
geo.x = st_transform(geo.x, crs = "ESRI:102004")

#Euclidian XY in meters
XY = as.data.frame(st_coordinates(geo.x))

# Transfer it to km
distinct_grid_lat_lon$X_Grid_km = XY$X / 1000
distinct_grid_lat_lon$Y_Grid_km = XY$Y / 1000

grid %>% 
  left_join(distinct_grid_lat_lon, by = c("Grid_lat", "Grid_lon_new")) %>%
  mutate(Date.group = as.numeric(as.Date(date) - as.Date("2018-01-01")) + 1) -> grid.new

# create output
num.weeks <- 365 %/% 7
num.days <- 7
range.Date.group <- NULL

for (w in c(1:num.weeks)){
  range.Date.group <- c(((w-1)*7+1): (w*7))
  grid.7days <- grid.new[grid.new$Date.group %in% range.Date.group,] %>% ungroup()
  write.csv(grid.7days, paste0("grid.7days.", w, ".csv"), row.names = F)
} 
   

