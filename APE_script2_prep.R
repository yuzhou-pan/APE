# Packages ---------------------------------------------------------------------

library(INLA)
library(haven)
library(knitr)
library(tidyverse)
library(sf)
library(gtsummary)
library(gt)
library(webshot2)
library(huxtable)
#!/usr/bin/env Rscript

# grab the array id value from the environment variable passed from sbatch
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')

# coerce the value to an integer
n <- as.numeric(slurm_arrayid)

#if (n == 1){
#  inla.binary.install(os = "CentOS Linux-7")
#}


#sessionInfo()



# Date range -------------------------------------------------------------------

Date.start <- "2018-01-01"
Date.end <- "2018-12-31"

# Import datasets --------------------------------------------------------------

PM_AQS_2018 <- read_sas("PM_AQS_2018.sas7bdat")
grid <- read_sas("grid_model_pm_o3_2018.sas7bdat")
dat <- read_sas("Relationship_File_AQS_Model_PM.sas7bdat") 

# to see whether datasets are correctly imported
print(dim(PM_AQS_2018))
print(dim(grid))
print(dim(dat))

# Match each PM2.5 monitor to the nearest grid cell ----------------------------

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

# Remove neg or NA PM values ---------------------------------------------------
df <- PM_grid
df %>%
  drop_na(PM_AQS, PM25_TOT_NCAR) %>%
  filter(PM_AQS >= 0) %>%
  rowwise() %>%
  mutate(Grid_lon = Grid_lon - 360) -> df

# Non Euclid -> Euclid locations in km -----------------------------------------
# Source: Define coordinates.R

## Monitors --------------------------------------------------------------------
# Create spatial point object
geo.x = mapply ( function (x,y){c(x,y)}, x=dat$LON_AQS, y=dat$LAT_AQS , SIMPLIFY=FALSE)
geo.x = lapply (geo.x, st_point)
geo.x = st_sfc(geo.x, crs = 4326) 
geo.x = st_transform(geo.x, crs = "ESRI:102004")

#Euclidian XY in meters
XY = as.data.frame(st_coordinates(geo.x))
dat$X_AQS = XY$X
dat$Y_AQS = XY$Y

# Transfer it to km
dat$X_AQS_km <- XY$X / 1000
dat$Y_AQS_km <- XY$Y / 1000

## Grid ------------------------------------------------------------------------
geo.x = mapply ( function (x,y){c(x,y)}, x=dat$Grid_LON, y=dat$Grid_LAT, SIMPLIFY=FALSE)
geo.x = lapply (geo.x, st_point)
geo.x = st_sfc(geo.x, crs = 4326) 
geo.x = st_transform(geo.x, crs = "ESRI:102004")

#Euclidian XY in meters
XY = as.data.frame(st_coordinates(geo.x))
dat$X_Grid = XY$X
dat$Y_Grid = XY$Y

# Transfer it to km
dat$X_Grid_km <- XY$X / 1000
dat$Y_Grid_km <- XY$Y / 1000

# SPDE -------------------------------------------------------------------------

relationship <- dat

## All monitors ----------------------------------------------------------------

df$Date <- as.character(df$Date)

df %>%
  left_join(relationship %>%
              dplyr::select(AQS_Site_id, X_AQS_km, Y_AQS_km, X_Grid_km, Y_Grid_km), 
            by = "AQS_Site_id") %>%
  drop_na() %>%
  mutate(Date.group = as.numeric(as.Date(Date) - as.Date("2018-01-01")) + 1) -> df

# descriptive table 

df %>% 
  summarise(site_num = n_distinct(AQS_Site_id)) %>%
  bind_rows(
    df %>% 
      mutate(quarter = case_when(
        (Date >= "2018-01-01" & Date <= "2018-03-31") ~ "Q1",
        (Date >= "2018-04-01" & Date <= "2018-06-30") ~ "Q2",
        (Date >= "2018-07-01" & Date <= "2018-09-30") ~ "Q3",
        (Date >= "2018-10-01" & Date <= "2018-12-31") ~ "Q4")) %>%
      group_by(quarter) %>%
      summarise(site_num = n_distinct(AQS_Site_id))
  ) -> df.site_num

df.site_num[1, 2] <- "Overall"
df.site_num.t <- t(df.site_num)
colnames(df.site_num.t) <- df.site_num.t[2, ]
df.site_num.t <- df.site_num.t[1, ]
df.site_num.t <- c("Count", df.site_num.t)

df.site_num %>%
  uncount(site_num, .remove = FALSE) %>%
  tbl_summary(by = quarter)

tbl_summary(df.site_num, by = quarter) %>%
  add_overall()

wb <- openxlsx::createWorkbook()
df %>% 
  mutate(quarter = case_when(
    (Date >= "2018-01-01" & Date <= "2018-03-31") ~ "Q1",
    (Date >= "2018-04-01" & Date <= "2018-06-30") ~ "Q2",
    (Date >= "2018-07-01" & Date <= "2018-09-30") ~ "Q3",
    (Date >= "2018-10-01" & Date <= "2018-12-31") ~ "Q4")) %>%
  select(-one_of(c("Date", "LAT_AQS", "LON_AQS", "Grid_lat", "Grid_lon", 
                   "Date.group", "AQS_Site_id", "X_AQS_km", "Y_AQS_km", 
                   "X_Grid_km", "Y_Grid_km"))) %>%
  gtsummary::tbl_summary(
    by = quarter,
    type = all_continuous() ~ "continuous2",
    label = list(PM_AQS ~ "Daily PM2.5",
                 PM25_TOT_NCAR ~ "Simulated CMAQ PM2.5"),
    statistic = list(all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"))) %>%
  add_overall() %>%
  as_hux_table() %>%
  add_rows(c("Monitors", rep("", 5)), after = 7) %>%
  add_rows(df.site_num.t) %>%
  as_Workbook(Workbook = wb) -> wb
openxlsx::saveWorkbook(wb, "descriptive_table.xlsx", overwrite = T)

## divide 365 days into multiple windows ---------------------------------------

# there is no PM measurement dated "2018-12-31"
# > sum(df$Date == "2018-12-31")
# [1] 0
# > sum(df$Date == "2018-12-30")
# [1] 569

num.weeks <- 365 %/% 7
num.days <- 7
range.Date.group <- NULL

set.seed(123456)
result.full <- NULL

for (w in c(1:num.weeks)){
  range.Date.group <- c(((w-1)*7+1): (w*7))
  df.7days <- df[df$Date.group %in% range.Date.group,] %>% ungroup()
  write.csv(df.7days, paste0("df.7days.", w, ".csv"), row.names = F)
}