# Packages ---------------------------------------------------------------------

library(INLA)
library(haven)
library(knitr)
library(tidyverse)
library(sf)
#inla.binary.install(os = "CentOS Linux-7")

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
dat$X_AQS = XY$X
dat$Y_AQS = XY$Y

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
  
  points <- data.matrix(cbind(df.7days$X_AQS_km, 
                              df.7days$Y_AQS_km))
  
  grid.pts <- data.matrix(cbind(df.7days$X_Grid_km, 
                                df.7days$Y_Grid_km))
  
## initialization --------------------------------------------------------------
  
  n.total <- nrow(points)
  base.size <- n.total %/% 10
  row.num.list <- sample(n.total)
  
  index <- NULL
  
  for (i in c(1:10)){
    index <- c(index, list(sample(row.num.list, base.size)))
    row.num.list <- row.num.list[!(row.num.list %in% index[[i]])]
  }
  
  idx <- 1
  for (r in row.num.list){
    index[[idx]] <- c(index[[idx]], r)
    idx <- idx + 1
  }
  
  rmse.1.1 <- NULL; rmse.2.1 <- NULL; rmse.3.1 <- NULL; rmse.4.1 <- NULL
  rmse.1.2 <- NULL; rmse.2.2 <- NULL; rmse.3.2 <- NULL; rmse.4.2 <- NULL
  
  R2.1.1 <- NULL; R2.2.1 <- NULL; R2.3.1 <- NULL; R2.4.1 <- NULL
  R2.1.2 <- NULL; R2.2.2 <- NULL; R2.3.2 <- NULL; R2.4.2 <- NULL
  
  prop.1.1 <- NULL; prop.2.1 <- NULL; prop.3.1 <- NULL; prop.4.1 <- NULL
  prop.1.2 <- NULL; prop.2.2 <- NULL; prop.3.2 <- NULL; prop.4.2 <- NULL
  
  mean.pred.1.1.list <- rep(NA, n.total); mean.pred.1.2.list <- rep(NA, n.total)
  mean.pred.2.1.list <- rep(NA, n.total); mean.pred.2.2.list <- rep(NA, n.total)
  mean.pred.3.1.list <- rep(NA, n.total); mean.pred.3.2.list <- rep(NA, n.total)
  mean.pred.4.1.list <- rep(NA, n.total); mean.pred.4.2.list <- rep(NA, n.total)
  
  ll.pred.1.1.list <- rep(NA, n.total); ll.pred.1.2.list <- rep(NA, n.total)
  ll.pred.2.1.list <- rep(NA, n.total); ll.pred.2.2.list <- rep(NA, n.total)
  ll.pred.3.1.list <- rep(NA, n.total); ll.pred.3.2.list <- rep(NA, n.total)
  ll.pred.4.1.list <- rep(NA, n.total); ll.pred.4.2.list <- rep(NA, n.total)
  
  ul.pred.1.1.list <- rep(NA, n.total); ul.pred.1.2.list <- rep(NA, n.total)
  ul.pred.2.1.list <- rep(NA, n.total); ul.pred.2.2.list <- rep(NA, n.total)
  ul.pred.3.1.list <- rep(NA, n.total); ul.pred.3.2.list <- rep(NA, n.total)
  ul.pred.4.1.list <- rep(NA, n.total); ul.pred.4.2.list <- rep(NA, n.total)
  
  sd.pred.1.1.list <- rep(NA, n.total); sd.pred.1.2.list <- rep(NA, n.total)
  sd.pred.2.1.list <- rep(NA, n.total); sd.pred.2.2.list <- rep(NA, n.total)
  sd.pred.3.1.list <- rep(NA, n.total); sd.pred.3.2.list <- rep(NA, n.total)
  sd.pred.4.1.list <- rep(NA, n.total); sd.pred.4.2.list <- rep(NA, n.total)
  
  cv.list <- rep(NA, n.total)
  Date.group.list <- rep(NA, n.total)
  
  ## For loop --------------------------------------------------------------------
  
  radius <- 100
  for (i in c(1:10)){
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    index.test <- index[i]
    names(index.test) <- NULL
    index.test <- unlist(index.test)
    
    df.test <- df.7days[index.test, ] # also as centers
    
    # figure out the interior set
    df.interior <- NULL
    for (a.row in nrow(df.test)){
      center.X <- df.test[a.row, ]$X_AQS_km
      center.Y <- df.test[a.row, ]$Y_AQS_km
      center.Date.group <- df.test[a.row, ]$Date.group
      df.7days %>%
        rowwise() %>%
        filter((((X_AQS_km - center.X)^2 + (Y_AQS_km - center.Y)^2) <= radius^2)) -> temp.interior
      #abs(Date.group - center.Date.group) <= 1) -> temp.interior
      if (is.null(df.interior)){
        df.interior <- temp.interior
      } else{
        df.interior <- rbind(df.interior, temp.interior)
      }
    }
    
    setdiff(df.7days, df.interior) -> df.train
    
    print("Here!!!!")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    n.test <- nrow(df.test)
    n.train <- nrow(df.train)
    
    # monitors
    points.train <- data.matrix(cbind(df.train$X_AQS_km, df.train$Y_AQS_km))
    points.test <- data.matrix(cbind(df.test$X_AQS_km, df.test$Y_AQS_km))
    
    # grid centroids
    grid.pts.train <- data.matrix(cbind(df.train$X_Grid_km, df.train$Y_Grid_km))
    grid.pts.test <- data.matrix(cbind(df.test$X_Grid_km, df.test$Y_Grid_km))
    
    # y
    PM_AQS.train <- df.train$PM_AQS
    PM_AQS.test <- df.test$PM_AQS
    
    # x
    PM25_TOT_NCAR.train <- df.train$PM25_TOT_NCAR
    PM25_TOT_NCAR.test <- df.test$PM25_TOT_NCAR
    
    # Dates
    Date.group.train <- df.train$Date.group
    Date.group.test <- df.test$Date.group
    
    # Diag
    diag.train <- Diagonal(length(PM25_TOT_NCAR.train), PM25_TOT_NCAR.train)
    diag.test <- Diagonal(length(PM25_TOT_NCAR.test), PM25_TOT_NCAR.test)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print("before 1")
    print(paste0("df.7days dimensions: ", dim(df.7days)))
    print(paste0("df.interior dimensions: ", dim(df.interior)))
    print(paste0("df.train dimemsions: ", dim(df.train)))
    print(paste0("points.train dimensions: ", dim(points.train)))
    ### 1. Define mesh -----------------------------------------------------------
    mesh.train <- inla.mesh.2d(loc = points.train,
                               cutoff = 12,
                               max.edge = c(300, 600))
    print("after 1")
    ## 2. Get A and Ap -----------------------------------------------------------
    A.s <- inla.spde.make.A(mesh = mesh.train, loc = points.train)
    Ap.s <- inla.spde.make.A(mesh = mesh.train, loc = points.test)
    
    A.st <- inla.spde.make.A(mesh = mesh.train, loc = points.train, group = Date.group.train)
    Ap.st <- inla.spde.make.A(mesh = mesh.train, loc = points.test, group = Date.group.test)
    
    A.s.cov <- inla.spde.make.A(mesh = mesh.train, loc = points.train)
    Ap.s.cov <- inla.spde.make.A(mesh = mesh.train, loc = points.test)
    
    A.st.cov <- inla.spde.make.A(mesh = mesh.train, loc = points.train, group = Date.group.train)
    Ap.st.cov <- inla.spde.make.A(mesh = mesh.train, loc = points.test, group = Date.group.test)
    
    print("after 2")
    ## 3. Get spde ---------------------------------------------------------------
    spde <- inla.spde2.matern(mesh.train, alpha = 2, constr = F)
    print("after 3")
    ## 4. Get spde index ---------------------------------------------------------
    mesh.index.s <- inla.spde.make.index(name = "s", n.spde = spde$n.spde)
    mesh.index.s.cov <- inla.spde.make.index(name = "s.cov", n.spde = spde$n.spde)
    mesh.index.st <- inla.spde.make.index(name = "st", n.spde = spde$n.spde, n.group = 7)
    mesh.index.st.cov <- inla.spde.make.index(name = "st.cov", n.spde = spde$n.spde, n.group = 7)
    print("after 4")
    ## 5. Stack for estimation and prediction ------------------------------------
    # 1.1
    stk.e.1.1 <- inla.stack(tag = "est",
                            data = list(PM_AQS = PM_AQS.train),
                            A = list(1, A.s),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.train), 
                                           s = mesh.index.s))
    stk.p.1.1 <- inla.stack(tag = "pred",
                            data = list(PM_AQS = NA),
                            A = list(1, Ap.s),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.test), 
                                           s = mesh.index.s))
    #print("here")
    # 1.2
    stk.e.1.2 <- inla.stack(tag = "est",
                            data = list(PM_AQS = PM_AQS.train),
                            A = list(1, A.st),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.train),
                                           st = mesh.index.st))
    stk.p.1.2 <- inla.stack(tag = "pred",
                            data = list(PM_AQS = NA),
                            A = list(1, Ap.st),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.test),
                                           st = mesh.index.st))
    # 2.1  
    stk.e.2.1 <- inla.stack(tag = "est",
                            data = list(PM_AQS = PM_AQS.train),
                            A = list(1, A.s),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.train), 
                                           s = mesh.index.s))
    stk.p.2.1 <- inla.stack(tag = "pred",
                            data = list(PM_AQS = NA),
                            A = list(1, Ap.s),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.test), 
                                           s = mesh.index.s))
    # 2.2
    stk.e.2.2 <- inla.stack(tag = "est",
                            data = list(PM_AQS = PM_AQS.train),
                            A = list(1, A.st),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.train), 
                                           st = mesh.index.st))
    stk.p.2.2 <- inla.stack(tag = "pred",
                            data = list(PM_AQS = NA),
                            A = list(1, Ap.st),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.test), 
                                           st = mesh.index.st))
    # 3.1
    stk.e.3.1 <- inla.stack(tag = "est",
                            data = list(PM_AQS = PM_AQS.train),
                            A = list(1, A.s, diag.train %*% A.s.cov),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.train),
                                           s = mesh.index.s,
                                           s.cov = mesh.index.s.cov))
    stk.p.3.1 <- inla.stack(tag = "pred",
                            data = list(PM_AQS = NA),
                            A = list(1, Ap.s, diag.test %*% Ap.s.cov),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.test),
                                           s = mesh.index.s,
                                           s.cov = mesh.index.s.cov))
    # 3.2
    stk.e.3.2 <- inla.stack(tag = "est",
                            data = list(PM_AQS = PM_AQS.train),
                            A = list(1, A.st, diag.train %*% A.s.cov),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.train),
                                           st = mesh.index.st,
                                           s.cov = mesh.index.s.cov))
    stk.p.3.2 <- inla.stack(tag = "pred",
                            data = list(PM_AQS = NA),
                            A = list(1, Ap.st, diag.test %*% Ap.s.cov),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.test),
                                           st = mesh.index.st,
                                           s.cov = mesh.index.s.cov))
    # 4.1
    stk.e.4.1 <- inla.stack(tag = "est",
                            data = list(PM_AQS = PM_AQS.train),
                            A = list(1, A.s, diag.train %*% A.st.cov),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.train),
                                           s = mesh.index.s,
                                           st.cov = mesh.index.st.cov))
    stk.p.4.1 <- inla.stack(tag = "pred",
                            data = list(PM_AQS = NA),
                            A = list(1, Ap.s, diag.test %*% Ap.st.cov),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.test),
                                           s = mesh.index.s,
                                           st.cov = mesh.index.st.cov))
    # 4.2
    stk.e.4.2 <- inla.stack(tag = "est",
                            data = list(PM_AQS = PM_AQS.train),
                            A = list(1, A.st, diag.train %*% A.st.cov),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.train),
                                           st = mesh.index.st,
                                           st.cov = mesh.index.st.cov))
    stk.p.4.2 <- inla.stack(tag = "pred",
                            data = list(PM_AQS = NA),
                            A = list(1, Ap.st, diag.test %*% Ap.st.cov),
                            effects = list(data.frame(b0 = 1,
                                                      PM25_TOT_NCAR = PM25_TOT_NCAR.test),
                                           st = mesh.index.st,
                                           st.cov = mesh.index.st.cov))
    print("after 5")
    ## 6. Combine stacks ---------------------------------------------------------
    stk.full.1.1 <- inla.stack(stk.e.1.1, stk.p.1.1)
    stk.full.1.2 <- inla.stack(stk.e.1.2, stk.p.1.2)
    stk.full.2.1 <- inla.stack(stk.e.2.1, stk.p.2.1)
    stk.full.2.2 <- inla.stack(stk.e.2.2, stk.p.2.2)
    stk.full.3.1 <- inla.stack(stk.e.3.1, stk.p.3.1)
    stk.full.3.2 <- inla.stack(stk.e.3.2, stk.p.3.2)
    stk.full.4.1 <- inla.stack(stk.e.4.1, stk.p.4.1)
    stk.full.4.2 <- inla.stack(stk.e.4.2, stk.p.4.2)
    print("after 6")
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # formula
    f.1.1 <- PM_AQS ~ 0 + b0 + f(s, model = spde, constr = F)
    f.1.2 <- PM_AQS ~ 0 + b0 + f(st, model = spde, constr = F, group = st.group, control.group = list(model = "ar1"))
    
    f.2.1 <- PM_AQS ~ 0 + b0 + PM25_TOT_NCAR + f(s, model = spde, constr = F)
    f.2.2 <- PM_AQS ~ 0 + b0 + PM25_TOT_NCAR + f(st, model = spde, constr = F, group = st.group, control.group = list(model = "ar1"))
    
    f.3.1 <- PM_AQS ~ 0 + b0 + f(s.cov, model = spde, constr = F) + f(s, model = spde, constr = F)
    f.3.2 <- PM_AQS ~ 0 + b0 + f(s.cov, model = spde, constr = F) + f(st, model = spde, constr = F, group = st.group, control.group = list(model = "ar1"))
    
    f.4.1 <- PM_AQS ~ 0 + b0 + f(st.cov, model = spde, constr = F, group = st.cov.group, control.group = list(model = "ar1")) + f(s, model = spde, constr = F)
    f.4.2 <- PM_AQS ~ 0 + b0 + f(st.cov, model = spde, constr = F, group = st.cov.group, control.group = list(model = "ar1")) + f(st, model = spde, constr = F, group = st.group, control.group = list(model = "ar1"))
    
    print("after formula")
    # model
    ####### model spec ######################
    model.1.1 <- inla(f.1.1, data = inla.stack.data(stk.full.1.1),
                      control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.1.1)))
    
    model.1.2 <- inla(f.1.2, data = inla.stack.data(stk.full.1.2),
                      control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.1.2)))
    
    model.2.1 <- inla(f.2.1, data = inla.stack.data(stk.full.2.1),
                      control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.2.1)))
    
    model.2.2 <- inla(f.2.2, data = inla.stack.data(stk.full.2.2),
                      control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.2.2)))
    
    model.3.1 <- inla(f.3.1, data = inla.stack.data(stk.full.3.1),
                      control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.3.1)))
    
    model.3.2 <- inla(f.3.2, data = inla.stack.data(stk.full.3.2),
                      control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.3.2)))
    
    model.4.1 <- inla(f.4.1, data = inla.stack.data(stk.full.4.1),
                      control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.4.1)))
    
    model.4.2 <- inla(f.4.2, data = inla.stack.data(stk.full.4.2),
                      control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.4.2)))
    print("after model")
    #svc.3.1 <- ~ -1 + beta_0(geometry, model = spde) + beta_1(geometry, weights = PM25_TOT_NCAR)
    #svc.3.1.f <- PM_AQS ~ .
    
    #model.test <- inlabru::bru(svc.3.1, like(formula = svc.3.1.f), family = "gaussian", 
    #options = list(
    #   control.compute = list(waic = TRUE, cpo = FALSE),
    #   control.inla = list(int.strategy = "eb"),
    #   verbose = FALSE), 
    # data = df.train)
    
    
    # retrieve predicted value summary statistics
    index.test.1.1 <- inla.stack.index(stk.full.1.1, "pred")$data
    mean.pred.1.1 <- model.1.1$summary.fitted.values[index.test.1.1, ]$mean
    sd.pred.1.1 <- model.1.1$summary.fitted.values[index.test.1.1, ]$sd
    
    index.test.1.2 <- inla.stack.index(stk.full.1.2, "pred")$data
    mean.pred.1.2 <- model.1.2$summary.fitted.values[index.test.1.2, ]$mean
    sd.pred.1.2 <- model.1.2$summary.fitted.values[index.test.1.2, ]$sd
    
    index.test.2.1 <- inla.stack.index(stk.full.2.1, "pred")$data
    mean.pred.2.1 <- model.2.1$summary.fitted.values[index.test.2.1, ]$mean
    sd.pred.2.1 <- model.2.1$summary.fitted.values[index.test.2.1, ]$sd
    
    index.test.2.2 <- inla.stack.index(stk.full.2.2, "pred")$data
    mean.pred.2.2 <- model.2.2$summary.fitted.values[index.test.2.2, ]$mean
    sd.pred.2.2 <- model.2.2$summary.fitted.values[index.test.2.2, ]$sd
    
    index.test.3.1 <- inla.stack.index(stk.full.3.1, "pred")$data
    mean.pred.3.1 <- model.3.1$summary.fitted.values[index.test.3.1, ]$mean
    sd.pred.3.1 <- model.3.1$summary.fitted.values[index.test.3.1, ]$sd
    
    index.test.3.2 <- inla.stack.index(stk.full.3.2, "pred")$data
    mean.pred.3.2 <- model.3.2$summary.fitted.values[index.test.3.2, ]$mean
    sd.pred.3.2 <- model.3.2$summary.fitted.values[index.test.3.2, ]$sd
    
    index.test.4.1 <- inla.stack.index(stk.full.4.1, "pred")$data
    mean.pred.4.1 <- model.4.1$summary.fitted.values[index.test.4.1, ]$mean
    sd.pred.4.1 <- model.4.1$summary.fitted.values[index.test.4.1, ]$sd
    
    index.test.4.2 <- inla.stack.index(stk.full.4.2, "pred")$data
    mean.pred.4.2 <- model.4.2$summary.fitted.values[index.test.4.2, ]$mean
    sd.pred.4.2 <- model.4.2$summary.fitted.values[index.test.4.2, ]$sd
    
    
    
    #~~~~~~~~~~
    sigma.sq.1.1 <- (model.1.1$summary.hyperpar$mode[1])^(-1)
    sigma.sq.1.2 <- (model.1.2$summary.hyperpar$mode[1])^(-1)
    sigma.sq.2.1 <- (model.2.1$summary.hyperpar$mode[1])^(-1)
    sigma.sq.2.2 <- (model.2.2$summary.hyperpar$mode[1])^(-1)
    sigma.sq.3.1 <- (model.3.1$summary.hyperpar$mode[1])^(-1)
    sigma.sq.3.2 <- (model.3.2$summary.hyperpar$mode[1])^(-1)
    sigma.sq.4.1 <- (model.4.1$summary.hyperpar$mode[1])^(-1)
    sigma.sq.4.2 <- (model.4.2$summary.hyperpar$mode[1])^(-1)
    
    ll.pred.1.1 <- mean.pred.1.1 - 1.96 * sqrt(sd.pred.1.1^2 + sigma.sq.1.1)
    ul.pred.1.1 <- mean.pred.1.1 + 1.96 * sqrt(sd.pred.1.1^2 + sigma.sq.1.1)
    
    ll.pred.1.2 <- mean.pred.1.2 - 1.96 * sqrt(sd.pred.1.2^2 + sigma.sq.1.2)
    ul.pred.1.2 <- mean.pred.1.2 + 1.96 * sqrt(sd.pred.1.2^2 + sigma.sq.1.2)
    
    ll.pred.2.1 <- mean.pred.2.1 - 1.96 * sqrt(sd.pred.2.1^2 + sigma.sq.2.1)
    ul.pred.2.1 <- mean.pred.2.1 + 1.96 * sqrt(sd.pred.2.1^2 + sigma.sq.2.1)
    
    ll.pred.2.2 <- mean.pred.2.2 - 1.96 * sqrt(sd.pred.2.2^2 + sigma.sq.2.2)
    ul.pred.2.2 <- mean.pred.2.2 + 1.96 * sqrt(sd.pred.2.2^2 + sigma.sq.2.2)
    
    ll.pred.3.1 <- mean.pred.3.1 - 1.96 * sqrt(sd.pred.3.1^2 + sigma.sq.3.1)
    ul.pred.3.1 <- mean.pred.3.1 + 1.96 * sqrt(sd.pred.3.1^2 + sigma.sq.3.1)
    
    ll.pred.3.2 <- mean.pred.3.2 - 1.96 * sqrt(sd.pred.3.2^2 + sigma.sq.3.2)
    ul.pred.3.2 <- mean.pred.3.2 + 1.96 * sqrt(sd.pred.3.2^2 + sigma.sq.3.2)
    
    ll.pred.4.1 <- mean.pred.4.1 - 1.96 * sqrt(sd.pred.4.1^2 + sigma.sq.4.1)
    ul.pred.4.1 <- mean.pred.4.1 + 1.96 * sqrt(sd.pred.4.1^2 + sigma.sq.4.1)
    
    ll.pred.4.2 <- mean.pred.4.2 - 1.96 * sqrt(sd.pred.4.2^2 + sigma.sq.4.2)
    ul.pred.4.2 <- mean.pred.4.2 + 1.96 * sqrt(sd.pred.4.2^2 + sigma.sq.4.2)
    
    
    mean.pred.1.1.list[index.test] <- mean.pred.1.1
    ll.pred.1.1.list[index.test] <- ll.pred.1.1
    ul.pred.1.1.list[index.test] <- ul.pred.1.1
    sd.pred.1.1.list[index.test] <- sd.pred.1.1
    
    mean.pred.1.2.list[index.test] <- mean.pred.1.2
    ll.pred.1.2.list[index.test] <- ll.pred.1.2
    ul.pred.1.2.list[index.test] <- ul.pred.1.2
    sd.pred.1.2.list[index.test] <- sd.pred.1.2
    
    mean.pred.2.1.list[index.test] <- mean.pred.2.1
    ll.pred.2.1.list[index.test] <- ll.pred.2.1
    ul.pred.2.1.list[index.test] <- ul.pred.2.1
    sd.pred.2.1.list[index.test] <- sd.pred.2.1
    
    mean.pred.2.2.list[index.test] <- mean.pred.2.2
    ll.pred.2.2.list[index.test] <- ll.pred.2.2
    ul.pred.2.2.list[index.test] <- ul.pred.2.2
    sd.pred.2.2.list[index.test] <- sd.pred.2.2
    
    mean.pred.3.1.list[index.test] <- mean.pred.3.1
    ll.pred.3.1.list[index.test] <- ll.pred.3.1
    ul.pred.3.1.list[index.test] <- ul.pred.3.1
    sd.pred.3.1.list[index.test] <- sd.pred.3.1
    
    mean.pred.3.2.list[index.test] <- mean.pred.3.2
    ll.pred.3.2.list[index.test] <- ll.pred.3.2
    ul.pred.3.2.list[index.test] <- ul.pred.3.2
    sd.pred.3.2.list[index.test] <- sd.pred.3.2
    
    mean.pred.4.1.list[index.test] <- mean.pred.4.1
    ll.pred.4.1.list[index.test] <- ll.pred.4.1
    ul.pred.4.1.list[index.test] <- ul.pred.4.1
    sd.pred.4.1.list[index.test] <- sd.pred.4.1
    
    mean.pred.4.2.list[index.test] <- mean.pred.4.2
    ll.pred.4.2.list[index.test] <- ll.pred.4.2
    ul.pred.4.2.list[index.test] <- ul.pred.4.2
    sd.pred.4.2.list[index.test] <- sd.pred.4.2
    
    
    cv.list[index.test] <- rep(i, n.test)
    Date.group.list[index.test] <- Date.group.test
    print(paste0("iteration", i)) 
  }
  # Output -----------------------------------------------------------------------
  NULL %>%
    bind_cols(df.7days[, c("AQS_Site_id", "PM_AQS", "PM25_TOT_NCAR", "Date")]) %>%
    bind_cols(data.frame(
      cv.list, Date.group.list,
      mean.pred.1.1.list, mean.pred.1.2.list, mean.pred.2.1.list, mean.pred.2.2.list,
      mean.pred.3.1.list, mean.pred.3.2.list, mean.pred.4.1.list, mean.pred.4.2.list,
      sd.pred.1.1.list, sd.pred.1.2.list, sd.pred.2.1.list, sd.pred.2.2.list,
      sd.pred.3.1.list, sd.pred.3.2.list, sd.pred.4.1.list, sd.pred.4.2.list,
      ll.pred.1.1.list, ll.pred.1.2.list, ll.pred.2.1.list, ll.pred.2.2.list,
      ll.pred.3.1.list, ll.pred.3.2.list, ll.pred.4.1.list, ll.pred.4.2.list,
      ul.pred.1.1.list, ul.pred.1.2.list, ul.pred.2.1.list, ul.pred.2.2.list,
      ul.pred.3.1.list, ul.pred.3.2.list, ul.pred.4.1.list, ul.pred.4.2.list)) -> result.full.7days
  
  result.full <- bind_rows(result.full, result.full.7days)
}









write.csv(result.full.7days, "result.full.7days.csv", row.names = F)
