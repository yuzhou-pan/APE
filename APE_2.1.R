#################################################################################
# Purpose: run our final model 2.1 with the entire dataset predicting to all    #
#          grid locations. Outputs include model summaries and the prediction   #
#          statistics.                                                          #
# Notes: 1. This script was run on HPC Cluster using a slurm array from 1 to 13.#
#################################################################################

t.0 <- Sys.time()
print(t.0)
# Packages ---------------------------------------------------------------------

library(INLA)
library(haven)
library(knitr)
library(tidyverse)
library(sf)
#!/usr/bin/env Rscript

# grab the array id value from the environment variable passed from sbatch
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')

# coerce the value to an integer
n <- as.numeric(slurm_arrayid)

#if (n == 1){
#  inla.binary.install(os = "CentOS Linux-7")
#}

#data(World)
#USA.sf <- World[World$iso_a3 == "USA", ]$geometry
#USA.contiguous.sf <- st_cast(USA.sf, "POLYGON")[6]

#sessionInfo()


## divide 365 days into multiple windows ---------------------------------------

# there is no PM measurement dated "2018-12-31"
# > sum(df$Date == "2018-12-31")
# [1] 0
# > sum(df$Date == "2018-12-30")
# [1] 569

# initialization of summary of model coefficients
summary.hyperpar.2.1 <- NULL

summary.fixed.2.1 <- NULL

num.weeks <- 365 %/% 7
num.days <- 7
range.Date.group <- NULL

set.seed(123456)
result.full <- NULL
grid.pred <- NULL

range.weeks <- c(((n-1)*4+1): (n*4))
for (w in range.weeks){
  range.Date.group <- c(((w-1)*7+1): (w*7))
  df.7days <- read.csv(paste0("df.7days.", w, ".csv"), header = T)
  grid.7days <- read.csv(paste0("grid.7days.", w, ".csv"), header = T)
  grid.7days.copy <- grid.7days
  
  print("Here!!!!")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  n.grid <- nrow(grid.7days)
  
  # points
  points.est <- data.matrix(cbind(df.7days$X_AQS_km, 
                                  df.7days$Y_AQS_km))
  points.pred <- data.matrix(cbind(grid.7days$X_Grid_km, 
                                   grid.7days$Y_Grid_km))
  
  # define domain (boundary) for mesh construction
  domain <- inla.nonconvex.hull(points.est, concave = -0.07, convex = -0.05, resolution = c(100, 100))
  
  # y
  PM_AQS.est <- df.7days$PM_AQS
  PM_AQS.pred <- rep(NA, n.grid)
  
  # x
  PM25_TOT_NCAR.est <- df.7days$PM25_TOT_NCAR
  PM25_TOT_NCAR.pred <- grid.7days$PM25_TOT_NCAR
  
  # Dates
  Date.group.est <- df.7days$Date.group %% 7
  Date.group.est[Date.group.est == 0] <- 7
  Date.group.pred <- grid.7days$Date.group %% 7
  Date.group.pred[Date.group.pred == 0] <- 7
  
  
  # Diag
  diag.est <- Diagonal(length(PM25_TOT_NCAR.est), PM25_TOT_NCAR.est)
  diag.pred <- Diagonal(length(PM25_TOT_NCAR.pred), PM25_TOT_NCAR.pred)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  print("before 1")
  print(paste0("df.7days dimensions: ", dim(df.7days)))
  print(paste0("grid.7days dimensions: ", dim(grid.7days)))
  ### 1. Define mesh -----------------------------------------------------------
  mesh.train <- inla.mesh.2d(loc = points.est,
                             cutoff = 12,
                             n = 8,
                             max.edge = c(360, 720),
                             offset = c(600, 1200),
                             boundary = domain)
  print("after 1")
  ## 2. Get A and Ap -----------------------------------------------------------
  A.s <- inla.spde.make.A(mesh = mesh.train, loc = points.est)
  Ap.s <- inla.spde.make.A(mesh = mesh.train, loc = points.pred)
  
  A.st <- inla.spde.make.A(mesh = mesh.train, loc = points.est, group = Date.group.est)
  Ap.st <- inla.spde.make.A(mesh = mesh.train, loc = points.pred, group = Date.group.pred)
  
  A.s.cov <- inla.spde.make.A(mesh = mesh.train, loc = points.est)
  Ap.s.cov <- inla.spde.make.A(mesh = mesh.train, loc = points.pred)
  
  A.st.cov <- inla.spde.make.A(mesh = mesh.train, loc = points.est, group = Date.group.est)
  Ap.st.cov <- inla.spde.make.A(mesh = mesh.train, loc = points.pred, group = Date.group.pred)
  
  print("after 2")
  ## 3. Get spde ---------------------------------------------------------------
  spde <- inla.spde2.pcmatern(mesh.train, alpha = 2, constr = F,
                              prior.range = c(20, 0.05), 
                              prior.sigma = c(22, 0.01))
  #spde <- inla.spde2.matern(mesh.train, alpha = 2, constr = F)
  print("after 3")
  ## 4. Get spde index ---------------------------------------------------------
  mesh.index.s <- inla.spde.make.index(name = "s", n.spde = spde$n.spde)
  mesh.index.s.cov <- inla.spde.make.index(name = "s.cov", n.spde = spde$n.spde)
  mesh.index.st <- inla.spde.make.index(name = "st", n.spde = spde$n.spde, n.group = 7)
  mesh.index.st.cov <- inla.spde.make.index(name = "st.cov", n.spde = spde$n.spde, n.group = 7)
  print("after 4")
  ## 5. Stack for estimation and prediction ------------------------------------

  # 2.1  
  stk.e.2.1 <- inla.stack(tag = "est",
                          data = list(PM_AQS = PM_AQS.est),
                          A = list(1, A.s),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.est),
                                         s = mesh.index.s))
  stk.p.2.1 <- inla.stack(tag = "pred",
                          data = list(PM_AQS = NA),
                          A = list(1, Ap.s),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.pred), 
                                         s = mesh.index.s))
  
  print("after 5")
  ## 6. Combine stacks ---------------------------------------------------------
  stk.full.2.1 <- inla.stack(stk.e.2.1, stk.p.2.1)
  print("after 6")
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # formula
  f.2.1 <- PM_AQS ~ 0 + b0 + PM25_TOT_NCAR + f(s, model = spde, constr = F)
  
  print("after formula")
  # model
  ####### model spec ######################
  model.2.1 <- inla(f.2.1, data = inla.stack.data(stk.full.2.1),
                    control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.2.1)),
                    #control.mode = list(theta = c(0, 3, -6)),
                    control.compute = list(openmp.strategy="huge"), num.threads = 1)
  print("after model.2.1")
  print("after model")
  
  # get summary coefficients
  temp <- model.2.1$summary.fixed
  rownames(temp) <- paste0(rownames(temp), "_", w)
  summary.fixed.2.1 <- bind_rows(summary.fixed.2.1, temp)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  temp <- model.2.1$summary.hyperpar
  rownames(temp) <- paste0(rownames(temp), "_", w)
  summary.hyperpar.2.1 <- bind_rows(summary.hyperpar.2.1, temp)
  
  # add pred to original dataset  ----------------------------------------------
  index <- inla.stack.index(stk.full.2.1, tag = "pred")$data
  
  pred_mean <- model.2.1$summary.fitted.values[index, "mean"]
  pred_ll <- model.2.1$summary.fitted.values[index, "0.025quant"]
  pred_ul <- model.2.1$summary.fitted.values[index, "0.975quant"]
  pred_sd <- model.2.1$summary.fitted.values[index, "sd"]
  pred_sigma_sq <- (model.2.1$summary.hyperpar$mode[1])^(-1)
  
  grid.7days.copy %>%
    select(one_of("X_Grid_km", "Y_Grid_km", "date", "Date.group", "PM25_TOT_NCAR")) %>%
    bind_cols(data.frame(pred_mean, pred_ll, pred_ul, pred_sd)) %>%
    mutate(pred_sigma_sq = pred_sigma_sq) -> grid.7days.copy
  
  grid.pred <- bind_rows(grid.pred, grid.7days.copy)

}

write.csv(summary.fixed.2.1, paste0("./summary.all/summary.fixed.2.1_", n, ".0303.csv"), row.names = T)
write.csv(summary.hyperpar.2.1, paste0("./summary.all/summary.hyperpar.2.1_", n, ".0303.csv"), row.names = T)
write.csv(grid.pred, paste0("./pred.all/grid.pred_", n, ".csv"), row.names = F)



t.1 <- Sys.time()
print(t.1)

print(t.1 - t.0)
