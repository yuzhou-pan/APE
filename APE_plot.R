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

n <- 1

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
summary.hyperpar.1.1 <- NULL
summary.hyperpar.1.2 <- NULL
summary.hyperpar.2.1 <- NULL
summary.hyperpar.2.2 <- NULL
summary.hyperpar.3.1 <- NULL
summary.hyperpar.3.2 <- NULL
summary.hyperpar.4.1 <- NULL
summary.hyperpar.4.2 <- NULL

summary.fixed.1.1 <- NULL
summary.fixed.1.2 <- NULL
summary.fixed.2.1 <- NULL
summary.fixed.2.2 <- NULL
summary.fixed.3.1 <- NULL
summary.fixed.3.2 <- NULL
summary.fixed.4.1 <- NULL
summary.fixed.4.2 <- NULL

num.weeks <- 365 %/% 7
num.days <- 7
range.Date.group <- NULL

set.seed(123456)
result.full <- NULL

range.weeks <- c(((n-1)*4+1): (n*4))
for (w in range.weeks){
  range.Date.group <- c(((w-1)*7+1): (w*7))
  df.7days <- read.csv(paste0("df.7days.", w, ".csv"), header = T)
  grid.7days <- read.csv(paste0("grid.7days.", w, ".csv"), header = T)
    
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
                          data = list(PM_AQS = PM_AQS.est),
                          A = list(1, A.s),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.est),
                                         s = mesh.index.s))
  stk.p.1.1 <- inla.stack(tag = "pred",
                          data = list(PM_AQS = NA),
                          A = list(1, Ap.s),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.pred),
                                         s = mesh.index.s))
  #print("here")
    
  # 1.2
  stk.e.1.2 <- inla.stack(tag = "est",
                          data = list(PM_AQS = PM_AQS.est),
                          A = list(1, A.st),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.est),
                                         st = mesh.index.st))
  stk.p.1.2 <- inla.stack(tag = "pred",
                          data = list(PM_AQS = NA),
                          A = list(1, Ap.st),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.pred),
                                         st = mesh.index.st))
    
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
  # 2.2
  stk.e.2.2 <- inla.stack(tag = "est",
                          data = list(PM_AQS = PM_AQS.est),
                          A = list(1, A.st),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.est),
                                         st = mesh.index.st))
  stk.p.2.2 <- inla.stack(tag = "pred",
                          data = list(PM_AQS = NA),
                          A = list(1, Ap.st),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.pred), 
                                         st = mesh.index.st))
  # 3.1
  stk.e.3.1 <- inla.stack(tag = "est",
                          data = list(PM_AQS = PM_AQS.est),
                          A = list(1, A.s, diag.est %*% A.s.cov),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.est),
                                         s = mesh.index.s,
                                         s.cov = mesh.index.s.cov))
  stk.p.3.1 <- inla.stack(tag = "pred",
                          data = list(PM_AQS = NA),
                          A = list(1, Ap.s, diag.pred %*% Ap.s.cov),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.pred),
                                         s = mesh.index.s,
                                         s.cov = mesh.index.s.cov))
  # 3.2
  stk.e.3.2 <- inla.stack(tag = "est",
                          data = list(PM_AQS = PM_AQS.est),
                          A = list(1, A.st, diag.est %*% A.s.cov),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.est),
                                         st = mesh.index.st,
                                         s.cov = mesh.index.s.cov))
  stk.p.3.2 <- inla.stack(tag = "pred",
                          data = list(PM_AQS = NA),
                          A = list(1, Ap.st, diag.pred %*% Ap.s.cov),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.pred),
                                         st = mesh.index.st,
                                         s.cov = mesh.index.s.cov))
  # 4.1
  stk.e.4.1 <- inla.stack(tag = "est",
                          data = list(PM_AQS = PM_AQS.est),
                          A = list(1, A.s, diag.est %*% A.st.cov),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.est),
                                         s = mesh.index.s,
                                         st.cov = mesh.index.st.cov))
  stk.p.4.1 <- inla.stack(tag = "pred",
                          data = list(PM_AQS = NA),
                          A = list(1, Ap.s, diag.pred %*% Ap.st.cov),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.pred),
                                         s = mesh.index.s,
                                         st.cov = mesh.index.st.cov))
  # 4.2
  stk.e.4.2 <- inla.stack(tag = "est",
                          data = list(PM_AQS = PM_AQS.est),
                          A = list(1, A.st, diag.est %*% A.st.cov),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.est),
                                         st = mesh.index.st,
                                         st.cov = mesh.index.st.cov))
  stk.p.4.2 <- inla.stack(tag = "pred",
                          data = list(PM_AQS = NA),
                          A = list(1, Ap.st, diag.pred %*% Ap.st.cov),
                          effects = list(data.frame(b0 = 1,
                                                    PM25_TOT_NCAR = PM25_TOT_NCAR.pred),
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
                    control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.1.1)),
                    control.mode = list(theta = c(0, 3, -6)), 
                    control.compute = list(openmp.strategy="huge"), num.threads = 1)
    
  print("after model.1.1")
    
  model.1.2 <- inla(f.1.2, data = inla.stack.data(stk.full.1.2),
                    control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.1.2)),
                    control.mode = list(theta = c(0.8, 3, -6, 0.8)), 
                    control.compute = list(openmp.strategy="huge"), num.threads = 1)
  print("after model.1.2")
    
  model.2.1 <- inla(f.2.1, data = inla.stack.data(stk.full.2.1),
                    control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.2.1)),
                    control.mode = list(theta = c(0, 3, -6)),
                    control.compute = list(openmp.strategy="huge"), num.threads = 1)
  print("after model.2.1")
    
  model.2.2 <- inla(f.2.2, data = inla.stack.data(stk.full.2.2),
                    control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.2.2)),
                    control.mode = list(theta = c(0.8, 3, -6, 0.8)), 
                    control.compute = list(openmp.strategy="huge"), num.threads = 1)
  print("after model.2.2")
    
  model.3.1 <- inla(f.3.1, data = inla.stack.data(stk.full.3.1),
                    control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.3.1)),
                    control.mode = list(theta = c(0, 6, -6, 3, -6)), 
                    control.compute = list(openmp.strategy="huge"), num.threads = 1)
  print("after model.3.1")
    
  model.3.2 <- inla(f.3.2, data = inla.stack.data(stk.full.3.2),
                    control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.3.2)),
                    control.mode = list(theta = c(0.8, 6, -6, 3, -6, 0.8)),
                    control.compute = list(openmp.strategy="huge"), num.threads = 1)
  print("after model.3.2")
    
  model.4.1 <- inla(f.4.1, data = inla.stack.data(stk.full.4.1),
                    control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.4.1)),
                    control.mode = list(theta = c(0, 6, -6, 1, 3, -6)),
                    control.compute = list(openmp.strategy="huge"), num.threads = 1)
  print("after model.4.1")
    
  model.4.2 <- inla(f.4.2, data = inla.stack.data(stk.full.4.2),
                    control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.4.2)),
                    control.mode = list(theta = c(0.8, 6, -6, 1, 3, -6, 0.8)),
                    control.compute = list(openmp.strategy="huge"), num.threads = 1)
  print("after model.4.2")
  print("after model")

  # get summary coefficients
  temp <- model.1.1$summary.fixed
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.fixed.1.1 <- bind_rows(summary.fixed.1.1, temp)
    
  temp <- model.1.2$summary.fixed
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.fixed.1.2 <- bind_rows(summary.fixed.1.2, temp)
    
  temp <- model.2.1$summary.fixed
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.fixed.2.1 <- bind_rows(summary.fixed.2.1, temp)
    
  temp <- model.2.2$summary.fixed
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.fixed.2.2 <- bind_rows(summary.fixed.2.2, temp)
    
  temp <- model.3.1$summary.fixed
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.fixed.3.1 <- bind_rows(summary.fixed.3.1, temp)
    
  temp <- model.3.2$summary.fixed
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.fixed.3.2 <- bind_rows(summary.fixed.3.2, temp)
    
  temp <- model.4.1$summary.fixed
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.fixed.4.1 <- bind_rows(summary.fixed.4.1, temp)
    
  temp <- model.4.2$summary.fixed
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.fixed.4.2 <- bind_rows(summary.fixed.4.2, temp)
    
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  temp <- model.1.1$summary.hyperpar
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.hyperpar.1.1 <- bind_rows(summary.hyperpar.1.1, temp)
    
  temp <- model.1.2$summary.hyperpar
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.hyperpar.1.2 <- bind_rows(summary.hyperpar.1.2, temp)
    
  temp <- model.2.1$summary.hyperpar
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.hyperpar.2.1 <- bind_rows(summary.hyperpar.2.1, temp)
    
  temp <- model.2.2$summary.hyperpar
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.hyperpar.2.2 <- bind_rows(summary.hyperpar.2.2, temp)
    
  temp <- model.3.1$summary.hyperpar
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.hyperpar.3.1 <- bind_rows(summary.hyperpar.3.1, temp)
    
  temp <- model.3.2$summary.hyperpar
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.hyperpar.3.2 <- bind_rows(summary.hyperpar.3.2, temp)
    
  temp <- model.4.1$summary.hyperpar
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.hyperpar.4.1 <- bind_rows(summary.hyperpar.4.1, temp)
    
  temp <- model.4.2$summary.hyperpar
  rownames(temp) <- paste0(rownames(temp), "_", w, "_", i)
  summary.hyperpar.4.2 <- bind_rows(summary.hyperpar.4.2, temp)
  
  # plot -----------------------------------------------------------------------
  index <- inla.stack.index(stk.full.2.1, tag = "pred")$data
  
  pred_mean <- model.2.1$summary.fitted.values[index, "mean"]
  pred_ll <- model.2.1$summary.fitted.values[index, "0.025quant"]
  pred_ul <- model.2.1$summary.fitted.values[index, "0.975quant"]
  
  df <- rbind(
    data.frame(
      lat = points.pred[, 1], lon = points.pred[, 2], date = Date.group.pred,
      value = pred_mean, variable = "Prediction (Mean)"
    ),
    data.frame(
      lat = points.pred[, 1], lon = points.pred[, 2], date = Date.group.pred,
      value = pred_ll, variable = "Prediction (Lower Level)"
    ),
    data.frame(
      lat = points.pred[, 1], lon = points.pred[, 2], date = Date.group.pred,
      value = pred_ul, variable = "Prediction (Upper Level)"
    ),
    data.frame(
      lat = points.pred[, 1], lon = points.pred[, 2], date = Date.group.pred,
      value = grid.7days$PM25_TOT_NCAR, variable = "NCAR Simulation"
    )
  )
  df$variable <- as.factor(df$variable)
  
  for (d in c(1:7)){
    days <- (w - 1) * 7 + d
    the.date <- as.Date("2018-01-01") + days - 1
  
    ggplot(data = df[df$date == d, ]) + 
      geom_tile(aes(x = lat, y = lon, fill = value), width = 12, height = 12) +
      facet_wrap(~variable, nrow = 2) +
      coord_fixed(ratio = 1) +
      scale_fill_gradient(
        name = "PM2.5",
        low = "blue", high = "orange"
      ) +
      labs(title = paste0("PM 2.5 Simulation/Prediction on Grid on ", the.date),
           x = "Latitude", 
           y = "Longitude") +
      theme_bw() -> the.plot
    ggsave(plot = the.plot, 
           filename = paste0("figures/plot.model.2.1.", the.date, ".jpg"), 
           device = "jpeg", width = 8, height = 6)
  }
}

t.1 <- Sys.time()
print(t.1)

print(t.1 - t.0)
