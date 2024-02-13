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
  
  points <- data.matrix(cbind(df.7days$X_AQS_km, 
                              df.7days$Y_AQS_km))
  
  grid.pts <- data.matrix(cbind(df.7days$X_Grid_km, 
                                df.7days$Y_Grid_km))
  
  # define domain (boundary) for mesh construction
  domain <- inla.nonconvex.hull(points, concave = -0.07, convex = -0.05, resolution = c(100, 100))
  
  ## initialization --------------------------------------------------------------
  
  n.total <- nrow(points)
  base.size <- n.total %/% 10
  row.num.list <- sample(n.total)
  
  loc <- unique(points)
  loc.n.total <- nrow(loc)
  loc.base.size <- loc.n.total %/% 10
  loc.row.num.list <- sample(loc.n.total)
  
  index <- NULL
  loc.index <- NULL
  
  for (i in c(1:10)){
    loc.index <- c(loc.index, list(sample(loc.row.num.list, loc.base.size)))
    loc.row.num.list <- loc.row.num.list[!(loc.row.num.list %in% loc.index[[i]])]
  }
  
  idx <- 1
  for (r in loc.row.num.list){
    loc.index[[idx]] <- c(loc.index[[idx]], r)
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
  
  radius <- 200
  for (i in c(1:10)){
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    loc.test <- loc[unlist(loc.index[i]), ]
    loc.test.df <- as.data.frame(loc.test) %>% mutate(row.num = row_number())
    points.df <- as.data.frame(points) %>% mutate(row.num = row_number())
    temp <- semi_join(points.df, loc.test.df, by = join_by(V1, V2))
    index.test <- temp$row.num
    
    df.test <- df.7days[index.test, ] # also as centers
    
    # figure out the interior set
    #df.interior <- NULL
    df.7days %>%
      rowwise() %>%
      filter(sum(((X_AQS_km-df.test$X_AQS_km)^2 + 
                    (Y_AQS_km-df.test$Y_AQS_km)^2) <= radius^2) > 0) -> df.interior
    
    setdiff(df.7days, df.interior) -> df.train
    
    #ggplot() +
    #  geom_sf(data = USA.contiguous.sf, fill = NA) +
    #  geom_point(aes(x = LON_AQS, y = LAT_AQS, color = "Train"), size = 1,
    #             data = df.train) +
    #  geom_point(aes(x = LON_AQS, y = LAT_AQS, color = "Test"), size = 1, 
    #             data = df.test)+
    #  labs(title = element_text(paste("Fold = ", i, ", Radius = ", radius, sep = ""))) +
    #  theme(legend.position = "none") +
    #  theme_minimal() -> plot.train
    #gridExtra::grid.arrange(plot.train)
    
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
    Date.group.train <- df.train$Date.group %% 7
    Date.group.train[Date.group.train == 0] <- 7
    Date.group.test <- df.test$Date.group %% 7
    Date.group.test[Date.group.test == 0] <- 7
    
    
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
                               n = 8,
                               max.edge = c(360, 720),
                               offset = c(600, 1200),
                               boundary = domain)
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
    #svc.3.1 <- ~ -1 + beta_0(geometry, model = spde) + beta_1(geometry, weights = PM25_TOT_NCAR)
    #svc.3.1.f <- PM_AQS ~ .
    
    #model.test <- inlabru::bru(svc.3.1, like(formula = svc.3.1.f), family = "gaussian", 
    #options = list(
    #   control.compute = list(waic = TRUE, cpo = FALSE),
    #   control.inla = list(int.strategy = "eb"),
    #   verbose = FALSE), 
    # data = df.train)
    
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

write.csv(result.full, paste0("result.full.", n, ".radius200.0212.csv"), row.names = F)

write.csv(summary.fixed.1.1, paste0("./summary/summary.fixed.1.1_", n, ".radius200.csv"), row.names = T)
write.csv(summary.fixed.1.2, paste0("./summary/summary.fixed.1.2_", n, ".radius200.csv"), row.names = T)
write.csv(summary.fixed.2.1, paste0("./summary/summary.fixed.2.1_", n, ".radius200.csv"), row.names = T)
write.csv(summary.fixed.2.2, paste0("./summary/summary.fixed.2.2_", n, ".radius200.csv"), row.names = T)
write.csv(summary.fixed.3.1, paste0("./summary/summary.fixed.3.1_", n, ".radius200.csv"), row.names = T)
write.csv(summary.fixed.3.2, paste0("./summary/summary.fixed.3.2_", n, ".radius200.csv"), row.names = T)
write.csv(summary.fixed.4.1, paste0("./summary/summary.fixed.4.1_", n, ".radius200.csv"), row.names = T)
write.csv(summary.fixed.4.2, paste0("./summary/summary.fixed.4.2_", n, ".radius200.csv"), row.names = T)

write.csv(summary.hyperpar.1.1, paste0("./summary/summary.hyperpar.1.1_", n, ".radius200.csv"), row.names = T)
write.csv(summary.hyperpar.1.2, paste0("./summary/summary.hyperpar.1.2_", n, ".radius200.csv"), row.names = T)
write.csv(summary.hyperpar.2.1, paste0("./summary/summary.hyperpar.2.1_", n, ".radius200.csv"), row.names = T)
write.csv(summary.hyperpar.2.2, paste0("./summary/summary.hyperpar.2.2_", n, ".radius200.csv"), row.names = T)
write.csv(summary.hyperpar.3.1, paste0("./summary/summary.hyperpar.3.1_", n, ".radius200.csv"), row.names = T)
write.csv(summary.hyperpar.3.2, paste0("./summary/summary.hyperpar.3.2_", n, ".radius200.csv"), row.names = T)
write.csv(summary.hyperpar.4.1, paste0("./summary/summary.hyperpar.4.1_", n, ".radius200.csv"), row.names = T)
write.csv(summary.hyperpar.4.2, paste0("./summary/summary.hyperpar.4.2_", n, ".radius200.csv"), row.names = T)


t.1 <- Sys.time()
print(t.1)

print(t.1 - t.0)
