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
#slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')

# coerce the value to an integer
#n <- as.numeric(slurm_arrayid)

#if (n == 1){
#  inla.binary.install(os = "CentOS Linux-7")
#}


#sessionInfo()


## divide 365 days into multiple windows ---------------------------------------

# there is no PM measurement dated "2018-12-31"
# > sum(df$Date == "2018-12-31")
# [1] 0
# > sum(df$Date == "2018-12-30")
# [1] 569

for (n in c(1:13)){

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
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print("before 1")
    print(paste0("df.7days dimensions: ", dim(df.7days)))
    print(paste0("df.interior dimensions: ", dim(df.interior)))
    print(paste0("df.train dimemsions: ", dim(df.train)))
    print(paste0("points.train dimensions: ", dim(points.train)))
    print(paste0("Week: ", w, "; iteration: ",i))
    ### 1. Define mesh -----------------------------------------------------------
    domain <- inla.nonconvex.hull(points, concave = -0.07, convex = -0.05, resolution = c(100, 100))
    mesh.train <- inla.mesh.2d(loc = points.train,
                               cutoff = 12,
                               n = 8,
                               max.edge = c(360, 720),
                               offset = c(600, 1200),
                               boundary = domain,
                               plot.delay = 1)
    print("after 1")
    
    print(paste0("iteration", i)) 
  }
  print(paste0("Week ", w))
}

}

t.1 <- Sys.time()
print(t.1)

print(t.1 - t.0)
