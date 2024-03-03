library(tidyverse)
library(gridExtra)

grid.pred.1 <- read.csv("grid.pred_1.csv", header = T)
grid.pred.2 <- read.csv("grid.pred_2.csv", header = T)
grid.pred.3 <- read.csv("grid.pred_3.csv", header = T)
grid.pred.4 <- read.csv("grid.pred_4.csv", header = T)
grid.pred.5 <- read.csv("grid.pred_5.csv", header = T)
grid.pred.6 <- read.csv("grid.pred_6.csv", header = T)
grid.pred.7 <- read.csv("grid.pred_7.csv", header = T)
grid.pred.8 <- read.csv("grid.pred_8.csv", header = T)
grid.pred.9 <- read.csv("grid.pred_9.csv", header = T)
grid.pred.10 <- read.csv("grid.pred_10.csv", header = T)
grid.pred.11 <- read.csv("grid.pred_11.csv", header = T)
grid.pred.12 <- read.csv("grid.pred_12.csv", header = T)
grid.pred.13 <- read.csv("grid.pred_13.csv", header = T)

grid.pred <- bind_rows(grid.pred.1, grid.pred.2) %>%
  bind_rows(grid.pred.3) %>%
  bind_rows(grid.pred.4) %>%
  bind_rows(grid.pred.5) %>%
  bind_rows(grid.pred.6) %>%
  bind_rows(grid.pred.7) %>%
  bind_rows(grid.pred.8) %>%
  bind_rows(grid.pred.9) %>%
  bind_rows(grid.pred.10) %>%
  bind_rows(grid.pred.11) %>%
  bind_rows(grid.pred.12) %>%
  bind_rows(grid.pred.13)

# Q1: 2018-01-01, 2018-03-31
# Q2: 2018-04-01, 2018-06-30
# Q3: 2018-07-01, 2018-09-30
# Q4: 2018-10-01, 2018-12-31

# plot by quarters
grid.pred %>%
  mutate(quarter = case_when(
    (date >= "2018-01-01" & date <= "2018-03-31") ~ "Q1",
    (date >= "2018-04-01" & date <= "2018-06-30") ~ "Q2",
    (date >= "2018-07-01" & date <= "2018-09-30") ~ "Q3",
    (date >= "2018-10-01" & date <= "2018-12-31") ~ "Q4"
  )) %>%
  mutate(week = (Date.group - 1) %/% 7 + 1) -> grid.pred

grid.pred %>%
  group_by(quarter, X_Grid_km, Y_Grid_km) %>%
  summarise(simulation = mean(PM25_TOT_NCAR),
            pred = mean(pred_mean),
            diff_sim_pred = mean(pred_mean - simulation),
            sd = mean(pred_sd)) %>%
  ungroup() -> grid.pred.by.quarter

for (q in c("Q1", "Q2", "Q3", "Q4")){
  df <- grid.pred.by.quarter[grid.pred.by.quarter$quarter == q, ]
  df.1 <- df %>% select(one_of(c("X_Grid_km", "Y_Grid_km", "simulation")))
  df.2 <- df %>% select(one_of(c("X_Grid_km", "Y_Grid_km", "pred")))
  df.3 <- df %>% select(one_of(c("X_Grid_km", "Y_Grid_km", "diff_sim_pred")))
  df.4 <- df %>% select(one_of(c("X_Grid_km", "Y_Grid_km", "sd")))
  
  print(summary(df.4$sd))
  
  ggplot(df.1) +
    geom_tile(aes(x = X_Grid_km, y = Y_Grid_km, fill = simulation), width = 12, height = 12) +
    coord_fixed(ratio = 1) +
    scale_fill_gradient(
      low = "blue", high = "yellow",
      limits = c(0, 40)
    ) +
    labs(title = "simulation", x = "Latitude", y = "Longitude") +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) -> the.plot.1
  
  ggplot(df.2) +
    geom_tile(aes(x = X_Grid_km, y = Y_Grid_km, fill = pred), width = 12, height = 12) +
    coord_fixed(ratio = 1) +
    scale_fill_gradient(
      low = "blue", high = "orange",
      limits = c(0, 40)
    ) +
    labs(title = "prediction", x = "Latitude", y = "Longitude") +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) -> the.plot.2
  
  ggplot(df.3) +
    geom_tile(aes(x = X_Grid_km, y = Y_Grid_km, fill = diff_sim_pred), width = 12, height = 12) +
    coord_fixed(ratio = 1) +
    scale_fill_gradientn(
      #low = "green", mid = "white", high = "red",
      #limits = c(-4, 4), midpoint = 0
      colors = c("darkgreen", "green", "white", "red"),
      values = scales::rescale(c(-12, -4, 0, 4)),
      limits = c(-12, 4)
    ) +
    labs(title = "prediction - simulation", x = "Latitude", y = "Longitude") +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) -> the.plot.3
  
  ggplot(df.4) +
    geom_tile(aes(x = X_Grid_km, y = Y_Grid_km, fill = sd), width = 12, height = 12) +
    coord_fixed(ratio = 1) +
    scale_fill_gradient(
      low = "white", high = "black",
      limits = c(0, 0.086)
    ) +
    labs(title = "standard deviation", x = "Latitude", y = "Longitude") +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) -> the.plot.4
  
  the.plot <- grid.arrange(the.plot.1, the.plot.2, the.plot.3, the.plot.4, ncol = 2)
  
  
  ggsave(plot = the.plot, 
         filename = paste0("figures/plot.", q, ".jpg"), 
         device = "jpeg", width = 8, height = 6)
}
