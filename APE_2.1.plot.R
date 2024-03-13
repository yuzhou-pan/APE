library(tidyverse)
library(gridExtra)
library(latex2exp)

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


grid.pred %>%
  mutate(quarter = case_when(
    (date >= "2018-01-01" & date <= "2018-03-31") ~ "Q1",
    (date >= "2018-04-01" & date <= "2018-06-30") ~ "Q2",
    (date >= "2018-07-01" & date <= "2018-09-30") ~ "Q3",
    (date >= "2018-10-01" & date <= "2018-12-31") ~ "Q4"
  )) %>%
  mutate(week = (Date.group - 1) %/% 7 + 1) -> grid.pred

################################################################################
# plot by quarters

grid.pred %>%
  mutate(pred_length = 2 * 1.96 * sqrt(pred_sd^2 + pred_sigma_sq)) %>%
  group_by(quarter, X_Grid_km, Y_Grid_km) %>%
  summarise(simulation = mean(PM25_TOT_NCAR),
            pred = mean(pred_mean),
            diff_sim_pred = mean(pred_mean - simulation),
            sd = mean(pred_sd),
            the_length = mean(pred_length)) %>%
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
      low = "blue", high = "orange",
      limits = c(0, 40)
    ) +
    labs(title = "simulation", x = "Latitude", y = "Longitude") +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank())-> the.plot.1
  
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
      colors = c("darkblue", "blue", "white", "orange"),
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
         device = "jpeg", width = 10, height = 7.5)
}

################################################################################

ggplot(grid.pred.by.quarter) +
  geom_tile(aes(x = X_Grid_km, y = Y_Grid_km, fill = simulation), width = 12, height = 12) +
  coord_fixed(ratio = 1) +
  facet_wrap(~quarter) +
  scale_fill_gradient(
    low = "blue", high = "orange",
    name = "PM2.5",
    limits = c(0, 40)
  ) +
  labs(title = "NCAR Simulation", x = "Latitude", y = "Longitude") +
  theme_bw() -> simulation.plot
ggsave(plot = simulation.plot, 
       filename = "figures/simulation.jpg", 
       device = "jpeg", width = 8, height = 6)

ggplot(grid.pred.by.quarter) +
  geom_tile(aes(x = X_Grid_km, y = Y_Grid_km, fill = pred), width = 12, height = 12) +
  coord_fixed(ratio = 1) +
  facet_wrap(~quarter) +
  scale_fill_gradient(
    low = "blue", high = "orange",
    name = "PM2.5",
    limits = c(0, 40)
  ) +
  labs(title = "Prediction", x = "Latitude", y = "Longitude") +
  theme_bw() -> pred.plot
ggsave(plot = pred.plot, 
       filename = "figures/pred.jpg", 
       device = "jpeg", width = 8, height = 6)

ggplot(grid.pred.by.quarter) +
  geom_tile(aes(x = X_Grid_km, y = Y_Grid_km, fill = diff_sim_pred), width = 12, height = 12) +
  coord_fixed(ratio = 1) +
  facet_wrap(~quarter) +
  scale_fill_gradientn(
    colors = c("darkblue", "blue", "white", "orange"),
    values = scales::rescale(c(-12, -4, 0, 4)),
    limits = c(-12, 4),
    name = "PM2.5"
  ) +
  labs(title = "Prediction - Simulation", x = "Latitude", y = "Longitude") +
  theme_bw() -> diff.plot
ggsave(plot = diff.plot, 
       filename = "figures/diff.jpg", 
       device = "jpeg", width = 8, height = 6)

ggplot(grid.pred.by.quarter) +
  geom_tile(aes(x = X_Grid_km, y = Y_Grid_km, fill = sd), width = 12, height = 12) +
  coord_fixed(ratio = 1) +
  facet_wrap(~quarter) +
  scale_fill_gradient(
    low = "white", high = "black",
    name = "PM2.5"
  ) +
  labs(title = "Standard Deviation", x = "Latitude", y = "Longitude") +
  theme_bw() -> sd.plot
ggsave(plot = sd.plot, 
       filename = "figures/sd.jpg", 
       device = "jpeg", width = 8, height = 6)

ggplot(grid.pred.by.quarter) +
  geom_tile(aes(x = X_Grid_km, y = Y_Grid_km, fill = the_length), width = 12, height = 12) +
  coord_fixed(ratio = 1) +
  facet_wrap(~quarter) +
  scale_fill_gradient(
    low = "blue", high = "orange",
    name = "PM2.5"
  ) +
  labs(title = "95% Prediction Interval Length", x = "Latitude", y = "Longitude") +
  theme_bw() -> length.plot
ggsave(plot = length.plot, 
       filename = "figures/length.jpg", 
       device = "jpeg", width = 8, height = 6)

################################################################################

################################################################################
fixed <- NULL
hyperpar <- NULL
for (i in c(1:13)){
  fixed <- bind_rows(fixed, read.csv(paste0("summary.fixed.2.1_", i, ".csv"), header = T))
  hyperpar <- bind_rows(hyperpar, read.csv(paste0("summary.hyperpar.2.1_", i, ".csv"), header = T))
}

fixed[(c(1: 52) * 2 - 1), ] %>% mutate(week = factor(row_number())) -> beta0
fixed[(c(1: 52) * 2), ] %>% mutate(week = factor(row_number())) -> beta1

hyperpar[(c(0: 51) * 3 + 2), ] %>% mutate(week = factor(row_number())) -> rho
hyperpar[(c(0: 51) * 3 + 3), ] %>% mutate(week = factor(row_number())) -> sigma

#beta0$var <- "beta0"
#beta1$var <- "beta1"
#beta <- bind_rows(beta0, beta1)

#ggplot(beta, aes(x = week, y = X0.5quant)) +
#  geom_errorbar(aes(ymin = X0.025quant, ymax = X0.975quant), width = 0.3) +
#  facet_wrap(~var, nrow = 2, scales = "free") +
#  theme_bw()

ggplot(beta0, aes(x = week, y = X0.5quant)) +
  geom_errorbar(aes(ymin = X0.025quant, ymax = X0.975quant), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(y = TeX("$\\beta_0$")) +
  theme_bw() -> beta0.plot
ggsave(plot = beta0.plot,
       filename = "../figures/beta0.jpg", 
       device = "jpeg", width = 16, height = 6)

ggplot(beta1, aes(x = week, y = X0.5quant)) +
  geom_errorbar(aes(ymin = X0.025quant, ymax = X0.975quant), width = 0.3) +
  labs(y = TeX("$\\beta_1$")) +
  theme_bw() -> beta1.plot
ggsave(plot = beta1.plot,
       filename = "../figures/beta1.jpg", 
       device = "jpeg", width = 16, height = 6)

ggplot(rho, aes(x = week, y = X0.5quant)) +
  geom_errorbar(aes(ymin = X0.025quant, ymax = X0.975quant), width = 0.5) +
  labs(y = TeX("$\\rho$")) +
  theme_bw() -> rho.plot
ggsave(plot = rho.plot,
       filename = "../figures/rho.jpg", 
       device = "jpeg", width = 16, height = 6)

ggplot(sigma, aes(x = week, y = X0.5quant)) +
  geom_errorbar(aes(ymin = X0.025quant, ymax = X0.975quant), width = 0.5) +
  labs(y = TeX("$\\sigma$")) +
  theme_bw() -> sigma.plot
ggsave(plot = sigma.plot,
       filename = "../figures/sigma.jpg", 
       device = "jpeg", width = 16, height = 6)
