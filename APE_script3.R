library(tidyverse)
library(huxtable)
library(openxlsx)

for (n in c(1:4)){
if (n == 1){
  file.ext <- ".csv"
  wb1 <- openxlsx::createWorkbook()
  wb2 <- openxlsx::createWorkbook()
  footnote <- "Radius 100km; sample by (location, date)"
} else if (n == 2) {
  file.ext <- ".nobuffer.csv"
  wb1 <- openxlsx::loadWorkbook("RMSE.by.quarter.xlsx")
  wb2 <- openxlsx::loadWorkbook("coverage.by.quarter.xlsx")
  footnote <- "No buffer / radius 0km; sample by location"
} else if (n == 3) {
  file.ext <- ".new.csv"
  wb1 <- openxlsx::loadWorkbook("RMSE.by.quarter.xlsx")
  wb2 <- openxlsx::loadWorkbook("coverage.by.quarter.xlsx")
  footnote <- "Radius 100km; sample by location"
} else {
  file.ext <- ".radius200.csv"
  wb1 <- openxlsx::loadWorkbook("RMSE.by.quarter.xlsx")
  wb2 <- openxlsx::loadWorkbook("coverage.by.quarter.xlsx")
  footnote <- "Radius 200km; sample by location"
}

result.full <- NULL

for (i in c(1:13)){
  temp <- read.csv(paste0("result.full.", i, file.ext))
  result.full <- bind_rows(result.full, temp)
}

result.full %>%
  mutate(Month = as.numeric(format(as.Date(Date), format = "%m")),
         Day = as.numeric(format(as.Date(Date), format = "%d"))) %>%
  mutate(Quarter = case_when(Month >= 1 & Month <= 3 ~ "Q1",
                             Month >= 4 & Month <= 6 ~ "Q2",
                             Month >= 7 & Month <= 9 ~ "Q3",
                             Month >= 10 & Month <= 12 ~ "Q4")) %>%
  mutate(cover.1.1 = (PM_AQS >= ll.pred.1.1.list & PM_AQS <= ul.pred.1.1.list),
         cover.1.2 = (PM_AQS >= ll.pred.1.2.list & PM_AQS <= ul.pred.1.2.list),
         cover.2.1 = (PM_AQS >= ll.pred.2.1.list & PM_AQS <= ul.pred.2.1.list),
         cover.2.2 = (PM_AQS >= ll.pred.2.2.list & PM_AQS <= ul.pred.2.2.list),
         cover.3.1 = (PM_AQS >= ll.pred.3.1.list & PM_AQS <= ul.pred.3.1.list),
         cover.3.2 = (PM_AQS >= ll.pred.3.2.list & PM_AQS <= ul.pred.3.2.list),
         cover.4.1 = (PM_AQS >= ll.pred.4.1.list & PM_AQS <= ul.pred.4.1.list),
         cover.4.2 = (PM_AQS >= ll.pred.4.2.list & PM_AQS <= ul.pred.4.2.list)) -> result.full

# RMSE ----------------------------------------------------------------------

result.full %>%
  group_by(Quarter) %>%
  summarise(RMSE.1.1 = sqrt(mean((PM_AQS - mean.pred.1.1.list) ^ 2)),
            RMSE.1.2 = sqrt(mean((PM_AQS - mean.pred.1.2.list) ^ 2)),
            RMSE.2.1 = sqrt(mean((PM_AQS - mean.pred.2.1.list) ^ 2)),
            RMSE.2.2 = sqrt(mean((PM_AQS - mean.pred.2.2.list) ^ 2)),
            RMSE.3.1 = sqrt(mean((PM_AQS - mean.pred.3.1.list) ^ 2)),
            RMSE.3.2 = sqrt(mean((PM_AQS - mean.pred.3.2.list) ^ 2)),
            RMSE.4.1 = sqrt(mean((PM_AQS - mean.pred.4.1.list) ^ 2)),
            RMSE.4.2 = sqrt(mean((PM_AQS - mean.pred.4.2.list) ^ 2))) -> tbl.quarter

result.full %>%
  summarise(RMSE.1.1 = sqrt(mean((PM_AQS - mean.pred.1.1.list) ^ 2)),
            RMSE.1.2 = sqrt(mean((PM_AQS - mean.pred.1.2.list) ^ 2)),
            RMSE.2.1 = sqrt(mean((PM_AQS - mean.pred.2.1.list) ^ 2)),
            RMSE.2.2 = sqrt(mean((PM_AQS - mean.pred.2.2.list) ^ 2)),
            RMSE.3.1 = sqrt(mean((PM_AQS - mean.pred.3.1.list) ^ 2)),
            RMSE.3.2 = sqrt(mean((PM_AQS - mean.pred.3.2.list) ^ 2)),
            RMSE.4.1 = sqrt(mean((PM_AQS - mean.pred.4.1.list) ^ 2)),
            RMSE.4.2 = sqrt(mean((PM_AQS - mean.pred.4.2.list) ^ 2))) -> tbl.overall

RMSE.quarter <- as.data.frame(t(tbl.quarter[,-1]))
RMSE.overall <- as.data.frame(t(tbl.overall))
colnames(RMSE.quarter) <- c("Q1", "Q2", "Q3", "Q4")
colnames(RMSE.overall) <- c("Overall")

RMSE <- bind_cols(RMSE.quarter, RMSE.overall)
rownames(RMSE) <- c("Model 1.1", "Model 1.2", "Model 2.1", "Model 2.2", 
                    "Model 3.1", "Model 3.2", "Model 4.1", "Model 4.2")
RMSE %>%
  huxtable::as_hux() %>%
  add_rownames(rowname = c("Model 1.1", "Model 1.2", "Model 2.1", "Model 2.2", 
                 "Model 3.1", "Model 3.2", "Model 4.1", "Model 4.2")) -> RMSE.hux
RMSE.hux[1, 1] <- ""
RMSE.hux <- add_rows(RMSE.hux, c("Root Mean Square Error", rep("", 5)), after = 0)

RMSE.hux %>%
  set_align(row = 1, col = c(1: 6), "center") %>%
  set_top_border(row = 1, col = c(1: 6)) %>%
  set_bottom_border(row = c(1, 10), col = c(1: 6)) %>%
  merge_across(row = 1, col = c(1: 6)) -> RMSE.hux.formatted

RMSE.hux.formatted <- add_footnote(RMSE.hux.formatted, text = footnote)
print(RMSE.hux.formatted)

# Coverage ---------------------------------------------------------------------

result.full %>%
  group_by(Quarter) %>%
  summarise(coverage.1.1 = mean(cover.1.1),
            coverage.1.2 = mean(cover.1.2),
            coverage.2.1 = mean(cover.2.1),
            coverage.2.2 = mean(cover.2.2),
            coverage.3.1 = mean(cover.3.1),
            coverage.3.2 = mean(cover.3.2),
            coverage.4.1 = mean(cover.4.1),
            coverage.4.2 = mean(cover.4.2)) -> tbl.quarter

result.full %>%
  summarise(coverage.1.1 = mean(cover.1.1),
            coverage.1.2 = mean(cover.1.2),
            coverage.2.1 = mean(cover.2.1),
            coverage.2.2 = mean(cover.2.2),
            coverage.3.1 = mean(cover.3.1),
            coverage.3.2 = mean(cover.3.2),
            coverage.4.1 = mean(cover.4.1),
            coverage.4.2 = mean(cover.4.2)) -> tbl.overall

coverage.quarter <- as.data.frame(t(tbl.quarter[,-1]))
coverage.overall <- as.data.frame(t(tbl.overall))
colnames(coverage.quarter) <- c("Q1", "Q2", "Q3", "Q4")
colnames(coverage.overall) <- c("Overall")

coverage <- bind_cols(coverage.quarter, coverage.overall)
rownames(coverage) <- c("Model 1.1", "Model 1.2", "Model 2.1", "Model 2.2",
                        "Model 3.1", "Model 3.2", "Model 4.1", "Model 4.2")
coverage %>%
  huxtable::as_hux() %>%
  add_rownames(rowname = c("Model 1.1", "Model 1.2", "Model 2.1", "Model 2.2", 
                           "Model 3.1", "Model 3.2", "Model 4.1", "Model 4.2")) -> coverage.hux
coverage.hux[1, 1] <- ""
coverage.hux <- add_rows(coverage.hux, c("Coverage of 95% Prediction Interval", rep("", 5)), after = 0)

coverage.hux %>%
  set_align(row = 1, col = c(1: 6), "center") %>%
  set_top_border(row = 1, col = c(1: 6)) %>%
  set_bottom_border(row = c(1, 10), col = c(1: 6)) %>%
  merge_across(row = 1, col = c(1: 6)) -> coverage.hux.formatted

coverage.hux.formatted <- add_footnote(coverage.hux.formatted, text = footnote)
print(coverage.hux.formatted)

# ------------------------------------------------------------------------------

sheet.name <- paste0("Sheet", n)

wb1 <- as_Workbook(RMSE.hux.formatted, Workbook = wb1, sheet = sheet.name)
saveWorkbook(wb1, "RMSE.by.quarter.xlsx", overwrite = T)

wb2 <- as_Workbook(coverage.hux.formatted, Workbook = wb2, sheet = sheet.name)
saveWorkbook(wb2, "coverage.by.quarter.xlsx", overwrite = T)
}
