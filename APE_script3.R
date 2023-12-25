library(tidyverse)
library(huxtable)
library(openxlsx)

for (n in c(1:4)){
if (n == 1){
  file.ext <- ".csv"
  wb <- openxlsx::createWorkbook()
  footnote <- "Radius 100km; sample by (location, date)"
} else if (n == 2) {
  file.ext <- ".nobuffer.csv"
  wb <- openxlsx::loadWorkbook("RMSE.by.quarter.xlsx")
  footnote <- "No buffer / radius 0km; sample by location"
} else if (n == 3) {
  file.ext <- ".new.csv"
  wb <- openxlsx::loadWorkbook("RMSE.by.quarter.xlsx")
  footnote <- "Radius 100km; sample by location"
} else {
  file.ext <- ".radius200.csv"
  wb <- openxlsx::loadWorkbook("RMSE.by.quarter.xlsx")
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
                             Month >= 10 & Month <= 12 ~ "Q4")) -> result.full

# Quarter ----------------------------------------------------------------------

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
sheet.name <- paste0("Sheet", n)

wb <- as_Workbook(RMSE.hux.formatted, Workbook = wb, sheet = sheet.name)
saveWorkbook(wb, "RMSE.by.quarter.xlsx", overwrite = T)
}
