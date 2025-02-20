library(readxl)
library(dplyr)
rm(list = ls())
path_to_file <- "/Users/allan/Downloads/PenguinDataSP25.xlsx"
PenguinData <- subset(read_excel(path_to_file, skip = 5) %>% filter(row_number() <= n()-4), select = -c(sampleID, scientific, health_metrics)) %>% filter(life_stage == 'adult' & body_mass != 0)
PenguinData <- subset(PenguinData, select = -c(life_stage))
PenguinData