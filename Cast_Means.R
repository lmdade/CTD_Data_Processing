# Lindsay Dade
# November 9, 2023
rm(list = ls())
library(tidyverse)
library(oce)
library(matrixStats)
library(readxl)
library(tools)

setwd("/Users/lmdade/Desktop/Water Quality Monitoring/CTD Analyses")

#read in metadata for casts
meta  <- read_excel("File_info_CTDcasts.xlsx", col_names = TRUE, sheet = 1)

#read in functions 
source("CTD_Data_Processing/CTD_functions.R")

##### process one file ######
d <- create_ctd_df("Oce_Package_ACSII/February/20230221_CC.asc")
summary(d)
clean_ctd_df("Oce_Package_ACSII/February/20230221_CC.asc")

#looking at temp, any bad data?
plot(d$T090C, d$PrdM, type = "l")

#get table of means
bp <- bottom_means_tbl("Oce_Package_ACSII/February/20230221_CC.asc", "20230221_CC")
head(bp)

####### now loop all February files #####
# creating list of file names
feb.files <- dir("/Users/lmdade/Desktop/Water Quality Monitoring/CTD Analyses/Oce_Package_ACSII/February", pattern = "[.]asc$", recursive = TRUE, full.names = TRUE)
head(feb.files)

df <- data.frame() #create empty dataframae to populate in loop

for (i in 1:length(feb.files)){
  a <- bottom_means_tbl(feb.files[i], file_path_sans_ext(basename(feb.files[i]))) %>% 
    as.data.frame()
  df <- rbind(df,a)
}

#March
mar.files <- dir("/Users/lmdade/Desktop/Water Quality Monitoring/CTD Analyses/Oce_Package_ACSII/March", pattern = "[.]asc$", recursive = TRUE, full.names = TRUE)

for (i in 1:length(mar.files)){
  a <- bottom_means_tbl(mar.files[i], file_path_sans_ext(basename(mar.files[i]))) %>% 
    as.data.frame()
  df <- rbind(df,a)
}

#April 

