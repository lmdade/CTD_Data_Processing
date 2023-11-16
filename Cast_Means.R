# Lindsay Dade
# November 9, 2023
rm(list = ls())
library(tidyverse)
library(oce)
library(matrixStats)
library(readxl)

setwd("/Users/lmdade/Desktop/Water Quality Monitoring/CTD Analyses")

#read in metadata for casts
meta  <- read_excel("File_info_CTDcasts.xlsx", col_names = TRUE, sheet = 1)

#read in functions IN PROGRESS
source("CTD_functions.R")
create_ctd_df("Oce_Package_ACSII/February/20230222_FB.asc")

# old code
f <-  "Oce_Package_ACSII/February/20230222_FB.asc"
header <- readLines(f, n=1, encoding="latin1")
header <- gsub("é00", "", header)
names <- strsplit(header, "\t")[[1]]
d <- read.delim(f, sep="\t", skip=1, header=FALSE, col.names=names)

# transform bad flag codes to NA's
missingValue <- -9.99e-29
d$Sbeox0Mg.L[1] == missingValue
d[d == missingValue] <- NA
head(d)

#looking at temp, any bad data?
plot(d$T090C, d$PrdM, type = "l")

data  <-  d  %>%
  filter(PrdM > (max(PrdM) -  1)) %>% #filter out bottom meter
  select(c("PrdM", "Sal00", "T090C", "C0S.m", "FlECO.AFL", "TurbWETntu0", "Par.sat.log", "Sbeox0Mg.L")) %>% #picking out pressure,salinity, temp, conductivity, flourescence, turb, par, DO
  rename(c("Pressure" = "PrdM",
           "Salinity" = "Sal00",
           "Temperature" = "T090C",
           "Conductivity" = "C0S.m",
           "Chloro" = "FlECO.AFL",
           "Turbidity" = "TurbWETntu0",
           "Par" = "Par.sat.log", 
           "DO" = "Sbeox0Mg.L")) 

head(data)

# want to make new data  with cast_ID, site, date, time, coordinates, min,  max,  mean, sd for each site
#make it a matrix  first for matrixStat functions
matrix <- as.matrix(data)

mean <- colMeans(matrix, na.rm = TRUE) %>% 
  set_names(paste0('Mean_',  names(.))) %>%
  as.data.frame()

sd <-  colMads(matrix, na.rm = TRUE) %>%
  set_names(paste0('SD_',  names(.))) %>% 
  as.data.frame()

max <- colMaxs(matrix, na.rm = TRUE) %>%
  set_names(paste0('Max_',  names(.))) %>% 
  as.data.frame()

min <- colMins(matrix, na.rm = TRUE) %>%
  set_names(paste0('Min_',  names(.))) %>% 
  as.data.frame()

df <- rbind(mean, sd,  min, max) %>%
  rownames_to_column("par")

tbl <- df %>% 
  pivot_wider(names_from = "par",  values_from = ".") %>%
  mutate(Cast_ID = "20230222_FB") %>%
  left_join(meta, by  =  "Cast_ID") %>%
  select(-c("Old_Name", "Notes")) %>% 
  relocate("Cast_ID", .before = "Mean_Pressure") %>% 
  relocate("Month", .after = "Cast_ID") %>% 
  relocate("Location", .after = "Month") %>% 
  relocate("Instrument_Time", .after = "Location") %>% 
  relocate("Long", .after = "Instrument_Time") %>% 
  relocate("Lat", .after = "Long")
View(tbl)

##########
clean <- clean_ctd_df("Oce_Package_ACSII/February/20230222_FB.asc")

mean <- colMeans(clean, na.rm = TRUE) %>% 
  set_names(paste0('Mean_',  names(.))) %>%
  as.data.frame() 

sd <-  colMads(clean, na.rm = TRUE) %>%
  set_names(paste0('SD_',  names(.))) %>% 
  as.data.frame()

max <- colMaxs(clean, na.rm = TRUE) %>%
  set_names(paste0('Max_',  names(.))) %>% 
  as.data.frame()

min <- colMins(clean, na.rm = TRUE) %>%
  set_names(paste0('Min_',  names(.))) %>% 
  as.data.frame()

tbl <- rbind(mean, sd,  min, max) %>%
  rownames_to_column("par") %>% 
  pivot_wider(names_from = "par",  values_from = ".") %>%
  mutate(Cast_ID = "20230222_FB") %>%
  left_join(meta, by  =  "Cast_ID") %>%
  select(-c("Old_Name", "Notes")) %>% 
  relocate("Cast_ID", .before = "Mean_Pressure") %>% 
  relocate("Month", .after = "Cast_ID") %>% 
  relocate("Location", .after = "Month") %>% 
  relocate("Instrument_Time", .after = "Location") %>% 
  relocate("Long", .after = "Instrument_Time") %>% 
  relocate("Lat", .after = "Long")