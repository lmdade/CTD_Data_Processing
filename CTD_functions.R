#Lindsay Dade

#### Creating a ctd dataframe from ASCII file, after it has been renamed for the correct site. Setting bad flag codes to NA or missing values. 
# code adapted from meeting with Dan Kelly, see "Cleaning_Binning.R"

create_ctd_df <- function(file) {
  header <- readLines(file, n=1, encoding="latin1")
  header <- gsub("é00", "", header)
  names <- strsplit(header, "\t")[[1]]
  d <- read.delim(file, sep="\t", skip=1, header=FALSE, col.names=names)
  missingValue <- -9.99e-29
  d$Sbeox0Mg.L[1] == missingValue
  d[d == missingValue] <- NA
  return(d)
}

# cleaning up and prepping data
clean_ctd_df <- function(file) {
  clean <- create_ctd_df(file) %>% 
    filter((PrdM > 0)) %>% #remove negative pressure values
    filter(PrdM > (max(PrdM) -  1)) %>% #filter out bottom meter
    select(c("PrdM", "Sal00", "T090C", "C0S.m", "FlECO.AFL", "TurbWETntu0", "Par.sat.log", "Sbeox0Mg.L")) %>% #picking out pressure,salinity, temp, conductivity, flourescence, turb, par, DO
    rename(c("Pressure" = "PrdM",
             "Salinity" = "Sal00",
             "Temperature" = "T090C",
             "Conductivity" = "C0S.m",
             "Chloro" = "FlECO.AFL",
             "Turbidity" = "TurbWETntu0",
             "Par" = "Par.sat.log", 
             "DO" = "Sbeox0Mg.L")) %>%
    as.matrix()   
  return(clean)
}

###### make into summary table of min, max, standard error, and mean. also pull in metadata for cast location 
bottom_means_tbl <- function(file, cast) {
  clean <- clean_ctd_df(file)
 
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
    mutate(Cast_ID = cast) %>%
    left_join(meta, by  =  "Cast_ID") %>%
    select(-c("Old_Name", "Notes")) %>% 
    relocate("Cast_ID", .before = "Mean_Pressure") %>% 
    relocate("Month", .after = "Cast_ID") %>% 
    relocate("Location", .after = "Month") %>% 
    relocate("Instrument_Time", .after = "Location") %>% 
    relocate("Long", .after = "Instrument_Time") %>% 
    relocate("Lat", .after = "Long")
  return(tbl)
}
