library(tidyverse)
library(arrow)

setwd("~/Small_Area_Analysis/Files/File_Creation/Pre_Datasets")
Regions <- read_csv("Regions.csv")

#==============================================================================#
# Create Regions
#==============================================================================#

# Mountains
Mountains <- Regions %>%
  filter(Region == "Mountains") %>%
  dplyr::select(ZCTA)

# Piedmont
Piedmont <- Regions %>%
  filter(Region == "Piedmont") %>%
  dplyr::select(ZCTA)

# Coast
Coast <- Regions %>%
  filter(Region == "Coast") %>%
  dplyr::select(ZCTA)

#==============================================================================#
# Create the filtered data
#==============================================================================#

Data <- left_join(open_dataset("NC_Heatwave_ML.parquet") %>%
                    select(Zip, Date, TAVG, TMAX, TMIN, RH),
                  open_dataset("Sheps_for_Gasparini.parquet") %>%
                    rename(Date = admitdt,
                           Zip = zip5) %>%
                    group_by(Zip, Date) %>%
                    summarise(Mental_Health = sum(Mental_Health),
                              Substance = sum(Substance),
                              Schizophrenia = sum(Schizophrenia),
                              Mood = sum(Mood),
                              Anxiety = sum(Anxiety),
                              Behavioral = sum(Behavioral),
                              Personality = sum(Personality),
                              Intellectual = sum(Intellectual),
                              Developmental = sum(Developmental),
                              Emotional = sum(Emotional)),
                  by = c("Zip", "Date")) %>%
  collect() %>% 
  filter(Date >= "2016-01-01", Date <= "2019-12-31") %>%
  replace(is.na(.), 0) %>%
  mutate(region = ifelse(Zip %in% Mountains$ZCTA, "Mountains",
                         ifelse(Zip %in% Piedmont$ZCTA, "Piedmont",
                                ifelse(Zip %in% Coast$ZCTA, "Coast", "")))) 

# Data$Age <- fifelse(Data$agey <= 25, 1,
#               fifelse(Data$agey > 25 & Data$agey <= 49, 2,
#                 fifelse(Data$agey > 49 & Data$agey <= 64, 3,
#                   4)))

#==============================================================================#
# Finalize
#==============================================================================#

setwd("~/Small_Area_Analysis/Files")
write_parquet(Data, "Sheps_Temp_Regions.parquet")

