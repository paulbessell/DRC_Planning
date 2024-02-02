
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(rgeos)
library(lubridate)
library(readxl)
library(readr)

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"

hatCases2022 <- get(load("C:\\Users\\paul\\Dropbox\\Paul\\LSTM\\RCode\\DRC\\Case analysis\\DRC_Case_Analysis\\data\\Processed\\HAT_Cases_2020_2022.RData")) %>%
  mutate(ST2 - ifelse(Mode == "passive", NC, ST2),
         ST2 = NC - ST1) %>%
  dplyr::select(ZS, AS, Village, Longitude, Latitude, Year, NC, ST2) %>%
  rename("Cases" = "NC")


hatCases2021 <- read_excel("C:\\Users\\paul\\Dropbox\\Paul\\LSTM\\Data\\DRC\\HAT_Data\\ITMData\\ITM_2020-2021\\Endemic villages Bandundu 2020 and 2021 LSTM PB.xlsx") %>%
  rename("c2021" = "Cases\r\n2021",
         "c2020" = "Cases\r\n2020",
         "Village" = "Village name",
         "Video" = "Video taken for Quality Control",
         "PresenceC" = "Presence of Trypanosomes confirmed",
         "Screening" = "Active/Passive screening") %>%
  gather("Year", "Cases" ,-c(Village, Population, Province, ZS, AS, Longitude, Latitude, Video, PresenceC, Screening))%>%
  mutate(Year = as.integer(gsub("c", "", Year))) %>%
  select(ZS, AS, Village, Longitude, Latitude, Year, Cases)

hatCases1619 <- get(load("C:\\Users\\paul\\Dropbox\\Paul\\LSTM\\RCode\\DRC\\Case analysis\\DRC_Case_Analysis\\data\\Processed\\DataCleanind\\Cases_Processed_2016_2019.RData")) %>%
  mutate(ST2 = NC.Paul - NC.DP.ST1 - NC.DA.ST1) %>%
  rename("Cases" = "NC.Paul",
         "Village" = "VILLAGE") %>%
  dplyr::select(names(hatCases2022))




hatAtlas <- read.csv("C:\\Users\\paul\\Dropbox\\Paul\\LSTM\\Data\\DRC\\HAT_Data\\HAT Atlas\\Liverpool_School_of_Tropical_Medicine_HAT_Bandundu_2000_2015.csv") %>%
  rename("Cases" = "New_HAT_cases",
         "ZS" = "ZONE_DE_SANTE",
         "AS" = "AIRE_DE_SANTE",
         "Village" = "Location_name") %>%
  mutate(P1 = replace_na(P1, 0),
         P2 = replace_na(P2, 0)) %>%
  mutate(ST2 = Cases - P1) %>%
  dplyr::select(names(hatCases2022)) 

# names(hatAtlas) <- names(hatCases1720)

compiledCasesAll <- hatAtlas %>%
  bind_rows(hatCases1619) %>%
  bind_rows(hatCases2022) %>%
  filter(Cases > 0) 
write_excel_csv(compiledCasesAll, file = "data/Cases_Compiled_2000_2022.csv", na = "")

compiledCasesAllNA <- compiledCasesAll %>%
  filter(is.na(Latitude)) %>%
  arrange(desc(Year))

write_excel_csv(compiledCasesAllNA, file = "data/cleaning/missingLongitude.csv", na = "")

