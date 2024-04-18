### 10 June 2021 Updated with new 2015-2019 data to be able to exclude data from 2016

### Recalibrate selection such that only select rivers wiht > 1 case / 5km

rm(list = ls())
riverBuff <- 5000
villageBuff <- 1000

library(rgdal)
library(rgeos)
library(tidyverse)
library(maptools)
library(readxl)
library(ggplot2)
library(ggrepel)
library(readr)

utm34s <- CRS("+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
zs <- readOGR("C:\\Users\\paul\\Documents\\FIND\\Countries\\DRC\\Data\\DRC and Bandundu GIS data\\Bandundu DPS and health zone shapefiles\\BANDUNDU HZ BOUNDARIES","Bandundu_health_zone_boundaries") %>%
  spTransform(utm34s)

cases <- read.csv("C:\\Users\\paul\\Dropbox\\Paul\\LSTM\\Data\\DRC\\HAT_Data\\Data2018-2019\\Processed data\\HAT_Case_15_19_Agg_v2.csv")

casesRed <- cases

casesRedSP <- SpatialPointsDataFrame(cbind(casesRed$LONG, casesRed$LAT), data = casesRed, proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>% spTransform(utm34s)

casesRedSP$cID <- 1:nrow(casesRedSP@data)

cases20152019 <- casesRedSP
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"



# HAT cases 2017-2021 -----------------------------------------------------

hatCases2021 <- read_excel("C:\\Users\\paul\\Dropbox\\Paul\\LSTM\\Data\\DRC\\HAT_Data\\ITMData\\ITM_2020-2021\\Endemic villages Bandundu 2020 and 2021 LSTM.xlsx") %>%
  rename("c2021" = "Cases\r\n2021",
         "c2020" = "Cases\r\n2020",
         "Village" = "Village name",
         "Video" = "Video taken for Quality Control",
         "PresenceC" = "Presence of Trypanosomes confirmed",
         "Screening" = "Active/Passive screening") %>%
  gather("Year", "Cases" ,-c(Village, Population, Province, ZS, AS, Longitude, Latitude, Video, PresenceC, Screening))%>%
  mutate(Year = as.integer(gsub("c", "", Year))) %>%
  select(ZS, AS, Village, Longitude, Latitude, Year, Cases)


hatCases1719 <- read.csv("C:\\Users\\paul\\Dropbox\\Paul\\LSTM\\Data\\DRC\\HAT_Data\\ITMData\\ITM_2017-2020\\Endemic villages BDD 2017 - 2020.csv") %>%
  gather("Year", "Cases" ,-c(Nom, Population, Province, ZS, AS, Longitude, Latitude)) %>%
  filter(!Year %in% c("c2020", "c2021")) %>%
  mutate(Year = as.integer(gsub("c", "", Year))) %>%
  select(ZS, AS, Nom, Longitude, Latitude, Year, Cases)

names(hatCases1719) <- names(hatCases2021)

compiledCases <- hatCases2021 %>%
  bind_rows(hatCases1719) %>%
  mutate(Weight = (Year - 2016) / 5) %>%
  mutate(caseWeight = Cases * Weight)

compiledCasesComp <- compiledCases %>%
  filter(!is.na(Longitude),
         Cases > 0) %>%
  group_by(Longitude, Latitude) %>%
  summarise(TotalCases = sum(Cases),
            TotalWeight = sum(caseWeight))


compiledCasesSP <- SpatialPointsDataFrame(cbind(compiledCasesComp$Longitude, compiledCasesComp$Latitude), data = compiledCasesComp, proj4string = CRS(wgs84))
compiledCasesSP$cID <- 1:nrow(compiledCasesSP@data)

cases20172021 <- compiledCasesSP  %>% spTransform(utm34s)



# Intervention Areas ------------------------------------------------------

interventionAreas <- readOGR("C:\\Users\\paul\\Dropbox\\Paul\\LSTM\\Data\\DRC\\HAT_Data\\Data2018-2019\\Processed data\\Shapefile\\Foci", "VC_Foci_Comb_1519Edit_FINAL")
interventionAreas$cStatus[interventionAreas$cStatus == "Candidate"] <- "Planned"
interventionAreasRed <- interventionAreas[!interventionAreas$Future2 %in% c("Cease", "Drop", "Watch", "Planned") & interventionAreas$pName != "Bagata",]

casesO <- cases20172021 %over% zs
cases20172021$ZS <- casesO$ZS

iaOver <- cases20172021 %over% interventionAreasRed
cases20172021$iaZS <- iaOver$ZS
cases20172021$Status <- iaOver$cStatus
cases20172021$iaName <- iaOver$pName
cases20172021$Status[is.na(cases20172021$Status)] <- "None"

compiledCases2021 <- hatCases2021 %>%
  filter(!is.na(Longitude),
         Cases > 0) %>%
  group_by(Longitude, Latitude) %>%
  summarise(TotalCases = sum(Cases))

compiledCases2021SP <- SpatialPointsDataFrame(cbind(compiledCases2021$Longitude, compiledCases2021$Latitude), data = compiledCases2021, proj4string = CRS(wgs84))
compiledCases2021SP$cID <- 1:nrow(compiledCases2021SP@data)

compiledCases2021SP <- compiledCases2021SP  %>% spTransform(utm34s)

casesO <- compiledCases2021SP %over% zs
compiledCases2021SP$ZS <- casesO$ZS

iaOver <- compiledCases2021SP %over% interventionAreasRed
compiledCases2021SP$iaZS <- iaOver$ZS
compiledCases2021SP$Status <- iaOver$cStatus
compiledCases2021SP$iaName <- iaOver$pName
compiledCases2021SP$Status[is.na(compiledCases2021SP$Status)] <- "None"

# Spatial data ------------------------------------------------------------
zs <- readOGR("C:\\Users\\paul\\Documents\\FIND\\Countries\\DRC\\Data\\DRC and Bandundu GIS data\\Bandundu DPS and health zone shapefiles\\BANDUNDU HZ BOUNDARIES","Bandundu_health_zone_boundaries") %>%
  spTransform(utm34s)
allRivers <- readOGR("C:\\Users\\paul\\Dropbox\\Paul\\LSTM\\Data\\DRC\\Expansion\\Rivers", "RiverExpansion_red_v2") %>%
  spTransform(utm34s)
allRivers$FID <- allRivers$RID
allRivers$Length <- gLength(allRivers, byid = T) / 1000
allRivers@data <- allRivers@data %>%
  rename("Intervention" = "Interventi",
         "cStatus" = "Status") %>%
  mutate(nStatus = replace_na(nStatus, "None")) %>%
  mutate(Intervention = nStatus)


# Base Rivers
# Screening out rivers ----------------------------------------------------
cRivers <- allRivers[allRivers$nStatus %in% c("Current", "Keep"),]

cRiverBuff <- gBuffer(cRivers, byid = T, width = riverBuff)

caseOver2021 <- compiledCases2021SP %over% cRiverBuff
casesOut2021 <- compiledCases2021SP@data %>%
  bind_cols(caseOver2021) %>%
  mutate(nStatus =ifelse(is.na(nStatus), "None", nStatus),
         nStatus = ifelse(nStatus == "Keep", "Planned", nStatus),
        # nStatus = ifelse(Status == "Current", "Current", nStatus),
        # nStatus = ifelse(Status == "Planned", "Planned", nStatus)
        )

casesOutSummary2021 <- casesOut2021 %>%
  group_by(nStatus) %>%
  summarise(TotalCases = sum(TotalCases)) %>%
  mutate(Prop = TotalCases / sum(TotalCases))

casesOutSummary2021

caseOver1721 <- cases20172021 %over% cRiverBuff
casesOut1721 <- cases20172021@data %>%
  bind_cols(caseOver1721) %>%
  mutate(nStatus =ifelse(is.na(nStatus), "None", nStatus),
         nStatus = ifelse(nStatus == "Keep", "Planned", nStatus),
        # nStatus = ifelse(Status == "Current", "Current", nStatus),
        # nStatus = ifelse(Status == "Planned", "Planned", nStatus)
        )

casesOutSummary1721 <- casesOut1721 %>%
  group_by(nStatus) %>%
  summarise(TotalCases = sum(TotalCases)) %>%
  mutate(Prop = TotalCases / sum(TotalCases))

casesOutSummary1721

cRiversLength <- cRivers@data %>%
  group_by(nStatus) %>%
  summarise(Length = sum(Length))
sum(cRiversLength$Length)



# Summary statistics ------------------------------------------------------

casesSummary <- casesOut1721 %>%
  group_by(pName, Intervention, RID, ZS...12, River_I, cStatus, Coord, nStatus, Length, FID) %>%
  summarise(TotalCases = sum(TotalCases),
            TotalWeight = sum(TotalWeight)) %>%
  filter(nStatus != "None")

riverLengthSummary <- casesOut1721 %>%
  ungroup() %>%
  dplyr::select(RID, Length) %>%
  distinct()

write_excel_csv(casesSummary, file = "C:\\Users\\paul\\Dropbox\\Paul\\LSTM\\Data\\DRC\\Costs\\Marina model\\Data\\ZSCaseBreakdown_20230410.csv", na = "")



# Bagata Kwilu
# Screening out rivers ----------------------------------------------------
cRivers <- allRivers[allRivers$nStatus %in% c("Current", "Keep") | allRivers$RID %in% c(86,106),]
cRivers <- cRivers[!cRivers$RID %in% c(21,23,24,27),]
cRivers$nStatus[cRivers$RID %in% c(86,106)] <- "Keep"

cRiverBuff <- gBuffer(cRivers, byid = T, width = riverBuff)

caseOver2021 <- compiledCases2021SP %over% cRiverBuff
casesOut2021 <- compiledCases2021SP@data %>%
  bind_cols(caseOver2021) %>%
  mutate(nStatus =ifelse(is.na(nStatus), "None", nStatus),
         nStatus = ifelse(nStatus == "Keep", "Planned", nStatus),
        # nStatus = ifelse(Status == "Current", "Current", nStatus),
        # nStatus = ifelse(Status == "Planned", "Planned", nStatus)
        )

casesOutSummary2021 <- casesOut2021 %>%
  group_by(nStatus) %>%
  summarise(TotalCases = sum(TotalCases)) %>%
  mutate(Prop = TotalCases / sum(TotalCases))

casesOutSummary2021

caseOver1721 <- cases20172021 %over% cRiverBuff
casesOut1721 <- cases20172021@data %>%
  bind_cols(caseOver1721) %>%
  mutate(nStatus =ifelse(is.na(nStatus), "None", nStatus),
         nStatus = ifelse(nStatus == "Keep", "Planned", nStatus),
         # nStatus = ifelse(Status == "Current", "Current", nStatus),
         # nStatus = ifelse(Status == "Planned", "Planned", nStatus)
         )

# casesOut1721[casesOut1721$ZS== "Bagata",]
casesOutSummary1721 <- casesOut1721 %>%
  group_by(nStatus) %>%
  summarise(TotalCases = sum(TotalCases)) %>%
  mutate(Prop = TotalCases / sum(TotalCases))

casesOutSummary1721

cRiversLength <- cRivers@data %>%
  group_by(nStatus) %>%
  summarise(Length = sum(Length))
sum(cRiversLength$Length)




# Bolobo
# Screening out rivers ----------------------------------------------------
cRivers <- allRivers[allRivers$nStatus %in% c("Current", "Keep"),]
# cRivers <- cRivers[!cRivers$RID %in% c(21,23,24,27),]
cRivers$nStatus[cRivers$RID %in% c(21,23,24,27)] <- "None"

cRiverBuff <- gBuffer(cRivers, byid = T, width = riverBuff)

caseOver2021 <- compiledCases2021SP %over% cRiverBuff
casesOut2021 <- compiledCases2021SP@data %>%
  bind_cols(caseOver2021) %>%
  mutate(nStatus =ifelse(is.na(nStatus), "None", nStatus),
         nStatus = ifelse(nStatus == "Keep", "Planned", nStatus),
         # nStatus = ifelse(Status == "Current", "Current", nStatus),
         # nStatus = ifelse(Status == "Planned", "Planned", nStatus)
  )

casesOutSummary2021 <- casesOut2021 %>%
  group_by(nStatus) %>%
  summarise(TotalCases = sum(TotalCases)) %>%
  mutate(Prop = TotalCases / sum(TotalCases))

casesOutSummary2021

caseOver1721 <- cases20172021 %over% cRiverBuff
casesOut1721 <- cases20172021@data %>%
  bind_cols(caseOver1721) %>%
  mutate(nStatus =ifelse(is.na(nStatus), "None", nStatus),
         nStatus = ifelse(nStatus == "Keep", "Planned", nStatus),
         # nStatus = ifelse(Status == "Current", "Current", nStatus),
         # nStatus = ifelse(Status == "Planned", "Planned", nStatus)
  )

# casesOut1721[casesOut1721$ZS== "Bagata",]
casesOutSummary1721 <- casesOut1721 %>%
  group_by(nStatus) %>%
  summarise(TotalCases = sum(TotalCases)) %>%
  mutate(Prop = TotalCases / sum(TotalCases))

casesOutSummary1721

cRiversLength <- cRivers@data %>%
  group_by(nStatus) %>%
  summarise(Length = sum(Length))
sum(cRiversLength$Length[cRiversLength$nStatus != "None"])



# Kasai
# Screening out rivers ----------------------------------------------------
cRivers <- allRivers[allRivers$nStatus %in% c("Current", "Keep"),]
# cRivers <- cRivers[!cRivers$RID %in% c(21,23,24,27),]
cRivers$nStatus[cRivers$RID %in% c(87,88)] <- "None"

cRiverBuff <- gBuffer(cRivers, byid = T, width = riverBuff)

caseOver2021 <- compiledCases2021SP %over% cRiverBuff
casesOut2021 <- compiledCases2021SP@data %>%
  bind_cols(caseOver2021) %>%
  mutate(nStatus =ifelse(is.na(nStatus), "None", nStatus),
         nStatus = ifelse(nStatus == "Keep", "Planned", nStatus),
         # nStatus = ifelse(Status == "Current", "Current", nStatus),
         # nStatus = ifelse(Status == "Planned", "Planned", nStatus)
  )

casesOutSummary2021 <- casesOut2021 %>%
  group_by(nStatus) %>%
  summarise(TotalCases = sum(TotalCases)) %>%
  mutate(Prop = TotalCases / sum(TotalCases))

casesOutSummary2021

caseOver1721 <- cases20172021 %over% cRiverBuff
casesOut1721 <- cases20172021@data %>%
  bind_cols(caseOver1721) %>%
  mutate(nStatus =ifelse(is.na(nStatus), "None", nStatus),
         nStatus = ifelse(nStatus == "Keep", "Planned", nStatus),
         # nStatus = ifelse(Status == "Current", "Current", nStatus),
         # nStatus = ifelse(Status == "Planned", "Planned", nStatus)
  )

# casesOut1721[casesOut1721$ZS== "Bagata",]
casesOutSummary1721 <- casesOut1721 %>%
  group_by(nStatus) %>%
  summarise(TotalCases = sum(TotalCases)) %>%
  mutate(Prop = TotalCases / sum(TotalCases))

casesOutSummary1721

cRiversLength <- cRivers@data %>%
  filter(nStatus != "None") %>%
  group_by(nStatus) %>%
  summarise(Length = sum(Length))
sum(cRiversLength$Length[cRiversLength$nStatus != "None"])




# Lubue - Ipamu
# Screening out rivers ----------------------------------------------------
cRivers <- allRivers[allRivers$nStatus %in% c("Current", "Keep") | allRivers$RID %in% c(100),]
# cRivers <- cRivers[!cRivers$RID %in% c(21,23,24,27),]
cRivers$nStatus[cRivers$RID %in% c(100)] <- "Keep"
#cRivers$nStatus[cRivers$RID %in% c(21,23,24,27)] <- "None"

cRiverBuff <- gBuffer(cRivers, byid = T, width = riverBuff)

caseOver2021 <- compiledCases2021SP %over% cRiverBuff
casesOut2021 <- compiledCases2021SP@data %>%
  bind_cols(caseOver2021) %>%
  mutate(nStatus =ifelse(is.na(nStatus), "None", nStatus),
         nStatus = ifelse(nStatus == "Keep", "Planned", nStatus),
         # nStatus = ifelse(Status == "Current", "Current", nStatus),
         # nStatus = ifelse(Status == "Planned", "Planned", nStatus)
  )

casesOutSummary2021 <- casesOut2021 %>%
  group_by(nStatus) %>%
  summarise(TotalCases = sum(TotalCases)) %>%
  mutate(Prop = TotalCases / sum(TotalCases))

casesOutSummary2021

caseOver1721 <- cases20172021 %over% cRiverBuff
casesOut1721 <- cases20172021@data %>%
  bind_cols(caseOver1721) %>%
  mutate(nStatus =ifelse(is.na(nStatus), "None", nStatus),
         nStatus = ifelse(nStatus == "Keep", "Planned", nStatus),
         # nStatus = ifelse(Status == "Current", "Current", nStatus),
         # nStatus = ifelse(Status == "Planned", "Planned", nStatus)
  )

# casesOut1721[casesOut1721$ZS== "Bagata",]
casesOutSummary1721 <- casesOut1721 %>%
  group_by(nStatus) %>%
  summarise(TotalCases = sum(TotalCases)) %>%
  mutate(Prop = TotalCases / sum(TotalCases))

casesOutSummary1721

cRiversLength <- cRivers@data %>%
  filter(nStatus != "None") %>%
  group_by(nStatus) %>%
  summarise(Length = sum(Length))
sum(cRiversLength$Length[cRiversLength$nStatus != "None"])



