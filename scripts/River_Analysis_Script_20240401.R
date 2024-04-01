
rm(list = ls())
riverBuff <- 5000
villageBuff <- 1000
riverFlowCutoff <- 15
caseWeight <- FALSE
startYear <- 2019
finalYear <- 2023
tYears <- finalYear - startYear +1
caseThreshold <- 2 ### Add someting to look at case density


library(sf)
library(tidyverse)
library(spdep)
library(raster)
library(readr)
library(ggpubr)



# Compiled data -----------------------------------------------------------

# Case data
cases <- read_csv("data/Cases_Compiled_2000_2023.csv")

cases_sf <- cases %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  filter(Year %in% startYear:finalYear) %>%
  mutate(CaseID = row_number())

# ZS
# zs <- read_sf("C:/Users/paulb/Documents/FIND/Countries/DRC/Data/Spatial_Data/Shapefiles/zone_ste_puc/Zone_Sté_Puc.shp") %>% 
#   st_make_valid() %>%
#   st_transform(4326) %>%
#   mutate("ZS" = NOM_ZS)
# 
# AS
as <- read_sf("C:\\Users\\paul\\Documents\\FIND\\Countries\\DRC\\Data\\UCLA-PNLTHA Bandundu Village Lists\\BANDUNDU MERGE\\BANDUNDU HA MERGE 2.15.17", "BANDUNDU_HA_MERGE_02_15_17") %>%
  st_transform(crs = 4326)



# Rivers ------------------------------------------------------------------

rivers <- st_read("C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/BAndunduH_ZS_2_PMethod_5Cutoff.shp")

rivers <- st_read("C:/Users/paulb/Dropbox/Paul/LSTM/Data/DRC/Targets/Rivers/Cutoff5/Rivers_ZS_Analysis_Cut_off_5.shp") %>%
  mutate(SegmentID = row_number(),
         segLength = as.numeric(st_length(.)),
         RID = row_number()) %>%
  filter(DIS_AV_ > riverFlowCutoff)

riversIDs <- rivers %>%
  st_drop_geometry() %>%
  dplyr::select(RID, DIS_AV_, pRivrID, plZSAdj, Start, segLength)

# River processing ------------------------------------------------------------

caseDist <- as.numeric(st_distance(cases_sf, rivers))
caseRiverDF <- data.frame(list("CaseID" = cases_sf$CaseID,
                               Distance = caseDist,
                               Cases = cases_sf$Cases,
                               RID = rep(riversIDs$RID, each = nrow(cases_sf)),
                               Discharge = rep(riversIDs$DIS_AV_, each = nrow(cases_sf)),
                               pRivrID = rep(riversIDs$pRivrID, each = nrow(cases_sf)),
                               plZSAdj = rep(riversIDs$plZSAdj, each = nrow(cases_sf)),
                               Start = rep(riversIDs$Start, each = nrow(cases_sf)),
                               segLength = rep(riversIDs$segLength, each = nrow(cases_sf)))) %>%
  mutate(riverDistance = Distance < riverBuff)


# Summarising the case data -----------------------------------------------

caseRiverSummary <- caseRiverDF %>%
  filter(Distance < riverBuff) %>%
  group_by(CaseID) %>%
  mutate(Count = n(),
         CaseAdj = Cases / Count) %>%
  ungroup() %>%
  group_by(RID) %>%
  summarise(CasesAdjTotal = sum(CaseAdj))

riversIDsSummary <- rivers %>%
  dplyr::select(RID, DIS_AV_, pRivrID, plZSAdj, Start, segLength) %>%
  left_join(caseRiverSummary) %>%
  mutate(CasesAdjTotal = replace_na(CasesAdjTotal, 0))



## Out of range cases
sum(cases_sf$Cases) - sum(riversIDsSummary$CasesAdjTotal)

ZSRiverOutput <- riversIDsSummary %>%
  group_by(pRivrID, plZSAdj, Start) %>%
  summarise(CasesTotal = sum(CasesAdjTotal),
            RiverLength = sum(segLength / 1000)) %>%
  mutate(casesKm = CasesTotal / RiverLength) %>%
  arrange(desc(CasesTotal))


