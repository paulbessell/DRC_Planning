
rm(list = ls())
riverBuff <- 5000
villageBuff <- 1000
riverFlowCutoff <- 15
caseWeight <- FALSE
startYear <- 2018
finalYear <- 2022
tYears <- finalYear - startYear +1
caseThreshold <- 10


library(sf)
library(tidyverse)
library(spdep)
library(raster)
library(readr)

cases_2000_2022 <- read_csv("data/Cases_Compiled_2000_2022.csv")

cases_sf <- cases_2000_2022 %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(CaseID = row_number())

zs <- read_sf("C:/Users/paul/Documents/FIND/Countries/DRC/Data/Spatial_Data/Shapefiles/ZS NOUVEAU DECOUPAGE/drc_hz_border_Ed_Area.shp")
#rivers <- read_sf("C:/Users/paul/Documents/FIND/Countries/DRC/Data/Spatial_Data/Rasters/SRTM1/TrypElim/Hydrosheds/Shapefile/streamDissMP.shp") %>% 
#  filter(GRID_CODE >=7)

as <- read_sf("C:\\Users\\paul\\Documents\\FIND\\Countries\\DRC\\Data\\UCLA-PNLTHA Bandundu Village Lists\\BANDUNDU MERGE\\BANDUNDU HA MERGE 2.15.17", "BANDUNDU_HA_MERGE_02_15_17") %>%
  st_transform(crs = 4326)

rivers <- read_sf("C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/BAndunduH_2_PMethod_D2.shp") %>%
  filter(max_D > riverFlowCutoff) %>%
  mutate(RID = row_number()) %>%
  st_make_valid()

# targets <- read_sf("C:/Users/paulb/Dropbox/Paul/LSTM/Data/DRC/Targets/Curation/DRC_Target_Curation/outputs/Shapefile/TargetDeployment_Jun_2023.shp")

# targets_buffer <- st_buffer(targets, 1500) # These bits are painfully slow
# targets_buffer <- st_union(targets_buffer) # These bits are painfully slow

# save(targets_buffer, file = "Images/targets_Buffer.RData")  

load("Images/targets_Buffer.RData")


rivers_group <- st_read("C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/BAndunduH_ZS_2_PMethod.shp") %>%
  mutate(SegmentID = row_number())
rivers_group$segLength <- as.numeric(st_length(rivers_group))

# Clipping rivers ---------------------------------------------------------

riverClip <- rivers_group %>%
  st_intersection(targets_buffer %>%
                    st_make_valid())

save(riverClip, file = "Images/Current_Rivers.RData")

rivers_group <- rivers_group %>%
  mutate("Controls" = ifelse(SegmentID %in% riverClip$SegmentID, "Controlled", "Not controlled"))

  
# River processing --------------------------------------------------------

# zsBuffer <- st_buffer(zs, 1500)
# rivers_intersection <- rivers %>%
#   st_intersection(zsBuffer) %>%
#   mutate(RID2 = row_number())
#   
# rivers_intersection$Riv_Len <- st_length(rivers_intersection) # Use this for joining - will need to filter for duplicates - it has a part for each ZS. Need a way of identifying each controlled segment of river

# write_sf(rivers_intersection, dsn = "C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/BAndunduH_2_DI.shp", driver = "ESRI Shapefile")


# rivers_sj <- st_join(rivers_intersection, rivers_intersection)

# rivers_sj <- st_join(rivers, zsBuffer)

# Get river distances -----------------------------------------------------



# caseDist <- st_distance(cases_sf, rivers_intersection)
caseDist <- st_distance(cases_sf, rivers_group)


# Rivers within buffer distance -------------------------------------------

caseBufferDist <- apply(caseDist, 1, FUN = function(x)(which(x < riverBuff)))

# Expanding the list
caseLength <- sapply(caseBufferDist, function(x) length(x))
caseRep <- rep(1:length(caseBufferDist), times = caseLength)
riversIDs <- unlist(caseBufferDist)
caseRiverDF <- data.frame(list("CaseID" = caseRep, "RiverIDs" = riversIDs))
caseRiverDF$SegmentID <- rivers_group$SegmentID[caseRiverDF$RiverIDs]
# caseRiverDF$RID2 <- rivers_intersection$RID2[caseRiverDF$RiverIDs]
caseRiverDFBind <- caseRiverDF %>%
  bind_cols(cases_sf[caseRiverDF$CaseID,]) %>%
  left_join(rivers_group %>%
              dplyr::select(pRivrID, plZSAdj, cID, Controls, segLength, SegmentID), by = "SegmentID")


# Getting totals ----------------------------------------------------------

caseRiverDFBindRed <- caseRiverDFBind %>%
  filter(Year >= startYear) %>%
  mutate(weightCase = Cases * ((Year + 1 - startYear) / tYears))

caseRiverTotals <- caseRiverDFBindRed %>%
  group_by(RID, RID2) %>%
  summarise(Cases = sum(Cases),
            wCases = sum(weightCase)) %>%
  arrange(desc(wCases))

cRiver <- caseRiverTotals[1,]

rivers_output <- rivers_intersection %>%
  mutate(included = FALSE,
         Cases = NA,
         wCases = NA)

cases_output <- caseRiverDFBindRed %>%
  mutate(included = FALSE)

cases_list <- array(dim = 0)
rid_list <- array(dim = 0)
rid2_list <- array(dim = 0)  

# 
# cases_list <- c(cases_list, cases_output$CaseID[cases_output$RID2 %in% cRiver$RID2])
# rid_list <- c(rid_list, cRiver$RID)
# rid2_list <- c(rid2_list, cRiver$RID2)

cTest <- TRUE

while(cTest){

  caseRiverTotals <- caseRiverDFBindRed %>%
    filter(!CaseID %in% cases_list) %>%
    group_by(RID, RID2) %>%
    summarise(Cases = sum(Cases),
              wCases = sum(weightCase)) %>%
    arrange(desc(wCases))
  
  cRiver <- caseRiverTotals[1,]

  rivers_output$included[rivers_output$RID2 == cRiver$RID2] <- TRUE
  rivers_output$Cases[rivers_output$RID2 == cRiver$RID2] <- cRiver$Cases
  rivers_output$wCases[rivers_output$RID2 == cRiver$RID2] <- cRiver$wCases
  
  cases_list <- c(cases_list, cases_output$CaseID[cases_output$RID2 %in% cRiver$RID2])
  rid_list <- c(rid_list, cRiver$RID)
  rid2_list <- c(rid2_list, cRiver$RID2)
  
  cTest <- cRiver$Cases >= caseThreshold
  
}
