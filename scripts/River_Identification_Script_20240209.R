
rm(list = ls())
riverBuff <- 5000
villageBuff <- 1000
riverFlowCutoff <- 15
caseWeight <- FALSE
startYear <- 2018
finalYear <- 2022
tYears <- finalYear - startYear +1
caseThreshold <- 5 ### Add someting to look at case density


library(sf)
library(tidyverse)
library(spdep)
library(raster)
library(readr)



# Compiled data -----------------------------------------------------------

# Case data
cases_2000_2022 <- read_csv("data/Cases_Compiled_2000_2022.csv")

cases_sf <- cases_2000_2022 %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# ZS - could consider using a more upto date zs set
zs <- read_sf("C:/Users/paulb/Documents/FIND/Countries/DRC/Data/Spatial_Data/Shapefiles/ZS files/RDC_Zones de santé.shp") %>% st_make_valid() # Option for a new set

zs <- read_sf("C:/Users/paul/Documents/FIND/Countries/DRC/Data/Spatial_Data/Shapefiles/ZS NOUVEAU DECOUPAGE/drc_hz_border_Ed_Area.shp")

# AS
as <- read_sf("C:\\Users\\paul\\Documents\\FIND\\Countries\\DRC\\Data\\UCLA-PNLTHA Bandundu Village Lists\\BANDUNDU MERGE\\BANDUNDU HA MERGE 2.15.17", "BANDUNDU_HA_MERGE_02_15_17") %>%
  st_transform(crs = 4326)

rivers <- read_sf("C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/Edited/BAndunduH_2_PMethod_D2.shp") %>%
  filter(max_D > riverFlowCutoff) %>%
  mutate(RID = row_number()) %>%
  st_make_valid()

rivers$segLength <- as.numeric(st_length(rivers))


load("Images/Current_Rivers.RData")


# River processing ------------------------------------------------------------



caseDist <- st_distance(cases_sf, rivers)


# Rivers within buffer distance -------------------------------------------

caseBufferDist <- apply(caseDist, 1, FUN = function(x)(which(x < riverBuff)))

# Expanding the list
caseLength <- sapply(caseBufferDist, function(x) length(x))
caseRep <- rep(1:length(caseBufferDist), times = caseLength)
riversIDs <- unlist(caseBufferDist)
caseRiverDF <- data.frame(list("CaseID" = caseRep, "RiverIDs" = riversIDs))
caseRiverDF$pRiverID <- rivers$pRiverID[caseRiverDF$RiverIDs]
caseRiverDF$RID <- rivers$RID[caseRiverDF$RiverIDs]
caseRiverDFBind <- caseRiverDF %>%
  bind_cols(cases_sf[caseRiverDF$CaseID,])


# Getting totals ----------------------------------------------------------

caseRiverDFBindRed <- caseRiverDFBind %>%
  filter(Year >= startYear) %>%
  mutate(weightCase = Cases * ((Year + 1 - startYear) / tYears))

caseRiverTotals <- caseRiverDFBindRed %>%
  group_by(pRiverID, RID) %>%
  summarise(Cases = sum(Cases),
            wCases = sum(weightCase)) %>%
  arrange(desc(wCases))

cRiver <- caseRiverTotals[1,]

rivers_output <- rivers %>%
  mutate(included = FALSE,
         Cases = NA,
         wCases = NA)

cases_output <- caseRiverDFBindRed %>%
  mutate(included = FALSE)

cases_list <- array(dim = 0)
pRiverID_list <- array(dim = 0)
rid_list <- array(dim = 0)  

cTest <- TRUE

while(cTest){
  
  caseRiverTotals <- caseRiverDFBindRed %>%
    filter(!CaseID %in% cases_list) %>%
    group_by(pRiverID, RID) %>%
    summarise(Cases = sum(Cases),
              wCases = sum(weightCase)) %>%
    arrange(desc(wCases))
  
  cRiver <- caseRiverTotals[1,]
  
  rivers_output$included[rivers_output$RID == cRiver$RID] <- TRUE
  rivers_output$Cases[rivers_output$RID == cRiver$RID] <- cRiver$Cases
  rivers_output$wCases[rivers_output$RID == cRiver$RID] <- cRiver$wCases
  
  cases_list <- c(cases_list, cases_output$CaseID[cases_output$RID %in% cRiver$RID])
  pRiverID_list <- c(pRiverID_list, cRiver$pRiverID)
  rid_list <- c(rid_list, cRiver$RID)
  
  cTest <- cRiver$Cases >= caseThreshold
  
}
rivers_output <- rivers_output %>%
  mutate(segLength = segLength / 1000,
         caseskm = Cases / segLength,
         weightedCaseskm = wCases / segLength) %>%
  arrange(desc(weightedCaseskm)) %>%
  mutate(Rank = row_number())
View(rivers_output)


