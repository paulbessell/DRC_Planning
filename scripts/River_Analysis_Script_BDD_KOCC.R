
#rm(list = ls()) riverBuff <- params$RiverBuffer
villageBuff <- 1000
riverFlowCutoff <- ifelse(exists("params"), params$`Debit minimal du riviere`, 15)
caseWeight <- FALSE
startYear <- ifelse(exists("params"), params$`Debut annee de analyse`, 2019)
finalYear <- 2023
tYears <- finalYear - startYear +1
caseThreshold <- ifelse(exists("params"), params$`Total du cas minimal`, 5) ### Add someting to look at case density
riverBuff <- ifelse(exists("params"), params$`Tampon du riviere`, 5000)
writeOutput <- ifelse(exists("params"), params$`Ecrire des fichiers a afficher`, FALSE)


library(sf)
library(tidyverse)
library(spdep)
#library(raster)
library(readr)
# library(ggpubr)



# Compiled data -----------------------------------------------------------

# Case data
lFiles <- list.files("data/CaseData", recursive = F, include.dirs = F, full.names = T)
lFiles <- lFiles[grepl(".csv", lFiles)]

cases <- do.call(bind_rows, lapply(lFiles, read_csv)) %>%
  mutate(cCaseID = row_number())

load("data/RImages/distdfsort2_5k.RData")

cases$ClusterID <- distdfsort2_5k$GroupID[match(cases$cCaseID, distdfsort2_5k$xcaseID)]


cases_sf <- cases %>%
  filter(!is.na(Longitude), 
         Year %in% startYear:finalYear) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(CaseID = row_number(),
        Weight = (Year - (startYear - 1)) / tYears)

# ZS

zs <- read_sf("data/Spatial/ZS/RDC_Zones_De_Sante_Repair.shp") %>% 
  st_make_valid() %>%
  mutate("ZS" = Nom,
         PROVINCE = recode(PROVINCE, "MAI NDOMBE" = "Mai Ndombe"))

provinces <- read_sf("data/Spatial/Provinces/Provinces_2.shp")

# Rivers ------------------------------------------------------------------

# rivers <- st_read("C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/BAndunduH_ZS_2_PMethod_5Cutoff.shp")

rivers <- st_read("data/Spatial/Rivers/bddKasai_ZS_2_Method_5CutOff_AZS_Dist.shp", options = "ENCODING=WINDOWS-1252") %>%
  mutate(SegmentID = row_number(),
         segLength = as.numeric(st_length(.)),
         RID = row_number(),
         fProvince = recode(fProvince, "MAI NDOMBE" = "Mai Ndombe"))

# interventionRivers <- rivers %>%
#  filter(!is.na(Start))

rivers <- rivers %>%
  filter(DIS_AV_ > riverFlowCutoff | (Start != 0))

riversIDs <- rivers %>%
  st_drop_geometry() %>%
  dplyr::select(RID, DIS_AV_, pRivrID, plZSAdj, Start, segLength)

# River processing ------------------------------------------------------------

caseDist <- as.numeric(st_distance(cases_sf, rivers))
caseRiverDF <- data.frame(list("CaseID" = cases_sf$CaseID,
                               Distance = caseDist,
                               Cases = cases_sf$Cases,
                               Weight = cases_sf$Weight,
                               RID = rep(riversIDs$RID, each = nrow(cases_sf)),
                               Discharge = rep(riversIDs$DIS_AV_, each = nrow(cases_sf)),
                               pRivrID = rep(riversIDs$pRivrID, each = nrow(cases_sf)),
                               plZSAdj = rep(riversIDs$plZSAdj, each = nrow(cases_sf)),
                               Start = rep(riversIDs$Start, each = nrow(cases_sf)),
                               segLength = rep(riversIDs$segLength, each = nrow(cases_sf)))) %>%
  mutate(riverDistance = Distance < riverBuff)


# Summarising the case data -----------------------------------------------

caseRiverSummaryDist <- caseRiverDF %>%
  filter(Distance < riverBuff) %>%
  group_by(CaseID) %>%
  mutate(Count = n(),
         CaseAdj = Cases / Count,
         WeightAdj = Weight / Count) %>%
  ungroup() 

caseRiverSummary <- caseRiverSummaryDist %>%
  group_by(RID) %>%
  summarise(CasesAdjTotal = sum(CaseAdj),
            WeightAdjTotal = sum(WeightAdj))


# Cluster candidates ------------------------------------------------------
# These can be used for community clusters
candidateClusters <- cases_sf$CaseID[!cases_sf$CaseID %in% caseRiverSummaryDist$CaseID]
cases_sf_cluster <- cases_sf %>%
  filter(CaseID %in% candidateClusters) %>%
  filter(!is.na(ClusterID) | Cases >= caseThreshold) %>%
  group_by(ClusterID) %>%
  mutate(ClusterCases = sum(Cases),
         ClusterWeight = sum(Weight)) %>%
  filter(ClusterCases > caseThreshold) %>%
  group_by(geometry) %>%
  mutate(Cases = sum(Cases),
         Weight = sum(Weight)) %>%
  slice_max(row_number()) %>%
  st_join(zs %>%
            dplyr::select(PROVINCE, ZS)) %>%
  mutate(pVillage = ifelse(is.na(pVillage), Village, pVillage)) %>%
  group_by(ClusterID) %>%
  mutate("nVillages" = n()) %>%
  dplyr::select(-Province)

if(writeOutput) st_write(cases_sf_cluster, "output/sites_communitaire/Sites_communitaire.shp", delete_layer = T)

clusterSummarise <- cases_sf_cluster %>%
  st_drop_geometry() %>%
  group_by(ClusterID) %>%
  slice_max(row_number()) %>%
  arrange(desc(ClusterCases))


riversIDsSummary <- rivers %>%
  dplyr::select(RID, DIS_AV_, pRivrID, plZSAdj, fProvince, Start, segLength) %>%
  left_join(caseRiverSummary) %>%
  mutate(CasesAdjTotal = replace_na(CasesAdjTotal, 0),
         WeightAdjTotal = replace_na(WeightAdjTotal, 0))



## Out of range cases
sum(cases_sf$Cases) - sum(riversIDsSummary$CasesAdjTotal)

ZSRiverOutput <- riversIDsSummary %>%
  group_by(pRivrID, plZSAdj, fProvince, Start) %>%
  summarise(Discharge = min(DIS_AV_, na.rm = T),
            CasesTotal = sum(CasesAdjTotal),
            WeightTotal = sum(WeightAdjTotal),
            RiverLength = sum(segLength / 1000)) %>%
  mutate(casesKm = CasesTotal / RiverLength,
         WeightKm = WeightTotal / RiverLength) %>%
  arrange(desc(WeightKm)) %>%
  mutate(Status = "New site",
         Status = ifelse(Start > 0, "Current intervention", Status),
         Status = ifelse(CasesTotal < caseThreshold & Start == 0, "Non-candidate", Status))

if(writeOutput) st_write(ZSRiverOutput, "output/rivieres/Rivers_Target.shp", delete_layer = T)
