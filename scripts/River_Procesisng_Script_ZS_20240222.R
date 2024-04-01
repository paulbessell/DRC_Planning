
library(sf)
library(tidyverse)
library(overedge)
riverFlowCutoff <- 15
riverFlowCutoff <- 5
min_segments <- 3
min_chunk_length <- 20000


zs <- read_sf("C:/Users/paul/Documents/FIND/Countries/DRC/Data/Spatial_Data/Shapefiles/ZS NOUVEAU DECOUPAGE/drc_hz_border_Ed_Area.shp")

zs <- read_sf("C:/Users/paulb/Documents/FIND/Countries/DRC/Data/Spatial_Data/Shapefiles/zone_ste_puc/Zone_Sté_Puc.shp") %>% 
  st_make_valid() %>%
  st_transform(4326) %>%
  mutate("ZS" = NOM_ZS)


rivers <- read_sf("C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/BAndunduH_2.shp") %>%
  st_make_valid() %>%
  filter(DIS_AV_CMS > riverFlowCutoff) %>%
  mutate(pRiverID = NA,
         pComposite = ORD_STRA * 100 + ORD_CLAS * 10 + ORD_FLOW,
         pSequence = NEXT_DOWN %in% .$HYRIV_ID)

write_sf(rivers, dsn = "C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/BAndunduH_PreJoin.shp", driver = "ESRI Shapefile", append = F)

riversZS <- rivers %>%
  st_join(zs %>% dplyr::select(ZS), largest = T)

counter <- 1
# pZSm1 <- "" # Previous ZS minus 1
# pZS <- "" # Previous ZS
for(i in 1:nrow(riversZS)){
  cGroup <- array(dim = 0)
  cRiver <- riversZS[i,]

  cGroup <- c(cGroup, cRiver$HYRIV_ID)
  if(is.na(cRiver$pRiverID)){
    finished <- FALSE

    while(!finished & cRiver$pSequence){
      nextRiver <- riversZS[riversZS$HYRIV_ID == cRiver$NEXT_DOWN,]
      if(nextRiver$pComposite == cRiver$pComposite){
        cGroup <- c(cGroup, nextRiver$HYRIV_ID)
        cRiver <- rivers[riversZS$HYRIV_ID == cRiver$NEXT_DOWN,]
        
      }
      else finished <- TRUE
    }  
    
    riversZS$pRiverID[riversZS$HYRIV_ID %in% cGroup] <- counter 
    
    counter <- max(riversZS$pRiverID, na.rm = T) + 1
  }
}

riversZS <- riversZS %>%
#  arrange(pRiverID, DIS_AV_CMS) %>% # Sort by length of time in each ZS
  arrange(pRiverID, DIST_UP_KM) %>% # Sort by length of time in each ZS
  mutate(RID = row_number())

riversZS <- riversZS %>%
  group_by(pRiverID) %>%
  fill(ZS, .direction = "downup") %>%
  filter(!is.na(ZS))
riversZS$segLength <- st_length(riversZS)

riversZS$paulZS <- riversZS$ZS
group_list <- unique(riversZS$pRiverID)

for(i in 1:length(group_list)){
  cGroup <- group_list[i]
  cRiver <- riversZS[riversZS$pRiverID == cGroup,]
  if(length(unique(cRiver$ZS)) > 1){
    pZS <- cRiver$ZS[1]
    marker <- 1
    cCounter <- 0
    cZS <- ""
    for(k in  2:nrow(cRiver)){

      if(cRiver$ZS[k] != pZS){
        cCounter <- cCounter + 1
        cZS <- cRiver$ZS[k]
        
        if(sum(cZS %in% cRiver$ZS[1:(k-1)]) > 0){
          if(sum(cRiver$segLength[cRiver$ZS == pZS]) > sum(cRiver$segLength[cRiver$ZS == cZS]))         cRiver$paulZS[cRiver$ZS == cZS] <- pZS
          if(sum(cRiver$segLength[cRiver$ZS == pZS]) < sum(cRiver$segLength[cRiver$ZS == cZS])) cRiver$paulZS[cRiver$ZS == pZS] <- cZS
             
          }
        pZS <- cRiver$ZS[k]
        marker <- k
        
        }
        
      }
      
        
  }
  riversZS$paulZS[riversZS$pRiverID == cGroup] <- cRiver$paulZS
  }

riversZS <- riversZS %>%
  group_by(cID = consecutive_id(ZS, pRiverID)) %>%
  mutate(cID = cID) %>%
  group_by(cID) %>%
  mutate(chunk_length = as.numeric(sum(segLength)),
         Segments = n()) %>%
  mutate(paulZSAdj = ifelse(chunk_length > min_chunk_length & Segments >= min_segments, ZS, paulZS))

# write_sf(riversZS, dsn = "C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/BAndunduH_ZS_2_PMethod.shp", driver = "ESRI Shapefile", append = F)
write_sf(riversZS, dsn = "C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/BAndunduH_ZS_2_PMethod_5Cutoff.shp", driver = "ESRI Shapefile", append = F)
