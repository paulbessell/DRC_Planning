
library(sf)
library(tidyverse)
library(overedge)
riverFlowCutoff <- 15

zs <- read_sf("C:/Users/paul/Documents/FIND/Countries/DRC/Data/Spatial_Data/Shapefiles/ZS NOUVEAU DECOUPAGE/drc_hz_border_Ed_Area.shp")

zs <- read_sf("C:/Users/paulb/Documents/FIND/Countries/DRC/Data/Spatial_Data/Shapefiles/zone_ste_puc/Zone_Sté_Puc.shp") %>% 
  st_make_valid() %>%
  st_transform(4326) %>%
  mutate("ZS" = NOM_ZS)

zsBuffer <- st_buffer(zs, 5000)


rivers <- read_sf("C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/BAndunduH_2.shp") %>%
  st_make_valid() %>%
  filter(DIS_AV_CMS > riverFlowCutoff) %>%
  mutate(pRiverID = NA,
         pComposite = ORD_STRA * 100 + ORD_CLAS * 10 + ORD_FLOW,
         pSequence = NEXT_DOWN %in% .$HYRIV_ID)

counter <- 1
for(i in 1:nrow(rivers)){
  cGroup <- array(dim = 0)
  cRiver <- rivers[i,]
  cGroup <- c(cGroup, cRiver$HYRIV_ID)
  if(is.na(cRiver$pRiverID)){
    finished <- FALSE
    while(!finished & cRiver$pSequence){
      nextRiver <- rivers[rivers$HYRIV_ID == cRiver$NEXT_DOWN,]
      if(nextRiver$pComposite == cRiver$pComposite){
        cGroup <- c(cGroup, nextRiver$HYRIV_ID)
        cRiver <- rivers[rivers$HYRIV_ID == cRiver$NEXT_DOWN,]
      }
      else finished <- TRUE
    }  

  rivers$pRiverID[rivers$HYRIV_ID %in% cGroup] <- counter 
  
  counter <- max(rivers$pRiverID, na.rm = T) + 1
  }
}
#write_sf(rivers, dsn = "C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/BAndunduH_2_PMethod.shp", driver = "ESRI Shapefile")


riversD <- rivers %>%
  group_by(pRiverID) %>%
  summarise(min_D = min(DIS_AV_CMS),
            max_D = max(DIS_AV_CMS),
            mean_D = mean(DIS_AV_CMS))

# write_sf(riversD, dsn = "C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/BAndunduH_2_PMethod_D.shp", driver = "ESRI Shapefile", overwrite = T)





# Try a ridiculous matrix -------------------------------------------------

riversProc <- rivers %>%
  mutate(Segment_ID = row_number())
riversProc$segLength <- as.numeric(st_length(riversProc))
rivers_full_intersection <- st_intersection(riversProc, zsBuffer)
rivers_full_intersection$segLength <- as.numeric(st_length(rivers_full_intersection))
write_sf(rivers_full_intersection, dsn = "C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/Build/BAndunduH_Intersection.shp", driver = "ESRI Shapefile", overwrite = T)

rivers_full_summary <- rivers_full_intersection %>% 
  group_by(ZS, pRiverID) %>% 
  summarise(ZSLength = sum(segLength)) %>%
  group_by(pRiverID) %>%
  slice_max(ZSLength)

riversProcRed <- rivers_full_intersection %>%
  filter(paste0(ZS, pRiverID) %in% paste0(rivers_full_summary$ZS, rivers_full_summary$pRiverID))
# write_sf(riversProcRed, dsn = "C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/Build/RiverProcRed.shp", driver = "ESRI Shapefile", overwrite = T)

rivers_full_intersection_red <- rivers_full_intersection %>%
  filter(!Segment_ID %in% riversProcRed$Segment_ID)

while(nrow(rivers_full_intersection_red) > 0){
  rivers_full_summary <- rivers_full_intersection_red %>% 
    group_by(ZS, pRiverID) %>% 
    summarise(ZSLength = sum(segLength)) %>%
    group_by(pRiverID) %>%
    slice_max(ZSLength)
  
  riversProcRed <- riversProcRed %>% 
    bind_rows(rivers_full_intersection_red %>% 
                filter(paste0(ZS, pRiverID) %in% paste0(rivers_full_summary$ZS, rivers_full_summary$pRiverID)))
  rivers_full_intersection_red <- rivers_full_intersection %>%
    filter(!Segment_ID %in% riversProcRed$Segment_ID)
  
}

### This is it
riversDZS <- riversProcRed %>%
  group_by(pRiverID, ZS) %>%
  summarise(min_D = min(DIS_AV_CMS),
            max_D = max(DIS_AV_CMS),
            mean_D = mean(DIS_AV_CMS))

write_sf(riversDZS, dsn = "C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/BAndunduH_2_PMethod_D2.shp", driver = "ESRI Shapefile", overwrite = T)


# 
# # Trying to fix the gaps --------------------------------------------------
# 
# riversd_intersection <- st_intersection(riversD, zs, tolerance = 1000)
# 
# # Assigning ZS ------------------------------------------------------------
# 
# riversD$fullLength <- st_length(riversD)
# 
# riversD_intersection <- riversD %>%
#   st_intersection(zsBuffer) %>%
#   mutate(RID2 = row_number())
# 
# riversD_intersection$zsLength <- st_length(riversD_intersection)
# 
# riversD_intersection_red <- riversD_intersection %>%
#   group_by(pRiverID) %>%
#   slice_max(zsLength) %>%
#   mutate(remLength = as.numeric(fullLength) - as.numeric(zsLength))
# 
# riversD_remaining <- riversD_intersection_red %>%
#   filter(remLength < 10)
# 
# 
# 
# riversD_drop <- st_filter(riversD_intersection, riversD_intersection_red, .predicate = st_touches)
# riversD_remaining <- riversD_intersection %>%
#   filter(!RID2 %in% riversD_drop$RID2)
# 
# 
# 
# 
# 
# test <- st_erase(riversD, riversD_intersection_red)
# ?st_delete
# test <- st_intersection(riversD, riversD_intersection_red)
# test <- st_sym_difference(riversD_intersection_red, riversD)
# test <- st_buffer(riversD_intersection_red, 0)
# test2 <- st_intersection(riversD, test)
# 
# test3 <- st_buffer(riversD,0)
# test4 <- st_intersection(test3, test)
# test5 <- st_difference(riversD, test4 %>% st_make_valid())
# test6 <- test5 %>% st_join(riversD_intersection_red)
