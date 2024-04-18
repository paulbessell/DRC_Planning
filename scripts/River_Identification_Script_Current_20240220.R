
rm(list = ls())
riverBuff <- 5000
villageBuff <- 1000
riverFlowCutoff <- 15
caseWeight <- FALSE
startYear <- 2018
finalYear <- 2022
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

rivers <- read_sf("C:/Users/paulb/Documents/FIND/Data/Waterbodies/HydroRIVERS_v10_af_shp/Bandundu_H/Processed/Edited/BAndunduH_2_PMethod_D2_Edit_Comb_3.shp") %>%
  filter(max_D > riverFlowCutoff) %>%
  mutate(RID = row_number()) %>%
  st_make_valid()

rivers <- read_sf("C:/Users/paulb/Dropbox/Paul/LSTM/Data/DRC/Expansion/Rivers/RiverExpansion_red.shp") %>%
  mutate(RID = row_number()) %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  mutate(pRiverID = RID) %>%
  filter(!ZS %in% c("Oshwe", "Bosobe"))

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
  mutate(Rank = row_number()) %>%
  mutate(Status2024 = ifelse(nStatus %in% c("Current", "Keep"), "Planned / Ongoing", "None"))
# View(rivers_output)


weightedCasekmPlot <- ggplot(rivers_output %>%
         filter(!is.na(weightedCaseskm), Rank <= 50), aes(x = Rank, y = weightedCaseskm, fill = Status2024)) + 
  xlim(c(0, 50)) +
  geom_col() +
  scale_fill_discrete(name = "VC Status") +
  theme(legend.position = "bottom") +
  geom_text(aes(label = ZS), angle = 90, hjust = 0) +
  scale_x_continuous(breaks = 1:50) + ylab("Weighted cases per km")


weightedCasePlot <- ggplot(rivers_output %>%
                               filter(!is.na(wCases), Rank <= 50), aes(x = Rank, y = wCases, fill = Status2024)) + 
  xlim(c(0, 50)) +
  geom_col() +
  scale_fill_discrete(name = "VC Status") +
  theme(legend.position = "bottom") +
  geom_text(aes(label = ZS), angle = 90, hjust = 0) +
  scale_x_continuous(breaks = 1:50) + ylab("Weighted cases")

ggarrange(weightedCasekmPlot, weightedCasePlot, nrow = 2, common.legend = T)
ggsave(file = "C:/Users/paulb/Dropbox/Paul/LSTM/Plots/Expansion/TE4 Plans/CStatusPlots.jpg", width = 10, height = 8, units = "in")

st_write(rivers_output,  dsn = "C:/Users/paulb/Dropbox/Paul/LSTM/Data/DRC/Expansion/Rivers/TE4/BAndunduStatus_2022.shp", driver = "ESRI Shapefile", append=FALSE)
rivers <- read_sf("C:/Users/paulb/Dropbox/Paul/LSTM/Data/DRC/Expansion/Rivers/RiverExpansion_red.shp") %>%
  mutate(RID = row_number()) %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  mutate(pRiverID = RID)




case_post_2018 <- cases_2000_2022 %>%
  filter(Year >= startYear) %>%
  group_by(Latitude, Longitude) %>%
  mutate(weightCase = Cases * ((Year + 1 - startYear) / tYears)) %>%
  summarise(Cases = sum(Cases),
            weightCase = sum(weightCase))

write_excel_csv(case_post_2018, file = "data/Cases_Post_2018.csv", na = "")

