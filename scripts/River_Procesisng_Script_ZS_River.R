

# Starts on River_Processing_Script

riverZS <- rivers_group %>%
  group_by(pRivrID, plZSAdj, Controls) %>%
  summarise(minDis = min(DIS_AV_),
            maxDis = max(DIS_AV_),
            meanDis = mean(DIS_AV_),
            Length = sum(segLength)) %>%
  ungroup() %>%
  mutate(SegmentID = row_number())


# caseDist <- st_distance(cases_sf, rivers_intersection)
caseDist <- st_distance(cases_sf, riverZS)


# Rivers within buffer distance -------------------------------------------

caseBufferDist <- apply(caseDist, 1, FUN = function(x)(which(x < riverBuff)))

# Expanding the list
caseLength <- sapply(caseBufferDist, function(x) length(x))
caseRep <- rep(1:length(caseBufferDist), times = caseLength)
riversIDs <- unlist(caseBufferDist)
caseRiverDF <- data.frame(list("CaseID2" = caseRep, "RiverIDs" = riversIDs))
caseRiverDF$SegmentID <- riverZS$SegmentID[caseRiverDF$RiverIDs]
# caseRiverDF$RID2 <- rivers_intersection$RID2[caseRiverDF$RiverIDs]
caseRiverDFBind <- caseRiverDF %>%
  bind_cols(cases_sf[caseRiverDF$CaseID2,] %>% st_drop_geometry()) %>%
  left_join(riverZS %>% st_drop_geometry(), by = "SegmentID")


# Getting totals ----------------------------------------------------------

caseRiverDFBindRed <- caseRiverDFBind %>%
  filter(Year >= startYear) %>%
  mutate(weightCase = Cases * ((Year + 1 - startYear) / tYears))

caseRiverTotals <- caseRiverDFBindRed %>%
  group_by(SegmentID, pRivrID, plZSAdj) %>%
  summarise(Cases = sum(Cases),
            wCases = sum(weightCase)) %>%
  arrange(desc(wCases))

cRiver <- caseRiverTotals[1,]

rivers_output <- riverZS %>%
  mutate(included = FALSE,
         Cases = NA,
         wCases = NA)

cases_output <- caseRiverDFBindRed %>%
  mutate(included = FALSE)

cases_list <- array(dim = 0)
segmentID_list <- array(dim = 0)
pRivrID_list <- array(dim = 0)  


### Ignore this



# 
# cases_list <- c(cases_list, cases_output$CaseID[cases_output$RID2 %in% cRiver$RID2])
# rid_list <- c(rid_list, cRiver$RID)
# rid2_list <- c(rid2_list, cRiver$RID2)

cTest <- TRUE

while(cTest){
  
  caseRiverTotals <- caseRiverDFBindRed %>%
    filter(!CaseID %in% cases_list) %>%
    group_by(SegmentID, pRivrID, plZSAdj) %>%
    summarise(Cases = sum(Cases),
              wCases = sum(weightCase)) %>%
    arrange(desc(wCases))
  
  cRiver <- caseRiverTotals[1,]
  
  rivers_output$included[rivers_output$SegmentID == cRiver$SegmentID] <- TRUE
  rivers_output$Cases[rivers_output$SegmentID == cRiver$SegmentID] <- cRiver$Cases
  rivers_output$wCases[rivers_output$SegmentID == cRiver$SegmentID] <- cRiver$wCases
  
  cases_list <- c(cases_list, cases_output$CaseID[cases_output$SegmentID %in% cRiver$SegmentID])
  pRivrID_list <- c(pRivrID_list, cRiver$pRivrID)
  segmentID_list <- c(segmentID_list, cRiver$SegmentID)
  
  cTest <- cRiver$Cases >= caseThreshold
  
}

##Now need to reaggregate to the River ZS level

