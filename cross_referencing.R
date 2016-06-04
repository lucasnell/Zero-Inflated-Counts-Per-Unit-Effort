# Raw R code chunks for 'cross_referencing.Rmd'



## @knitr libraries
library("dplyr")
library("tidyr")
library("stringr")
library("readr")
library('readxl')


## @knitr read_masterObs
masterObs <- read_csv('masterObs.csv', col_types = 'cDiiddc')

dropped_vTagIDs <- read_csv('dropped_vTagIDs.csv')

PITs_w_vTag <- read_csv('PITs_w_vTag.csv')



## @knitr newest_fun

new_vTagID <- function(focal_vTagID){
    if (focal_vTagID %in% dropped_vTagIDs$dropped){
        out_vTagID <- dropped_vTagIDs$new[dropped_vTagIDs$dropped == focal_vTagID]
    } else {
        out_vTagID <- focal_vTagID
    }
    return(as.integer(out_vTagID))
}
# Vectorized version
vec_new_vTagID <- Vectorize(new_vTagID, 'focal_vTagID', USE.NAMES = FALSE)

# Below isn't really necessary bc the only df with `PIT_Tag` column doesn't have any rows
#  with non-`NA` `PIT_Tag` but `NA` `vTagID`
get_vTagID <- function(focal_PIT_Tag){
    out_vTagID <- PITs_w_vTag$vTagID[PITs_w_vTag$PIT_Tag == focal_PIT_Tag]
    if (length(out_vTagID) == 0){
        return(NA)
    }
    return(out_vTagID)
}
# Vectorized version
vec_get_vTagID <- Vectorize(get_vTagID, 'focal_PIT_Tag', USE.NAMES = FALSE)


## @knitr read_Others

Eglin <- read_excel(paste0('~/Google Drive/Gulf sturgeon/Adam/',
                  'Copy of Eglin_List_InspectbyAdam.xlsx'), sheet = 2) %>% 
    rename(vTagID_1 = V_TagID, vTagID_2 = old_V_TagID) %>%
    select(vTagID_1, vTagID_2, Tag_description) %>%
    rowwise %>%
    mutate(Year = sapply(Tag_description, function(x){
        as.numeric(tail(strsplit(x, ' ')[[1]], 1))})) %>%
    ungroup %>%
    select(Year, vTagID_1, vTagID_2) %>%
    gather(vTagID_name, vTagID, starts_with('vTagID')) %>%
    select(-vTagID_name) %>%
    mutate(vTagID = vec_new_vTagID(vTagID))



allGS <- read_excel(paste0('~/Google Drive/Gulf sturgeon/Adam/', 
                  'Copy of All_GS_transmitters_PCFRO_AdamInspect.xlsx'), na = 'NA', 
           col_types = c('text', 'date', rep('text', 4))) %>%
    rename(vTagID = Tag, PIT_Tag = `PIT  Tag`) %>%
    mutate(vTagID = as.numeric(gsub(' \\(F\\)', '', vTagID)),
           Site = paste(gsub(' ', '', River), 'River')) %>%
    mutate(vTagID = vec_new_vTagID(vTagID)) %>%
    filter(!is.na(PIT_Tag) | !is.na(vTagID)) %>%
    select(Site, Date, vTagID, PIT_Tag)




rec_vTagIDs <- read_rds(paste0('~/Google Drive/Gulf sturgeon/Compilation/',
                               'filtered_detections.rds')) %>% 
    as.tbl %>%
    mutate(Date = as.Date(paste(Month, Day, Year, sep = '-'), format = '%m-%d-%Y')) %>%
    rename(vTagID = Transmitter) %>%
    select(River, Date, Receiver, vTagID) %>%
    mutate(vTagID = vec_new_vTagID(vTagID)) %>%
    arrange(River, Date, Receiver, vTagID)



## @knitr comp_Eglin

# Data frame to compare to Eglin
EglinComp <- masterObs %>% 
    mutate(Year = format(Date, '%Y') %>% as.numeric) %>%
    select(Year, vTagID) %>% 
    group_by(Year, vTagID) %>%
    summarize(total = n()) %>%
    ungroup

Eglin2 <- Eglin %>% 
    group_by(Year, vTagID) %>%
    summarize(total = n()) %>%
    ungroup


# I don't see any reason to go any further...
EglinComp %>% 
    filter(vTagID %in% Eglin2$vTagID, 
           Year %in% Eglin2$Year)

Eglin2 %>%
    filter(Year == 2010, vTagID == 46441)

# Unique vTagIDs not in masterObs
Eglin$vTagID[! Eglin$vTagID %in% masterObs$vTagID] %>% unique

Eglin$Year %>% unique
EglinComp$Year %>% unique






## @knitr comp_allGS


allGS %>% select(Site) %>% unique
masterObs %>% select(Site) %>% unique

allGS_comp <- allGS %>%
    mutate(Date_vTagID = paste(Date, vTagID, sep = '_'))

master_comp <- masterObs %>% 
    mutate(Date_vTagID = paste(Date, vTagID, sep = '_'))

# Rows in allGS not in masterObs
write_csv(allGS[! allGS_comp$Date_vTagID %in% master_comp$Date_vTagID, ], 
          'allGS_rows_notInMaster.csv')

# Unique vTagIDs not in masterObs
allGS$vTagID[! allGS$vTagID %in% masterObs$vTagID] %>% unique




## @knitr comp_rec_vTagIDs

rec_vTagIDs

rec_comp <- rec_vTagIDs %>%
    mutate(Date_vTagID = paste(Date, vTagID, sep = '_'))

master_comp <- masterObs %>% 
    mutate(Date_vTagID = paste(Date, vTagID, sep = '_'))

rec_vTagIDs[! rec_comp$Date_vTagID %in% master_comp$Date_vTagID, ]

# Rows in rec_vTagIDs not in masterObs
write_csv(rec_vTagIDs[! rec_comp$Date_vTagID %in% master_comp$Date_vTagID, ], 
          'filtDetect_rows_notInMaster.csv')


length(unique(rec_vTagIDs$vTagID)[! unique(rec_vTagIDs$vTagID) %in% masterObs$vTagID])
length(unique(rec_vTagIDs$vTagID))




# Unique vTagIDs and whether or not they're present in masterObs
inMaster <- data.frame(vTagID = unique(rec_vTagIDs$vTagID),
                       in_master = unique(rec_vTagIDs$vTagID) %in% masterObs$vTagID) %>%
    as.tbl



(24746 / 24770) * 100

