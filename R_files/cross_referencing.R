# Raw R code chunks for 'cross_referencing.Rmd'



## @knitr libraries
library("dplyr")
library("tidyr")
library("stringr")
library("readr")
library('readxl')



## @knitr read_ID_objects
masterCaps <- read_csv('../csv_out/masterCaps.csv', col_types = 'cDiiddc')
dropped_vTagIDs <- read_csv('../csv_out/dropped_vTagIDs.csv')
PITs_w_vTag <- read_csv('../csv_out/PITs_w_vTag.csv')





## @knitr newest_vTagID
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


## @knitr vTagID_from_PIT
get_vTagID <- function(focal_PIT_Tag){
    out_vTagID <- PITs_w_vTag$vTagID[PITs_w_vTag$PIT_Tag == focal_PIT_Tag]
    if (length(out_vTagID) == 0){
        return(NA)
    }
    return(out_vTagID)
}
# Vectorized version
vec_get_vTagID <- Vectorize(get_vTagID, 'focal_PIT_Tag', USE.NAMES = FALSE)







## @knitr read_Eglin
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



## @knitr read_allGS
allGS <- read_excel(paste0('~/Google Drive/Gulf sturgeon/Adam/', 
                           'Copy of All_GS_transmitters_PCFRO_AdamInspect.xlsx'), na = 'NA', 
                    col_types = c('text', 'date', rep('text', 4))) %>%
    rename(vTagID = Tag, PIT_Tag = `PIT  Tag`) %>%
    mutate(vTagID = as.numeric(gsub(' \\(F\\)', '', vTagID)),
           Site = paste(gsub(' ', '', River), 'River')) %>%
    mutate(vTagID = vec_new_vTagID(vTagID)) %>%
    filter(!is.na(PIT_Tag) | !is.na(vTagID)) %>%
    select(Site, Date, vTagID, PIT_Tag)


## @knitr read_filtered_detections
filtDet <- read_rds(paste0('~/Google Drive/Gulf sturgeon/Compilation/',
                           'filtered_detections.rds')) %>% 
    as.tbl %>%
    mutate(Date = as.Date(paste(Month, Day, Year, sep = '-'), format = '%m-%d-%Y')) %>%
    rename(vTagID = Transmitter) %>%
    select(River, Date, Receiver, vTagID) %>%
    mutate(vTagID = vec_new_vTagID(vTagID)) %>%
    arrange(River, Date, Receiver, vTagID)










## @knitr comp_Eglin_makeDFs
masterCaps_byYear <- masterCaps %>% 
    mutate(Year = format(Date, '%Y') %>% as.numeric) %>%
    select(Year, vTagID) %>% 
    group_by(Year, vTagID) %>%
    summarize(total = n()) %>%
    ungroup %>%
    mutate(Year_vTagID = paste(Year, vTagID, sep = '_'))
Eglin_byYear <- Eglin %>% 
    group_by(Year, vTagID) %>%
    summarize(total = n()) %>%
    ungroup %>%
    mutate(Year_vTagID = paste(Year, vTagID, sep = '_'))

## @knitr comp_Eglin_sharedRow
Eglin_byYear %>% 
    filter(Year_vTagID %in% masterCaps_byYear$Year_vTagID) %>%
    as.data.frame
masterCaps_byYear %>% 
    filter(Year_vTagID %in% Eglin_byYear$Year_vTagID) %>% 
    as.data.frame

## @knitr comp_Eglin_vTagIDs_notInMaster
Eglin_vTagIDs_notInMaster <- unique(Eglin$vTagID)[! unique(Eglin$vTagID) 
                                                  %in% masterCaps$vTagID]
# unique vTagIDs in `Eglin` but not in master
length(Eglin_vTagIDs_notInMaster)
# total unique vTagIDs present in `Eglin`
length(unique(Eglin$vTagID))














## @knitr comp_allGS_makeDFs

allGS_comp <- allGS %>%
    mutate(Date_vTagID = paste(Date, vTagID, sep = '_'))
master_comp <- masterCaps %>% 
    mutate(Date_vTagID = paste(Date, vTagID, sep = '_'))

## @knitr comp_allGS_rows_notInMaster
allGS_comp %>% 
    filter(! Date_vTagID %in% master_comp$Date_vTagID)

## @knitr comp_allGS_vTagIDs_notInMaster
allGS_vTagIDs_notInMaster <- unique(allGS_comp$vTagID)[! unique(allGS_comp$vTagID) 
                                                       %in% masterCaps$vTagID]
# unique vTagIDs in `allGS` but not in master
length(allGS_vTagIDs_notInMaster)
# total unique vTagIDs present in `allGS`
length(unique(allGS_comp$vTagID))



















## @knitr comp_filtDet
filtDet

# Rows in filtDet where vTagID not in masterCaps
filtDet %>% 
    filter(! vTagID %in% masterCaps$vTagID)


# Number of unique vTagIDs in `filtDet` that are present in `masterCaps`
length(unique(filtDet$vTagID)[! unique(filtDet$vTagID) %in% masterCaps$vTagID])
# Total number of unique vTagIDs in `filtDet`
length(unique(filtDet$vTagID))

# Number of unique vTagIDs in `masterCaps` that are present in `filtDet`
length(unique(masterCaps$vTagID)[ unique(masterCaps$vTagID) %in% filtDet$vTagID])
# Total number of unique vTagIDs in `masterCaps`
length(unique(masterCaps$vTagID))


# Unique vTagIDs and whether or not they're present in masterCaps
filtDet_inMaster <- filtDet %>% 
    group_by(vTagID) %>%
    summarize(Rivers = River %>% unique %>% paste(., collapse = ':'),
              inMaster = (vTagID %in% masterCaps$vTagID)[[1]]) %>%
    arrange(inMaster, Rivers, vTagID) %>%
    select(inMaster, Rivers, vTagID)


# write_csv(filtDet_inMaster, '../csv_out/filtDet_inMaster.csv')




