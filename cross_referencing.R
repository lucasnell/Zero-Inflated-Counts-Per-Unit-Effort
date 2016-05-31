# Raw R code chunks for 'cross_referencing.Rmd'



## @knitr libraries
library("dplyr")
library("tidyr")
library("stringr")
library("readr")
library('readxl')


## @knitr read_masterObs
masterObs <- read_csv('masterObs.csv')




## @knitr read_Others

Eglin <- read_excel(paste0('~/Google Drive/Gulf sturgeon/Adam/',
                  'Copy of Eglin_List_InspectbyAdam.xlsx'), sheet = 2) %>% 
    rename(vTagID_1 = V_TagID, vTagID_2 = old_V_TagID) %>%
    select(vTagID_1, vTagID_2, Tag_description) %>%
    rowwise %>%
    mutate(Year = strsplit(Tag_description, ' ')[[1]] %>% tail(., 1) %>% as.numeric) %>%
    ungroup %>%
    select(Year, vTagID_1, vTagID_2) %>%
    gather(vTagID_name, vTagID, starts_with('vTagID')) %>%
    select(-vTagID_name)

allGS <- read_excel(paste0('~/Google Drive/Gulf sturgeon/Adam/', 
                  'Copy of All_GS_transmitters_PCFRO_AdamInspect.xlsx'), na = 'NA', 
           col_types = c('text', 'date', rep('text', 4))) %>%
    rename(vTagID = Tag, PIT_Tag = `PIT  Tag`) %>%
    select(Date, River, vTagID, PIT_Tag) %>%
    mutate(vTagID = as.numeric(gsub(' \\(F\\)', '', vTagID)))


rec_vTagIDs <- read_rds(paste0('~/Google Drive/Gulf sturgeon/Compilation/',
                               'filtered_detections.rds')) %>% 
    as.tbl %>%
    mutate(Date = as.Date(paste(Month, Day, Year, sep = '-'), format = '%m-%d-%Y')) %>%
    select(Date, Receiver, Transmitter, River)


paste0(rec_vTagIDs$Date[c(1, 200, 500, 5000)], collapse = '", "')

## @knitr comp_Eglin

Eglin

# Data frame to compare to Eglin
EglinComp <- masterObs %>% 
    mutate(Year = format(Date, '%Y') %>% as.numeric) %>%
    select(Year, vTagID)

Eglin %>% distinct(Year, vTagID)













