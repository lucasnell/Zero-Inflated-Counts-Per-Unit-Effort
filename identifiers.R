# Raw R code chunks for 'identifiers.Rmd'



## @knitr libraries
library("dplyr")
library("tidyr")
library("stringr")


## @knitr sourceCompiling
source("compiling.R")



## @knitr unique_PITs_per_vTag
# Table of # of unique, non-NA PIT_Tags per vTagID
allSites %>% 
    filter(!is.na(vTagID), !is.na(PIT_Tag)) %>%
    group_by(vTagID) %>%
    summarize(uniquePITs = length(unique(PIT_Tag))) %>%
    select(uniquePITs) %>% 
    table

## @knitr unique_vTags_per_PIT
# Table of # of unique, non-NA vTagIDs per PIT_Tag
allSites %>%
    filter(!is.na(vTagID), !is.na(PIT_Tag)) %>%
    group_by(PIT_Tag) %>%
    summarize(uniquevTagIDs = length(unique(vTagID))) %>%
    select(uniquevTagIDs) %>% 
    table




## @knitr vTag_PIT_lookup_DF
vTagID_PIT <- allSites %>%
    filter(!is.na(vTagID), !is.na(PIT_Tag)) %>%
    distinct(vTagID, PIT_Tag) %>%
    select(vTagID, PIT_Tag)



## @knitr vTag_PIT_lookup_Funs
vTagID_to_PITs <- function(in_vTagID){
    out_PIT <- (vTagID_PIT %>%
                    filter(vTagID == as.numeric(in_vTagID)) %>%
                    select(PIT_Tag))[[1]]
    return(out_PIT)
}

PIT_to_vTagID <- function(in_PIT){
    out_vTagID <- (vTagID_PIT %>%
                       filter(PIT_Tag == as.character(in_PIT)) %>%
                       select(vTagID))[[1]]
    return(out_vTagID)
}



## @knitr newest_vTagID_fun
findNewest_vTagID <- function(focal_vTagIDs, refDF = allSites){
    new_vTagID <- (refDF %>%
                       filter(vTagID %in% as.numeric(focal_vTagIDs[!is.na(focal_vTagIDs)])) %>%
                       arrange(desc(Date)))$vTagID[1]
    return(new_vTagID)
}



## @knitr equiv_vTagID_PIT
equiv_vTagID_PIT <- 
    vTagID_PIT %>%
    group_by(PIT_Tag) %>%
    # Combining all equivalent vTagIDs into one character column, separated by ':',
    # then split them up
    summarize(vTagIDs = paste(vTagID, collapse = ':')) %>%
    separate(vTagIDs, paste0('vTagID_', seq(3)), sep = ':', 
             fill = 'right') %>%
    # Remove rows where only one vTagID matches with the PIT_Tag
    filter(!is.na(vTagID_2)) %>%
    # Convert vTagID columns back to numeric for compatibility with `allSites` data frame
    mutate_each(funs(as.numeric), starts_with('vTagID')) %>%
    select(vTagID_1, vTagID_2, vTagID_3, PIT_Tag)





## @knitr equiv_vTagID
equiv_vTagID <- list(input = as.vector(t(equiv_vTagID_PIT[,1:3])),
                     newest = rep(apply(equiv_vTagID_PIT[,1:3], 1, findNewest_vTagID),
                                  each = 3)) %>% 
    data.frame %>%
    filter(!is.na(input)) %>%
    as.tbl







## @knitr expandLookup








## @knitr allSites_known
valid_vTagID <- allSites %>% filter(!is.na(vTagID))

valid_PIT <- allSites %>%
    filter(is.na(vTagID),
           PIT_Tag %in% vTagID_PIT$PIT_Tag) %>%
    rowwise %>%
    mutate(vTagID = PIT_to_vTagID(PIT_Tag)) %>%
    ungroup

allKnownSites <- list(valid_vTagID, valid_PIT) %>%
    bind_rows

rm(valid_vTagID, valid_PIT)

