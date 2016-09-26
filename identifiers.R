# Raw R code chunks for 'identifiers.Rmd'



## @knitr libraries
library("dplyr")
library("tidyr")
library("stringr")
library("readr")


## @knitr sourceCompiling
source("compiling.R")
rm(list = ls(pattern = '_df|gather'))




## @knitr unique_PITs_per_vTag
unique_PITs_Tab <- cleanSites %>% 
    filter(!is.na(vTagID), !is.na(PIT_Tag)) %>%
    group_by(vTagID) %>%
    summarize(uniquePITs = length(unique(PIT_Tag))) %>%
    select(uniquePITs) %>% 
    table
unique_PITs_Tab


## @knitr unique_vTags_per_PIT
unique_vTags_Tab <- cleanSites %>%
    filter(!is.na(vTagID), !is.na(PIT_Tag)) %>%
    group_by(PIT_Tag) %>%
    summarize(uniquevTagIDs = length(unique(vTagID))) %>%
    select(uniquevTagIDs) %>% 
    table
unique_vTags_Tab



## @knitr maxUniques

maxUniques <- c(as.numeric(attr(unique_PITs_Tab, 'dimnames')$.), 
                as.numeric(attr(unique_vTags_Tab, 'dimnames')$.)) %>% max



## @knitr vTag_PIT_lookup_DF
vTagID_PIT <- cleanSites %>%
    filter(!is.na(vTagID), !is.na(PIT_Tag)) %>%
    distinct(vTagID, PIT_Tag) %>%
    select(vTagID, PIT_Tag)



## @knitr vTag_PIT_lookup_Funs
vTagID_to_PITs <- function(in_vTagIDs){
    one_vTagID <- function(in_vTagID){
        as.character(filter(vTagID_PIT, vTagID == as.integer(in_vTagID))$PIT_Tag)
    }
    return(sapply(in_vTagIDs, one_vTagID, USE.NAMES = FALSE))
}

PIT_to_vTagID <- function(in_PITs){
    one_PIT <- function(in_PIT){
        as.integer(filter(vTagID_PIT, PIT_Tag == as.character(in_PIT))$vTagID)
    }
    return(sapply(in_PITs, one_PIT, USE.NAMES = FALSE))
}



## @knitr newest_vTagID_fun
findNewest_vTagID <- function(focal_vTagIDs, refDF = cleanSites){
    new_vTagID <- (refDF %>%
                       filter(vTagID %in% 
                                  as.integer(focal_vTagIDs[!is.na(focal_vTagIDs)])) %>%
                       arrange(desc(Date)))$vTagID[1]
    return(new_vTagID)
}



## @knitr equiv_vTagID_PIT
equiv_vTagID_PIT <- 
    vTagID_PIT %>%
    group_by(PIT_Tag) %>%
    # Combining all equivalent vTagIDs into one character column, separated by ':'
    summarize(vTagIDs = paste(vTagID, collapse = ':')) %>%
    # ... then split them back up
    separate(vTagIDs, paste0('vTagID_', seq(maxUniques)), sep = ':', 
             fill = 'right') %>%
    # Remove rows where only one vTagID matches with the PIT_Tag
    filter(!is.na(vTagID_2)) %>%
    # Convert vTagID columns back to integer for compatibility with `cleanSites` data frame
    mutate_each(funs(as.integer), starts_with('vTagID')) %>%
    # Using 'standard evaluation' by using `select_`, which allows use of `maxUniques`
    select_(.dots = c(paste0('vTagID_', seq(maxUniques)), 'PIT_Tag'))


## @knitr equiv_vTagID
equiv_vTagID <- data_frame(input = as.vector(t(equiv_vTagID_PIT[,1:maxUniques])),
                           newest = rep(apply(equiv_vTagID_PIT[,1:maxUniques], 1, 
                                              findNewest_vTagID),
                                        each = maxUniques)) %>% 
    filter(!is.na(input))




## @knitr expand_vTagID_PIT_NewRows

vTagID_PIT_NewRows <- equiv_vTagID %>%
    # Group by individual
    group_by(newest) %>%
    # Create character strings of all PIT_Tags and vTagIDs for each individual, 
    #   separated by colons
    summarize(PIT_Tags = paste0(vTagID_PIT$PIT_Tag[vTagID_PIT$vTagID %in% input],
                               collapse = ':'),
              vTagIDs = paste0(input, collapse = ':')) %>%
    # Separate PIT_Tags into separate columns (used 20 bc it's definitely more than what
    #   we'll observe)
    separate(PIT_Tags, paste0('PIT_Tags_', seq(20)), sep = ':', 
             fill = 'right') %>%
    # Condense into a single column by adding rows, or "lengthening" data frame
    gather(PIT_name, PIT_Tag, -newest, -vTagIDs, na.rm = TRUE) %>%
    # These columns no longer needed
    select(-PIT_name, -newest) %>%
    # Do the same as above for the vTagIDs
    separate(vTagIDs, paste0('vTagIDs_', seq(10)), sep = ':', 
             fill = 'right') %>%
    gather(vTagID_name, vTagID, -PIT_Tag, na.rm = TRUE) %>%
    # Make vTagID integer for compatibility
    mutate(vTagID = as.integer(vTagID)) %>%
    select(-vTagID_name) %>%
    # Filter for unique combinations
    distinct(vTagID, PIT_Tag)




## @knitr expand_vTagID_PIT

vTagID_PIT <- bind_rows(vTagID_PIT, vTagID_PIT_NewRows) %>%
    distinct(vTagID, PIT_Tag) %>%
    arrange(vTagID)


## @knitr max_vTagIDs

max_vTagIDs <- vTagID_PIT %>% 
    group_by(vTagID) %>% 
    summarize(len = n()) %>% 
    select(len) %>% 
    max


## @knitr PITs_w_vTag
PITs_w_vTag <- vTagID_PIT %>%
    group_by(PIT_Tag) %>% 
    summarize(vTagID_new = findNewest_vTagID(vTagID)) %>%
    rename(vTagID = vTagID_new) %>%
    mutate(
        System = sapply(PIT_Tag, function(x){
            tail(cleanSites$System[cleanSites$PIT_Tag == x] %>% na.omit, 1)}, 
            USE.NAMES = FALSE),
        Subsystem = sapply(PIT_Tag, function(x){
            tail(cleanSites$Subsystem[cleanSites$PIT_Tag == x] %>% na.omit, 1)}, 
            USE.NAMES = FALSE)
    ) %>%
    arrange(System, Subsystem, PIT_Tag) %>%
    select(System, Subsystem, PIT_Tag, vTagID)





## @knitr dropped_vTagIDs
dropped_vTagIDs <- equiv_vTagID %>% 
    filter(! input %in% equiv_vTagID$newest) %>%
    rename(dropped = input, new = newest)





## @knitr newestID_master
cleanSites_newIDs <- cleanSites %>%
    mutate(
        vTagID = sapply(vTagID, 
                        function(x){
                            ifelse(x %in% dropped_vTagIDs$dropped, 
                                   dropped_vTagIDs$new[dropped_vTagIDs$dropped == x], 
                                   x)
                        })
    )

## @knitr knownID_master
masterCaps <- list(
    # valid vTagIDs:
    cleanSites_newIDs %>% filter(!is.na(vTagID)),
    # NA vTagIDs, but valid PITs:
    cleanSites_newIDs %>%
        filter(is.na(vTagID),
               PIT_Tag %in% vTagID_PIT$PIT_Tag) %>%
        mutate(vTagID = PIT_to_vTagID(PIT_Tag))
    ) %>% 
    bind_rows

