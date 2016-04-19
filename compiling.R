
library("dplyr")
library("tidyr")
library("readxl")
library("stringr")
# library("reshape2")
# library("broom")

options(stringsAsFactors = FALSE)
source('compiling_preamble.R')

# Input data frames: Apal, Suwa, Pearl, Pasc


# ================
# Apalach
# ================


Apal <- Apal %>%
  select(1:21) %>%
  rename(PIT_Tag1 = PIT_New, PIT_Tag2 = Pit_Old) %>%
  mutate(
    FL_mm = FL_cm*10, 
    TL_mm = TL_cm*10,
    Date = as.Date(Date),
    Site = str_replace(Site, '_', ' ') %>% str_to_title
  ) %>%
  filter(!is.na(V_TagID)) %>%
  select(Site, Date, V_TagID, V_Serial, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm)


# ================
# Suwannee
# ================

Suwa <- Suwa %>%
  rename(
    V_TagID = `ACOUSTIC TAG #`,
    PIT_Tag1 = `TAG 9    PIT TAG (ANTERIOR D FIN BASE)`,
    PIT_Tag2 = `TAG 10 EXTRA  OR AUX PIT TAG (INCL EXTRA PIT TAG 2)`,
    TL_mm = `TL mm`,
    FL_mm = `FL mm`,
    Site = `RIVER SYS`,
    Date = DATE
  ) %>%
  mutate(
    Date = as.Date(Date),
    Site = str_replace(Site, '_', ' ') %>% str_to_title
  ) %>%
  select(Site, Date, V_TagID, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm)



# ================
# Pearl
# ================

Pearl <- Pearl %>% 
  as.tbl %>% 
  mutate(
    Date = as.Date(Date, format = '%m/%d/%Y'),
    Site="Pearl", 
    FL_mm=FL.cm*10, 
    TL_mm=TL.cm*10
  ) %>%
  rename(
    V_TagID = Tel_tag_code, 
    PIT_Tag1 = Pit.Tag,
    PIT_Tag2 = old_Pit_tag
  ) %>%
  select(Site, Date, V_TagID, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm)



# ================
# Pascagoula
# ================


Pasc <- Pasc %>%
  mutate(
    Date = as.Date(Date_Tagged),
    Site = "Pascagoula",
    FL_mm = `FL(cm)`*10,
    TL_mm = `TL(cm)`*10
  ) %>%
  rename(
    V_TagID = Tag_ID,
    PIT_Tag1 = `Pit Tag`
  ) %>%
  select(Site, Date, V_TagID, PIT_Tag1, TL_mm, FL_mm)



# ================================
# ================================

#     Gathering by PIT and combining

# ================================
# ================================



# Input data frames: Apal, Suwa, Pearl, Pasc

# Function to `gather` (i.e., collapse) PIT_Tag columns into one, resulting in more 
# rows but easier data frames to combine.
# Since I'm unsure of what sites might repeat PIT tags, I'm assigning them names that
# include the first 4 letters of the site.
gatherByPIT <- function(df){
  PIT_name_prefix <- str_c(str_sub(df$Site[1], 1, 4), '_')
  df %>%
    gather(PIT_name, PIT_Tag, starts_with('PIT_Tag', ignore.case = FALSE)) %>%
    mutate(
      PIT_name = str_replace(PIT_name, 'PIT_Tag', PIT_name_prefix)
    )
}

# Run above function and combine them all.
allSites <- lapply(list(Apal, Suwa, Pearl, Pasc), gatherByPIT) %>% 
  bind_rows


# I next want to figure out how many unique PIT Tag numbers are associated with 
# `V_TagID`s. Max appears to be 4.

allSites %>% 
  group_by(V_TagID) %>%
  summarize(uniquePITs = length(unique(PIT_Tag))) %>%
  select(uniquePITs) %>% 
  unlist %>% as.numeric %>% 
  table



