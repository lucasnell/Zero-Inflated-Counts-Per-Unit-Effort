
## @knitr libraries
library("dplyr")
library("tidyr")
library("readxl")
library("stringr")
library("readr")


## @knitr inputs
Apal <- suppressWarnings(
    read_excel(
        "~/Google Drive/Gulf sturgeon/Adam/Final NRDA_Blood.xlsx"))
Suwa <- read_excel(
    "~/Google Drive/Gulf sturgeon/Randall/Melissa/Copy of Suwannee.xlsx", 
    2)
Pearl <- read_csv(
    "~/Google Drive/Gulf sturgeon/Western/PR_Master_Sturgeon_only.csv",
    locale = locale(date_format = "%m/%d/%Y"))
Pasc <- read_excel(
    "~/Google Drive/Gulf sturgeon/Western/Copy of MSP_GS_Tagdata_Pascagoula.xlsx")
Choc <- read_csv(
    "~/Google Drive/Gulf sturgeon/Choctawhatchee/Choc_Sturgeon_trans_2010_12.csv.gz", 
    locale = locale(date_format = "%m/%d/%y"))



## @knitr manApal
Apal <- Apal %>%
    select(1:21) %>%
    rename(
        PIT_Tag1 = PIT_New, 
        PIT_Tag2 = Pit_Old,
        vTagID = V_TagID,
        vSerial = V_Serial) %>%
    mutate(
        FL_mm = FL_cm*10, 
        TL_mm = TL_cm*10,
        Date = as.Date(Date),
        Site = gsub('_', ' ', Site) %>% str_to_title
    ) %>%
    select(Site, Date, vTagID, vSerial, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm)


## @knitr manSuwa
Suwa <- Suwa %>%
    rename(
        vTagID = `ACOUSTIC TAG #`,
        PIT_Tag1 = `TAG 9    PIT TAG (ANTERIOR D FIN BASE)`,
        PIT_Tag2 = `TAG 10 EXTRA  OR AUX PIT TAG (INCL EXTRA PIT TAG 2)`,
        TL_mm = `TL mm`,
        FL_mm = `FL mm`,
        Site = `RIVER SYS`,
        Date = DATE
    ) %>%
    mutate(
        Date = as.Date(Date),
        Site = gsub('_', ' ', Site) %>% str_to_title
    ) %>%
    select(Site, Date, vTagID, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm)


## @knitr manPearl
Pearl <- Pearl %>% 
    mutate(
        Date = as.Date(Date),
        Site = "Pearl", 
        FL_mm = `FL-cm`*10, 
        TL_mm = `TL-cm`*10
    ) %>%
    rename(
        vTagID = Tel_tag_code, 
        PIT_Tag1 = `Pit Tag`,
        PIT_Tag2 = old_Pit_tag
    ) %>%
    select(Site, Date, vTagID, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm)


## @knitr manPasc
Pasc <- Pasc %>%
    mutate(
        Date = as.Date(Date_Tagged),
        Site = "Pascagoula",
        FL_mm = `FL(cm)`*10,
        TL_mm = `TL(cm)`*10
    ) %>%
    rename(
        vTagID = Tag_ID,
        PIT_Tag1 = `Pit Tag`
    ) %>%
    select(Site, Date, vTagID, PIT_Tag1, TL_mm, FL_mm)



## @knitr manChoc
Choc <- Choc %>%
    mutate(
        FL_mm = `Fork Length (cm)`*10,
        TL_mm = `Total Length (cm)`*10,
        Date = as.Date(`Date Tagged/landed`),
        Site = "Choctawhatchee River"
    ) %>%
    rename(
        vTagID = `VEMCO Tag #`,
        PIT_Tag1 = `New PIT Tag #`,
        PIT_Tag2 = `Existing Pit Tag #`
    ) %>%
    select(Site, Date, vTagID, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm)




## @knitr gatherFun
gatherByPIT <- function(df){
    df %>%
        gather(PIT_name, PIT_Tag, starts_with('PIT_Tag', ignore.case = FALSE)) %>%
        filter(PIT_name == 'PIT_Tag1' | !is.na(PIT_Tag)) %>%
        select(-PIT_name)
}



## @knitr runGather
allSites <- lapply(list(Apal, Suwa, Pearl, Pasc, Choc), gatherByPIT) %>% 
    bind_rows
allSites

