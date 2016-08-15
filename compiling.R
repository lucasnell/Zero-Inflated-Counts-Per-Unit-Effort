# Raw R code chunks for 'compiling.Rmd'



## @knitr libraries
library("dplyr")
library("tidyr")
library("readxl")
library("stringr")
library("readr")


## @knitr inputs
apal_df <- suppressWarnings(
    read_excel(paste0("~/Google Drive/Gulf sturgeon/Adam/Final NRDA_Blood.xlsx")))
suwa_df <- read_excel(paste0("~/Google Drive/Gulf sturgeon/Randall/Melissa/",
                          "Copy of Suwannee July 1 2016.xlsx"), 
                   sheet = 2)
pearl_df <- read_csv(paste0("~/Google Drive/Gulf sturgeon/Western/",
                         "PR_Master_Sturgeon_only.csv"),
                  locale = locale(date_format = "%m/%d/%Y"))
pasc_df <- read_excel(paste0("~/Google Drive/Gulf sturgeon/Western/",
                          "Copy of MSP_GS_Tagdata_Pascagoula.xlsx"))
choc_df <- suppressWarnings(
    read_excel(paste0("~/Google Drive/Gulf sturgeon/Choctawhatchee/",
                      "Choc_Sturgeon_transmitter_2010_2012.xlsx"), 
               na = 'NA'))
yell_df <- read_excel(paste0('~/Google Drive/Gulf sturgeon/Adam/',
                          'Yellow_Escambia_vTagID_update_June_2016.xlsx'),
                   na = 'NA')

panh_df <- read_excel(paste0('~/Google Drive/Gulf sturgeon/Adam/From Frank July 2016/',
                  'Panhandle_rivers_Frank_July_2016.xlsx'))

nrda_df <- read_excel(paste0('~/Google Drive/Gulf sturgeon/Adam/',
                          'Final NRDA_Blood_updated_July_2016.xlsx'), 
                   col_types = c('text', 'date', rep('text', 6), 'numeric', 
                                 rep('text', 4), 'numeric', 'numeric', 
                                 rep('text', 78-15)))

prma_df <- read_excel(paste0('~/Google Drive/Gulf sturgeon/Western/',
                          'PR_Master_Sturgeon_July_2016.xlsx'))

erdc_df <- read_excel(paste0('~/Google Drive/Gulf sturgeon/Western/',
                          'Pearl_ERDC_Todd_June_23_2016.xlsx'))







## @knitr manApal
apal_df <- apal_df %>%
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
suwa_df <- suwa_df %>%
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
    select(Site, Date, vTagID, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm) %>%
    filter(!is.na(Site))


## @knitr manPearl
pearl_df <- pearl_df %>% 
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
pasc_df <- pasc_df %>%
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
choc_df <- choc_df %>%
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



## @knitr manYell
yell_df <- yell_df %>%
    rename(
        PIT_Tag1 = `PIT  Tag`, 
        PIT_Tag2 = `PIT (old)`,
        FL_mm = `F L (mm)`, 
        TL_mm = `T L (mm)`) %>%
    mutate(
        Date = as.Date(Date),
        Site = gsub('_', ' ', Site) %>% str_to_title
    ) %>%
    select(Site, Date, vTagID, vSerial, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm)



## @knitr manPanh
panh_df <- panh_df %>%
    mutate(
        vTagID = as.integer(vTagID),
        vSerial = as.character(vSerial),
        FL_mm = FL_cm * 10, 
        TL_mm = TL_cm * 10,
        PIT_Tag1 = as.character(PIT_new), 
        PIT_Tag2 = as.character(PIT_old),
        Date = as.Date(Date),
        Site = paste0(gsub('_', ' ', River) %>% str_to_title, ' River'),
        Internal = (Internal_External == 'Internal')
    ) %>%
    select(Site, Date, vTagID, vSerial, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm, Internal)




## @knitr manNrda
nrda_df <- nrda_df %>% 
    select(2:15) %>%
    mutate(
        vTagID = as.integer(V_TagID),
        vSerial = as.character(V_Serial),
        FL_mm = FL_cm * 10, 
        TL_mm = TL_cm * 10,
        PIT_Tag1 = as.character(PIT_New), 
        PIT_Tag2 = as.character(Pit_Old),
        Date = as.Date(Date),
        Site = gsub('_', ' ', Site) %>% str_to_title
    ) %>%
    select(Site, Date, vTagID, vSerial, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm)



## @knitr manPrma
prma_df <- prma_df %>%
    mutate(
        Date = as.Date(paste(Month, Day, Year, sep = '-'), format = '%m-%d-%Y'),
        Site = gsub('_', ' ', `Capture Water Body`) %>% str_to_title,
        FL_mm = `FL-cm` * 10,
        TL_mm = `TL-cm` * 10,
        PIT_Tag1 = ifelse(!is.na(`Converted PIT Tag (hexadecimal format)`), 
                          `Converted PIT Tag (hexadecimal format)`, 
                          `Pit Tag`),
        vTagID = as.integer(Tel_tag_code),
        vSerial = as.character(Tel_tag_SN),
        PIT_Tag2 = as.character(`old_Pit_tag`)
    ) %>%
    select(Site, Date, vTagID, vSerial, PIT_Tag1, PIT_Tag2, TL_mm, FL_mm)




## @knitr manErdc
erdc_df <- erdc_df %>%
    mutate(
        Date = as.Date(`Date`),
        vTagID = as.integer(vTagID),
        vSerial = as.character(vSerial)
    ) %>%
    rename(PIT_Tag1 = PIT_Tag) %>%
    select(Site, Date, vTagID, vSerial, PIT_Tag1, TL_mm, FL_mm)



## @knitr gatherFun
gatherByPIT <- function(df){
    df %>%
        gather(PIT_name, PIT_Tag, starts_with('PIT_Tag', ignore.case = FALSE)) %>%
        filter(PIT_name == 'PIT_Tag1' | !is.na(PIT_Tag)) %>%
        select(-PIT_name)
}



## @knitr runGather
allSites <- lapply(ls(pattern = '_df'), function(x) eval(as.name(x))) %>%
    lapply(., gatherByPIT) %>% 
    bind_rows
allSites

