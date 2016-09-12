library(leaflet)
library(sp)
library(rgdal)
library(data.table)
library(readr)
library(RColorBrewer)
library(DT)
# library(ggplot2) # Not sure we are using this...
library(magrittr)
library(tidyr)
library(dplyr)
library(shiny)

# Make sure to restart session before launching app (CTRL + Shift + F10)
addResourcePath('www', './www/')

# Import geographic data
Region_Shape_16 <- readRDS("Data Sets/Region_Shape_16.rds")
TA_Shape_16 <- readRDS("Data Sets/TA_Shape_16.rds")
Area_Unit_Shape_16 <- readRDS("Data Sets/Area_Unit_Shape_16.rds")

# Import and clean the regional data
Region_Table <- read.csv("Data Sets/REGION_FINAL.csv", stringsAsFactors = F)
Region_Table %<>% rename(sex = x_gender_desc, agegrp = age_desc, description = reg) %>%
  mutate(description = replace(description, description == "Hawkes Bay Region","Hawke's Bay Region"),
         reg = description)

# Import and clean the Extended Territorial data
TA_Table <- read.csv("Data Sets/TA_FINAL.csv", stringsAsFactors = F)
TA_Table %<>% rename(sex = x_gender_desc, agegrp = age_desc, description = tla) %>%
  mutate(description = replace(description, description == "Wanganui District","Whanganui District"),
         description = replace(description, description == "Central Hawkes Bay District","Central Hawke's Bay District"),
         description = replace(description, description == "Devonport-Takapuna Local Board Are","Devonport-Takapuna Local Board Area"),
         description = replace(description, description == "Maungakiekie-Tamaki Local Board Ar","Maungakiekie-Tamaki Local Board Area"),
         tla = description,
         reg = replace(reg, reg == "Hawkes Bay Region","Hawke's Bay Region"))

# Import and really clean up the Area Unit data
Area_Unit_Table <- read.csv("Data Sets/AU_FINAL.csv", stringsAsFactors = F)
Area_Unit_Table %<>% 
  rename(sex = x_gender_desc, agegrp = age_desc, description = au) %>% 
  mutate(au = description) %>%
  mutate(reg = replace(reg, reg == "Hawkes Bay Region","Hawke's Bay Region"),
         tla = replace(tla, tla == "Wanganui District","Whanganui District"),
         tla = replace(tla, tla == "Central Hawkes Bay District","Central Hawke's Bay District"),
         tla = replace(tla, tla == "Devonport-Takapuna Local Board Are","Devonport-Takapuna Local Board Area"),
         tla = replace(tla, tla == "Maungakiekie-Tamaki Local Board Ar","Maungakiekie-Tamaki Local Board Area")) %>%
  filter(description != '')
Area_Unit_Table %<>% bind_rows(Area_Unit_Table %>% 
                                 group_by(reg, tla, description, agegrp) %>% 
                                 summarise_each(funs(sum),matches("all|_")) %>%
                                 mutate(sex = "All")) %>%
  arrange(reg, tla, description, agegrp, sex) 


#some lists for populating drop-down menus.
region.list <- setNames(Region_Shape_16@data$description, Region_Shape_16@data$description)
ta.list <- setNames(TA_Shape_16@data$description, TA_Shape_16@data$description)
Risk0 <- list("2+ Risk Indicators"="all_risk_2", 
              "3+ Risk Indicators"="all_risk_3", 
              "All 4 Risk Indicators"="all_risk_4")
Risk15 <- list("Teenagers with health, disability issues or special needs"="all_risk_1",
               "Mental health service users with stand-down or CYF history"="all_risk_2",
               "Teenage girls supported by benefits"="all_risk_3",
               "Teenage boys with Youth Justice or Corrections history"="all_risk_4",
               "Experienced significant childhood disadvantage"="all_risk_5",
               "In any of the above groups"="all_risk_6")
Risk20 <- list("Young offenders with custodial sentence"="all_risk_1",
               "Young offenders with community sentence and CYF history"="all_risk_2",
               "Jobseekers in poor health with CYF history"="all_risk_3",
               "Sole parents not in full-time employment with CYF history"="all_risk_4",
               "Long-term disability beneficiaries"="all_risk_5",
               "In any of the above groups"="all_risk_6")
sex.list <- list("Male"="Male", "Female"="Female", "Total"="All")
age.list <- list("0-5 years"="00-05", "6-14 years"="06-14", 
                 "15-19 years"="15-19", "20-24 years"="20-24")

# Import the static tables for display.
Table_0005 <- read.csv("Data Sets/Summary Data/Table_0005.csv", stringsAsFactors = F) 
Table_0614 <- read.csv("Data Sets/Summary Data/Table_0614.csv", stringsAsFactors = F) 
Table_1519 <- read.csv("Data Sets/Summary Data/Table_1519.csv", stringsAsFactors = F) 
Table_2024 <- read.csv("Data Sets/Summary Data/Table_2024.csv", stringsAsFactors = F) 

MaroonPalette <- c(rgb(1,1,1),rgb(114,30,53,maxColorValue=255)) #or "Blues" or whatever...
#MaroonPalette <- "Blues"

# Helper functions for HTML tables...
cell_html <- function(table_cell) paste0('<td>', table_cell, '</td>')

head_html <- function(table_cell) paste0('<th>', table_cell, '</th>')

row_html <- function(table_row) {
  cells <- sapply(table_row, cell_html)
  collapse_cells <- paste0(cells, collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}

source('UI_Def.R')
source('Server_Def.R')

runApp(list(
  ui = UI_Def,
  server = Server_Def
),
host='0.0.0.0',
port=80,
launch.browser = FALSE
)
