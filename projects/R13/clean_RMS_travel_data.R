## Clean data set from RMS export to be "dashboard ready"
# Data Source: CDC Resource Management System (RMS)
# Table: GID Travel Costs, by FY, country, and organizational unit
# Updated 2025-02-18
# Author: Lori Niehaus / GID Data Analytics Task Team (DATT)

# TO DO: Pending staff data from Edgar/GPT to match on staff name to identify associated GID Team and 
# primary strategic priority associated with GID team (and thus, team travel costs)

#######################################################################
#######################################################################
## R SETUP
#######################################################################
rm(list=ls()) # clear

## Install dependencies
required.packages <- c("tidyverse", "AzureStor","Microsoft365R", "readxl", "janitor","scales", "devtools")
packages.to.install <- setdiff(required.packages, installed.packages())
if(length(packages.to.install) >0) install.packages(packages.to.install)

# GID PEB SIR Team functions - use to read data from ADLS
# devtools::install_github("nish-kishore/sirfunctions")
# https://github.com/nish-kishore/sirfunctions

library(tidyverse)
library(readxl)
library(AzureStor)
library(Microsoft365R)
library(janitor)
library(scales)
library(sirfunctions)

# getwd()

datt_task_id <- "R13"

#######################################################################
## READ INPUT DATA
#######################################################################

# Data set to read from ADLS
data_fi <- "data_raw/FY2024_GID_TripCosts_by_Country.csv"

data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                              file_loc = data_fi)

#######################################################################
## READ REFERENCE DATA (from EDAV ADLS)
#######################################################################

## FUNCTION ONLY WORKS WITH CSV RDS AND RDA FILES
# # all ref data
# gid_ref_xls <- sirfunctions::edav_io(io = "list", default_dir = "GID/GIDMEA/giddatt/data_raw") %>% 
#   filter(grepl("GID_M&E_Data_Dictionary",name))
# 
# # most recent ref data
# gid_ref_xls$date <- as.Date(sub(".*?(\\d{4}-\\d{2}-\\d{2}).*", "\\1", gid_ref_xls$name))
# gid_ref_xls <- gid_ref_xls %>% filter(date == max(date))
# fi_name <- strsplit(gid_ref_xls$name, "/")[[1]][5]
# 
# # read ref data
# gid_ref_data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt/data_raw", 
#                               file_loc = fi_name)

gid_ref_xls <- "C:/Users/tvf1/OneDrive - CDC/07_Projects/0. DATT/2025-02-10_GID_M&E_Data_Dictionary.xlsx"

# Merge with geo and vpd reference data
budget_ref <- read_xlsx(gid_ref_xls,
                     sheet = "Budget Key") %>% clean_names() %>% as.data.frame()

orgunit_ref <- read_xlsx(gid_ref_xls,
                        sheet = "Org Unit Key") %>% clean_names() %>% as.data.frame()

country_ref <- read_xlsx(gid_ref_xls,
                         sheet = "Country Key") %>% clean_names() %>% as.data.frame()

country_ref$country_name_lower <- tolower(country_ref$country_name)


#######################################################################
## CLEAN TRAVEL DATA
#######################################################################
head(data,3)
str(data)

## 1. Remove NA subtotal rows and information on filters applied
# Check if Trip ID is 10 characters, and if not, drop row

class(data$`Trip ID`) # Character

cleaned_data <- data %>%
  filter(nchar(`Trip ID`) == 10)

#nrow(cleaned_data) # 2 rows dropped

## 2. change type of return and departure dates to class 'date' (from character); and voucher date

cleaned_data$`Trip Return Date test` <- as.Date(cleaned_data$`Trip Return Date`, format = "%m/%d/%Y")
cleaned_data$`Trip Departure Date` <- as.Date(cleaned_data$`Trip Departure Date`, format = "%m/%d/%Y")
cleaned_data$`Voucher Approved Date` <- as.Date(cleaned_data$`Voucher Approved Date`, format = "%m/%d/%Y")

# Check for NAs after conversion
if (sum(is.na(cleaned_data$`Trip Return Date`)) != 0){
  View(cleaned_data %>% filter(is.na(cleaned_data$`Trip Return Date`))) 
}

if (sum(is.na(cleaned_data$`Trip Departure Date`)) != 0){
  View(cleaned_data %>% filter(is.na(cleaned_data$`Departure Return Date`))) 
}


## 3. change type of cost cols to class 'numeric' (from character)

cleaned_data$`Reimbursable Amount` <- gsub("[$,]","",cleaned_data$`Reimbursable Amount`) %>% as.numeric()
cleaned_data$`Advanced Applied Amount`<- gsub("[$,]","",cleaned_data$`Advanced Applied Amount`) %>% as.numeric()
cleaned_data$`Trip Cost`<- gsub("[$,]","",cleaned_data$`Trip Cost`) %>% as.numeric()

## 4. add fiscal year ** based on departure date and return date (but what happens if over FY?)
cleaned_data <- cleaned_data %>%
  mutate(fy = ifelse(
    ((month(`Trip Departure Date`))>=10), # if fy starts in Oct
    ((year(`Trip Departure Date`)) + 1), # add 1 to year
    ((year(`Trip Departure Date`))))) # else, take year as is from date value

# cleaned_data$fy %>% unique() # all 2024 in current dataset

## 4. match reference data on country

#View(cleaned_data %>% filter(is.na(Country))) # no missing
#unique(cleaned_data$Country)
cleaned_data$country_name_lower <- tolower(cleaned_data$Country)
sort(setdiff(unique(cleaned_data$country_name_lower),unique(country_ref$country_name_lower)))
sort(country_ref$country_name_lower)

# update vals to match ref data
cleaned_data$country_name_lower[cleaned_data$country_name_lower == "congo, democratic republic of the"] <- "democratic republic of the congo"
cleaned_data$country_name_lower[cleaned_data$country_name_lower == "turkey"] <- "türkiye"
cleaned_data$country_name_lower[cleaned_data$country_name_lower == "korea, republic of"] <- "democratic people's republic of korea"
cleaned_data$country_name_lower[cleaned_data$country_name_lower == "netherlands"] <- "netherlands (kingdom of the)"
cleaned_data$country_name_lower[cleaned_data$country_name_lower == "tanzania, united republic of"] <- "united republic of tanzania"

# check
#sort(setdiff(unique(cleaned_data$country_name_lower),unique(country_ref$country_name_lower))) # 0

# merge reference data
wide_data <- cleaned_data %>% 
  full_join(country_ref,
            by = c("country_name_lower"))

## 4. match reference data on organizational units

#unique(orgunit_ref$branch_org_code)
#unique(cleaned_data$Organization) # meaning unclear
#unique(cleaned_data$`CAN Admin Code`) # matches

#nrow(cleaned_data %>% filter(is.na(`CAN Admin Code`))) #0 - No NA

cleaned_data$branch_org_code <- word(cleaned_data$`CAN Admin Code`,1)

sort(setdiff(unique(cleaned_data$branch_org_code),unique(orgunit_ref$branch_org_code)))
sort(setdiff(unique(orgunit_ref$branch_org_code),unique(cleaned_data$branch_org_code))) # all included

## What does it mean for org code to be just 'GID'? "CWC"
## issue with field > need further input/manual correction

View(cleaned_data %>% filter(cleaned_data$branch_org_code=="CWC"))

unique(cleaned_data$CAN)
unique(budget_ref$can) 
sort(setdiff(unique(cleaned_data$CAN),unique(budget_ref$can)))
sort(setdiff(unique(budget_ref$can),unique(cleaned_data$CAN)))

# Need CAN info
# "93906FY" "9390G93" "9390L1H" "939ZXBV" "939ZXBW"

cleaned_data$can <- cleaned_data$CAN

wide_data2 <- cleaned_data %>% 
  full_join(budget_ref,
            by = c("can"))

View(wide_data2 %>% filter(wide_data2$branch_org_code=="CWC"))
