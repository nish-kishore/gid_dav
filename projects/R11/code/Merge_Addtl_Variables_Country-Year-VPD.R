## Update long country-year-vpd dataset in ADLS to include additional variables
# Updated 2025-02-21
# Author: Lori Niehaus

#######################################################################
## VARIABLES TO ADDD
## coverage; Source: WUENIC (https://immunizationdata.who.int/)
## imm system indicator - functional NITAG
## US vaccine case data
## cost per case data
#######################################################################

#######################################################################
## R SETUP
#######################################################################
rm(list=ls()) # clear

## Install dependencies
required.packages <- c("tidyverse", "AzureStor","Microsoft365R", "readxl", "janitor","scales", "devtools", "rvest")
packages.to.install <- setdiff(required.packages, installed.packages())
if(length(packages.to.install) >0) install.packages(packages.to.install)

# GID PEB SIR Team functions - use to read data from ADLS
#devtools::install_github("nish-kishore/sirfunctions")
# https://github.com/nish-kishore/sirfunctions

# load libraries
lapply(c("tidyverse", "readxl", "AzureStor", "Microsoft365R", "janitor", "scales", "sirfunctions"), library, character.only = TRUE)

datt_task_id <- "R11"

############################################################################
## IMPORT CLEAN DATA FROM ADLS
############################################################################
# sirfunctions::edav_io(io = "list", default_dir = "GID/GIDMEA/giddatt/data_clean")  # view list of clean data files in ADLS

data_fi <- "data_clean/country_vpd_year_casedata.rds"
data_clean <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", #country-vpd-year-variable long dataframe
                                    file_loc = data_fi)
data <- data_clean
## Review dataset structure
#str(data)
#unique(data$vpd)
#unique(data$variable)


#######################################################################
## UPLOAD & RESAVE REFERENCE DATA (from EDAV ADLS)
#######################################################################

# gid_ref_xls <- "C:/Users/tvf1/OneDrive - CDC/07_Projects/0. DATT/2025-02-21_GID_M&E_Data_Dictionary.xlsx"
# 
# # Merge with geo and vpd reference data
# vpd_ref <- read_xlsx(gid_ref_xls,
#                      sheet = "VPD Key") %>% clean_names() %>% as.data.frame()
# country_ref <- read_xlsx(gid_ref_xls,
#                          sheet = "Country Key") %>% clean_names() %>% as.data.frame()



# write reference data to processing folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/country_ref.rds",
                      obj = country_ref)


country_ref <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                              file_loc = "data_processing/country_ref.rds")

country_ref$country_name_lower <- tolower(country_ref$country_name)

vpd_ref <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                     file_loc = "data_processing/vpd_ref.rds")


#######################################################################
## EBOLA CASE DATA
#######################################################################
#Source: "https://www.cdc.gov/ebola/outbreaks/index.html#cdc_listing_res-cases-and-outbreaks-of-ebola-disease-by-year"
# Data table from web scraping provied by Mary Choi NCEZID/VSBP

#######################################################################
## US MEASLES CASE DATA
#######################################################################
## Source: US CDC, Downloaded from website as of 2/28/2025; Updated on February 21, 2025.
## note: 2023–2025 case counts are preliminary and subject to change.


us_measles_cases <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                              file_loc = "data_raw/2025-02-28_CDC-US-MeaslesCases_1985-2025.csv") %>% select(-filter) %>% 
  mutate(iso3_code="USA", vpd="measles")


# Write data to processing folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                       file_loc = "data_processing/USA_measles_casedata.rds", obj = us_measles_cases)

# Prepare to merge with country-year-vpd-variable-value dataset
us_measles_merge <- us_measles_cases %>% 
  mutate(variable="cases") %>% 
  select(iso3_code, year, vpd, variable, cases) %>%
  rename(value=cases)

#######################################################################
## VACCINATION COVERAGE
#######################################################################

#Source 1:  CDC NIS-child (ChildVaxView) - >= 1 dose MMR coverage by age 24 mo by birth year, NIS-child

# Source 2: WUENIC 2023 revision (UNICEF Data warehouse) MCV, 1st dose = official and WUENIC (2000 - 2023)
# https://data.unicef.org/resources/data_explorer/unicef_f/?ag=UNICEF&df=GLOBAL_DATAFLOW&ver=1.0&dq=.IMMUNISATION+IM_BCG+IM_DTP1+IM_DTP3+IM_HEPB3+IM_HEPBB+IM_HIB3+IM_HPV+IM_IPV1+IM_MCV1+IM_MCV2+IM_PAB+IM_PCV3+IM_POL3+IM_RCV1+IM_ROTAC+IM_YFV+IM_IPV2+IM_MENGA.&startPeriod=1970&endPeriod=2024
# 1980 - 2023

# Upload only immunization indicators (prefix (IM_)) (full dataset is 637.46 MiB; ~200 K rows)
wuenic <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                          file_loc = "data_raw/2025-02-28_UNICEF_DATAWAREHOUSE_ALLINDICATORS.csv") %>% 
  filter(startsWith(`INDICATOR:Indicator`, "IM_")) %>% select(-DATAFLOW) %>% select(where(~ !all(is.na(.)))) # also remove cols from filtered data that are all NA

## Clean dataframe
colnames(wuenic) <- gsub(":.*", "", colnames(wuenic)) # remove text after colon to make cols easier to reference
wuenic <- wuenic %>% rename(geography = REF_AREA, year = TIME_PERIOD, value = OBS_VALUE) # rename cols to match existing GID data
wuenic$value <- wuenic$value %>% as.numeric() # coverage value to numeric

#unique(wuenic$OBS_FOOTNOTE) # generic footnotes included for MCV2 and HPV values
#unique(wuenic$DATA_SOURCE) # WUENIC and HPV estimates

wuenic <- wuenic %>% mutate(vaccine = gsub("IM_","",gsub(":.*", "", INDICATOR)),
                            variable = "imm_coverage")

# match vaccine to vpd using vpd reference data
unique(vpd_ref$vpd_vaccines)
unique(wuenic$vaccine)

