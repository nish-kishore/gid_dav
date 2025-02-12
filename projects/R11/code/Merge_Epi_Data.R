## Combine data files imported from WHO
# Updated 2025-02-07
# Author: Lori Niehaus

#######################################################################
## PURPOSE:
## Merge WHO case and IA2030 LoD outbreak data into one table
## Visualize VPD outbreaks/cases for GID 8 Critical Countries
## Note: imported data is from downloads (excel files) from data_raw folder in ADLS; need to change based on user


# TO DO: For polio, write LoD count to combine WPV + cVDPV value for the same country/year

#######################################################################
## R SETUP
#######################################################################
rm(list=ls()) # clear

## Install dependencies
required.packages <- c("tidyverse", "AzureStor","Microsoft365R", "readxl", "janitor","scales", "devtools")
packages.to.install <- setdiff(required.packages, installed.packages())
if(length(packages.to.install) >0) install.packages(packages.to.install)

# GID PEB SIR Team functions - use to read data from ADLS
devtools::install_github("nish-kishore/sirfunctions")
# https://github.com/nish-kishore/sirfunctions

library(tidyverse)
library(readxl)
library(AzureStor)
library(Microsoft365R)
library(janitor)
library(scales)
library(sirfunctions)

# getwd()

datt_task_id <- "R11"

#######################################################################
#### CONNECTIONS SET-UP

### Access to Task Team Teams Channel ("DATT")
#######################################################################
dstt <- get_team("GHC_GID_Data_&_Strategy_Tiger_Team")
dstt_channels <- dstt$list_channels()
datt <- dstt$get_channel("Data Analytics Task Team")

# get sharepoint site and default document library associated with team
dstt_site <- dstt$get_sharepoint_site()

datt_docs <- datt$get_folder()
doc_path <- datt_docs$get_path() # "/Data%20Analytics%20Task%20Team"
items <- datt_docs$list_items()

teams_data_folder <- datt_docs$get_item("2. Datasets_clean")
teams_data_files <- teams_data_folder$list_files()

teams_fig_folder <- datt_docs$get_item("3. Figures")
teams_fig_files <- teams_fig_folder$list_files()

## Code for saving datasets or figures to DATT Team Channel

# teams_data_folder$save_dataframe(df, "dataset_name.csv")
# teams_fig_folder$upload(fig_name.png)

## send message to DATT Team Channel

#datt$send_message("ignore - testing connection from R to teams channel")

#######################################################################
#### CONNECTIONS SET-UP
### Access to Task Team EDAV ADLS ("GIDMEA")
#######################################################################
# Use SIR function to access EDAV "/ddphsis-cgh/GID/GIDMEA/giddatt" blob container

# list files in raw_data folder
sirfunctions::edav_io(io = "list", default_dir = "GID/GIDMEA/giddatt/data_raw")

# read raw data
#data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
#                              file_loc = "data_clean/traveler_outbound.rds")

#######################################################################
## LOAD REFERENCE DATA (from EDAV ADLS)
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
vpd_ref <- read_xlsx(gid_ref_xls,
                     sheet = "VPD Key") %>% clean_names() %>% as.data.frame()
country_ref <- read_xlsx(gid_ref_xls,
                         sheet = "Country Key") %>% clean_names() %>% as.data.frame()

country_ref$country_name_lower <- tolower(country_ref$country_name)

#######################################################################
## UPLOAD & CLEAN PUBLIC WHO DATA SETS OF VPD CASES 1980-2003
#######################################################################

## Temporary - read from downloads folder, convert excels

folder_path <- "C:/Users/tvf1/OneDrive - CDC/07_Projects/0. DATT/data_raw/"
excel_files <- list.files(folder_path, pattern = ".xlsx")

# Initialize df
vpd_cases <- data.frame()

for (file in excel_files) {
  # Read the first sheet named "Sheet1" for all WHO VPD case data sets (publicly available, 2023 official)
  df <- read_excel(paste0(folder_path, file), sheet = "Sheet1")
  
  # drop 'Exported' info row
  df <- df %>% filter(!str_detect(df$`Country / Region`, fixed("Exported")))
   
   df_long <- df %>%
      pivot_longer(
        cols = matches("^19|^20"), # Selects columns starting with '19' or '20' (years)
        names_to = "year",
        values_to = "num_cases"
      )
   
   # Bind to other case count dfs
   vpd_cases <- rbind(vpd_cases,df_long)
   
}

# Change col names and data types
#head(vpd_cases)
names(vpd_cases) <- c("country_name","vpd","year","num_cases_WHOofficial")

vpd_cases$year <- as.numeric(vpd_cases$year)

# remove commas from string values before converting to numeric
vpd_cases$num_cases_WHOofficial <- gsub(",","",vpd_cases$num_cases_WHOofficial)
vpd_cases$num_cases_WHOofficial_numeric <- as.numeric(vpd_cases$num_cases_WHOofficial)

if (any(is.na(vpd_cases$num_cases_WHOofficial_numeric))){
  coerced_vals <- vpd_cases$num_cases_WHOofficial[is.na(vpd_cases$num_cases_WHOofficial_numeric)]
}
unique(coerced_vals) # NAs (OK)

# drop string col
vpd_cases <- vpd_cases %>% select(-"num_cases_WHOofficial") %>%
  rename(num_cases_WHOofficial = num_cases_WHOofficial_numeric)

## Clean country names (using ref data)

# Check geo values against ref data
vpd_cases$country_name_lower <- tolower(vpd_cases$country_name)
sort(setdiff(unique(vpd_cases$country_name_lower),unique(country_ref$country_name_lower)))
# 0 - perfect match (makes sense because both WHO ref data from WIISEmart)

############################################################################
## UPLOAD & CLEAN PUBLIC IA2030 LARGE OR DISRUPTIVE OUTBREAK DATA (2018-2023)
############################################################################

LoDpath <-"C:/Users/tvf1/OneDrive - CDC/07_Projects/0. DATT/IA2030_IG1.3_VPD LoD Outbreaks by country_2018-2023.xlsx"
excel_sheets(LoDpath)
outbreaks <- read_excel(paste0(LoDpath), sheet = "Historic Data (2018-2022)")

## PART 1. EXTRACT HISTORIC DATA (2018-2022) FR0M SHEET

## Clean
names(outbreaks)
# cols 2-11 measles 2018-2022 (10 columns); col 12 is notes, can drop; col 13-22 WPV; col 23 drop; 24 starts cVDPV;
# 35 cholera; 46 mening; 57 YF; 68 Ebola (78 notes, final drop)
# pattern: 10 columns per VPD (country, val), then 1 col notes across years can be dropped; 6 VPDs

df_vals <- outbreaks[-c(1:6),]

## For each VPD
vpds <- c("Measles","WPV","cVDPV","Cholera","Meningococcus", "YellowFever", "Ebola")

# initialize data frame
df_complete <- data.frame()

for (vpd in vpds) {
  
  # Separate out each VPD data set
  if (vpd=="Measles") {df <- df_vals %>% select(2:11) }
  if (vpd=="WPV") {df <- df_vals %>% select(13:22) }
  if (vpd=="cVDPV") {df <- df_vals %>% select(24:33) }
  if (vpd=="Cholera") {df <- df_vals %>% select(35:44) }
  if (vpd=="Meningococcus") {df <- df_vals %>% select(46:55) }
  if (vpd=="YellowFever") {df <- df_vals %>% select(57:66) }
  if (vpd=="Ebola") {df <- df_vals %>% select(68:77) }
  
  # Break the dataframe into three parts
  df1 <- df[, 1:2]  # First two columns
  df2 <- df[, 3:4]  # Next two columns
  df3 <- df[, 5:6]  # Last two columns
  df4 <- df[, 7:8]  # Last two columns
  df5 <- df[, 9:10] # Last two columns
  
  # Add a year column to each new dataframe
  df1$year <- 2018
  df2$year <- 2019
  df3$year <- 2020
  df4$year <- 2021
  df5$year <- 2022
  
  # Change col names of each new data frame
  names(df1) <- c("country_name","lod_count","year")
  names(df2) <- c("country_name","lod_count","year")
  names(df3) <- c("country_name","lod_count","year")
  names(df4) <- c("country_name","lod_count","year")
  names(df5) <- c("country_name","lod_count","year")
  
  df_clean <- rbind(df1, df2, df3, df4, df5)
  df_clean$vpd <- vpd
  names(df_clean) <- c("country_name","lod_count","year","vpd")
  
  df_complete <- rbind(df_complete, df_clean)
}

# unique(df_complete$country_name)

## PART 2. EXTRACT MOST RECENT REPORTING DATA (2023) FR0M ACROSS VPD SHEETS

# one sheet per vpd - first 4 rows give info; extra NA rows
# excel_sheets(LoDpath)

## Clean country names (using ref data)
#vpds %in% excel_sheets(LoDpath) # 2 FALSE - WPV, Yellow Fever
vpds_2023 <- c("Measles","Wild Polio Virus","cVDPV","Cholera","Meningococcus", "Yellow Fever", "Ebola")
vpds_2023 %in% excel_sheets(LoDpath) # WPV, Yellow Fever # ALL TRUE

# initialize data frame
df_2023 <- data.frame()

for (vpd in vpds_2023) {
  
  outbreaks_2023 <- read_excel(paste0(LoDpath), sheet = vpd) %>% as.data.frame() # read sheet
  
  outbreaks_2023 <- outbreaks_2023[-c(1:4),] %>% select(-1) # remove first 4 rows with misc info and first column with #s
  if (ncol(outbreaks_2023)==2){outbreaks_2023$incidence <- NA} # add a column if incidence not included
  
  names(outbreaks_2023) <- c("country_name","lod_count","incidence")
  outbreaks_2023$year <- 2023
  outbreaks_2023$vpd <- vpd
  
  df_2023 <- rbind(df_2023, outbreaks_2023) # bind each VPD data set together
}

## PART 3. mERGE HISTORIC AND MOST RECENT DATA

# use same VPD labels and keep 2023 measles incidence vals for future ref
df_complete$incidence <- NA
df_2023 <- df_2023 %>% select(names(df_complete))

df_2023 <- df_2023 %>% mutate(vpd = case_when(
  vpd == "Wild Polio Virus" ~ "WPV",
  vpd == "Yellow Fever" ~ "YellowFever",
  T ~ vpd
))

# bind rows for 2023 to complete dataset
names(df_2023) == names(df_complete) # ALL TRUE
df_complete <- rbind(df_complete, df_2023)

## Clean Complete data set
# unique(df_complete$vpd)
# unique(df_complete$year)
# class(df_complete$year)
# class(df_complete$lod_count)

# Check geo values against ref data
df_complete$country_name_lower <- tolower(df_complete$country_name)
#intersect(unique(df_complete$country_name_lower),unique(country_ref$country_name_lower))
sort(setdiff(unique(df_complete$country_name_lower),unique(country_ref$country_name_lower))) # multiple
sort(country_ref$country_name_lower)

# Change values to match ref data
# "turkey" to "türkiye"; "côte d’ivoire (x)" "côte d'ivoire"; "syria"; 
# "syrian arab republic"; "the united kingdom"; "united kingdom of great britain and northern ireland"
df_complete$country_name_lower[df_complete$country_name_lower == "côte d’ivoire"] <- "côte d'ivoire"
df_complete$country_name_lower[df_complete$country_name_lower == "turkey"] <- "türkiye"
df_complete$country_name_lower[df_complete$country_name_lower == "syria"] <- "syrian arab republic"
df_complete$country_name_lower[df_complete$country_name_lower == "the united kingdom"] <- "united kingdom of great britain and northern ireland"
df_complete$country_name_lower[df_complete$country_name_lower == "cameron"] <- "cameroon"
df_complete$country_name_lower[df_complete$country_name_lower == "car"] <- "central african republic"

# re-check
sort(setdiff(unique(df_complete$country_name_lower),unique(country_ref$country_name_lower))) 
notes <-sort(setdiff(unique(df_complete$country_name_lower),unique(country_ref$country_name_lower)))

drop_rows <- df_complete[which(df_complete$country_name_lower %in% notes),]

# all rows dropping, confirm no data
drop <- rbind(drop_rows, df_complete[is.na(df_complete$country_name),])
# unique(drop$lod_count) #NA   "na" "0" 

# drop rows with notes (not country info)
df_complete <- df_complete %>% filter(!(country_name_lower %in% notes))
sort(setdiff(unique(df_complete$country_name_lower),unique(country_ref$country_name_lower))) # 0

# drop rows without values (country names) to have final large or disruptive outbreaks
lodos <- df_complete %>% filter(!is.na(country_name_lower))

lodos$data_source <- "ia2030_2024_lodos_reporting"

#######################################################################
## MERGE DATA SETS & SAVE AS CLEAN DATA SET
## 1. IA2030 large or disruptive outbreak counts: lodos
## 2. WHO VPD case counts: vpd_cases
## 3. GID country reference data: country_ref
## 4. GID reference data: vpd_ref
#######################################################################

names(lodos)
names(vpd_cases)
names(country_ref)
names(vpd_ref)

# ensure matching primary/foreign keys: country_name_lower and vpd
sort(setdiff(unique(lodos$country_name_lower),unique(country_ref$country_name_lower))) # 0
sort(setdiff(unique(vpd_cases$country_name_lower),unique(country_ref$country_name_lower))) # 0

unique(vpd_ref$vpd)
sort(setdiff(unique(vpd_cases$vpd),unique(vpd_ref$vpd)))
sort(setdiff(unique(lodos$vpd),unique(vpd_ref$vpd)))

vpd_cases <- vpd_cases %>% mutate(vpd = case_when(
  vpd == "Congenital rubella syndrome" ~ "Congenital rubella syndrome (CRS)",
  vpd == "Neonatal tetanus" ~ "Tetanus (neonatal)",
  vpd == "Total tetanus" ~ "Tetanus (neonatal and/or non-neonatal)",
  T ~ vpd
))

lodos <- lodos %>% mutate(vpd = case_when(
  vpd == "Ebola" ~ "Ebola virus disease",
  vpd == "Meningococcus" ~ "Invasive meningococcal disease",
  vpd == "YellowFever" ~ "Yellow fever",
  T ~ vpd
))
# don't yet collapse WPV and cVDPV into polio

sort(setdiff(unique(lodos$vpd),unique(vpd_ref$vpd)))

## Merge data sets (vpd_cases, lodos, country_ref) using country_name_lower and vpd

wide_data <- vpd_cases %>% 
  full_join(lodos %>% select(-data_source),
             by = c("country_name_lower", "vpd","year"))
wide_data <- wide_data %>% select(-country_name.x, -country_name.y)

## Add reference data for countries and vpds
wide_data_ref <- merge(wide_data,country_ref, by="country_name_lower", all.x=TRUE) # left outer join
wide_data_ref <- merge(wide_data_ref, vpd_ref, by="vpd", all.x=TRUE) # left outer join

# measles incidence written as char var (cases/million/12 months), extract numeric
# clean variable
wide_data_ref$incidence[wide_data_ref$incidence=="NA" | 
    wide_data_ref$incidence=="2 of these districts with LDO were missed opportunities for vaccination, as no ICG request was submitted"
] <- NA

wide_data_ref$lod_count[wide_data_ref$lod_count=="NA"] <- NA

wide_data_ref <- wide_data_ref %>%
  mutate(
    incidence = as.numeric(gsub(" cases/million/12months", "", incidence)),
    lod_count = as.numeric(lod_count))

# correct VPD
wide_data_ref$vpd[wide_data_ref$vpd=="YellowFever"] <- "Yellow fever"

# check numeric
class(wide_data_ref$lod_count)
class(wide_data_ref$num_cases_WHOofficial)
class(wide_data_ref$incidence)

# add col for at least one LODO
wide_data_ref$lod_count_atleastone <- ifelse(is.na(wide_data_ref$lod_count), NA, 
                                             ifelse(wide_data_ref$lod_count >= 1, 1, 0))

## Create long data set using variable (epi var) and value
long_data_ref <- wide_data_ref %>%
  pivot_longer(cols = c(num_cases_WHOofficial, lod_count, lod_count_atleastone, incidence),
               names_to = "variable",
               values_to = "value")

# sort
cols_first <- c("country_name","country_abbrev","year","vpd","variable","value")
long_data_ref <-long_data_ref[,c(cols_first,
                              setdiff(names(long_data_ref), cols_first)) # other cols
]


# write clean data to ADLS
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_clean/country_vpd_year_casedata.rds", obj = long_data_ref)

