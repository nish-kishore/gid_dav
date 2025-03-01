## Combine data files imported from WHO
# Updated 2025-02-21
# Author: Lori Niehaus

#######################################################################
## PURPOSE:
## Merge WHO case and IA2030 LoD outbreak data into one table
## Visualize VPD outbreaks/cases for GID 8 Critical Countries
## Note: imported data is from downloads (excel files) from data_raw folder in ADLS; need to change based on user

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
#sirfunctions::edav_io(io = "list", default_dir = "GID/GIDMEA/giddatt/data_raw")

#######################################################################
## UPLOAD & RESAVE REFERENCE DATA (from EDAV ADLS)
#######################################################################

gid_ref_xls <- "C:/Users/tvf1/OneDrive - CDC/07_Projects/0. DATT/2025-02-21_GID_M&E_Data_Dictionary.xlsx"

# Merge with geo and vpd reference data
vpd_ref <- read_xlsx(gid_ref_xls,
                     sheet = "VPD Key") %>% clean_names() %>% as.data.frame()
country_ref <- read_xlsx(gid_ref_xls,
                         sheet = "Country Key") %>% clean_names() %>% as.data.frame()

country_ref$country_name_lower <- tolower(country_ref$country_name)

# write reference data to processing folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                                   file_loc = "data_processing/country_ref.rds",
                                   obj = country_ref)

sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/vpd_ref.rds",
                      obj = vpd_ref)

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

# Write data to processing folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/country_vpd_year_casedata.rds", obj = vpd_cases)

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

## PART 3. MERGE HISTORIC AND MOST RECENT DATA

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

# Write data to processing folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/country_vpd_year_lododata.rds", obj = lodos)


#######################################################################
## ADD ADDITIONAL ANTIGENS & YEARS (MPOX, C19, Cholera, MEASLES2024, POLIO2024)

vpd_cases <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                   file_loc = "data_processing/country_vpd_year_casedata.rds")

## query list of missing by VPD
# summary <- vpd_cases %>% filter(year==2023 & vpd=="Congenital rubella syndrome")%>%
#   group_by(vpd, country_name_lower) %>%
#   summarize(num_cases_WHOofficial)
#summary$country_name_lower[is.na(summary$num_cases_WHOofficial)]

## Clean each data set to have columns: iso3 year vpd variable value to prepare to merge with vpd_cases (long)

#############################################################################
## MPOX
#############################################################################

mpox_cases <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                   file_loc = "data_raw/mpox-cumulative-confirmed-and-suspected-cases-WHO-Ourworldindata.csv")
# Source: https://ourworldindata.org/explorers/monkeypox?Metric=Confirmed+and+suspected+cases&Frequency=7-day+average&Relative+to+population=false&country=COD~BDI~UGA~CAF
# as of Nov 2024, suspected cases no longer being reported
mpox_cases$country_name_lower <- tolower(mpox_cases$Country)
sort(setdiff(unique(mpox_cases$country_name_lower),unique(country_ref$country_name_lower)))

# change country vals
mpox_cases <- mpox_cases %>% mutate(country_name_lower = case_when(
  country_name_lower == "cote d'ivoire" ~ "côte d'ivoire",
  country_name_lower == "curacao" ~ "curaçao",
  country_name_lower == "democratic republic of congo" ~ "democratic republic of the congo",
  country_name_lower == "iran" ~ "iran (islamic republic of)",
  country_name_lower == "laos" ~ "lao people's democratic republic",
  country_name_lower == "moldova" ~ "republic of moldova",
  country_name_lower == "netherlands" ~ "netherlands (kingdom of the)",
  country_name_lower == "russia" ~ "russian federation",
  country_name_lower == "south korea" ~ "republic of korea",
  country_name_lower == "turkey" ~ "türkiye",
  country_name_lower == "united kingdom" ~ "united kingdom of great britain and northern ireland",
  country_name_lower == "united states" ~ "united states of america",
  country_name_lower == "venezuela" ~ "venezuela (bolivarian republic of)",
  country_name_lower == "vietnam" ~ "viet nam",
  T ~ country_name_lower
))

# filter out regional and global aggregates
mpox_cases_ctry <- mpox_cases %>% filter(
  !(mpox_cases$country_name_lower %in% sort(setdiff(unique(mpox_cases$country_name_lower),unique(country_ref$country_name_lower))))
)

# add ISO3 code
mpox_cases_ctry <- mpox_cases_ctry %>% left_join(
  country_ref %>% select(iso3_code, country_name_lower),
  by = "country_name_lower"
)

# reformat as date var & combine by year - cumulative so take max for year
mpox_cases_ctry$year <- mpox_cases_ctry$Day %>% as.Date(format = "%m/%d/%Y") %>% year()

mpox_cases_ctry_yr_cum <- mpox_cases_ctry %>% group_by(iso3_code, year) %>% 
  summarize(cum_confirmed_cases = max(`Total confirmed cases`, na.rm=TRUE),
           # cum_suspected_cases = ifelse(all(is.na(`Total suspected cases`)), NA, max(`Total suspected cases`, na.rm = TRUE)), # data discont Nov 2024 so handle missing
            .groups = 'drop'
            )

# recalculate total for specific year (so not cumulative across years)
mpox_cases_ctry_yr <- mpox_cases_ctry_yr_cum %>% 
  arrange(iso3_code, year) %>%  # Ensure data is sorted by iso3_code and year
  group_by(iso3_code) %>%
  mutate(
    confirmed_cases = ifelse(year == min(year), 
                             cum_confirmed_cases, 
                             cum_confirmed_cases - lag(cum_confirmed_cases,
                                                       default = first(cum_confirmed_cases)))
  ) %>%
  ungroup()

mpox_cases_ctry_yr <- mpox_cases_ctry_yr %>% mutate(vpd="Mpox", variable="cases") %>% 
  select(-cum_confirmed_cases) %>% rename(value=confirmed_cases)
  
mpox_df <- mpox_cases_ctry_yr[ , order(names(mpox_cases_ctry_yr))]

# save to processing folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/mpox-cntry-year-cases.rds",
                      obj=mpox_df)

#############################################################################
## CHOLERA
#############################################################################
cholera_cases <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                       file_loc = "data_raw/cholera-number-reported-cases.csv")

cholera_cases$country_name_lower <- tolower(cholera_cases$Entity)
#sort(setdiff(unique(cholera_cases$country_name_lower),unique(country_ref$country_name_lower)))
#sort(setdiff(unique(cholera_cases$Code),unique(country_ref$iso3_code))) # OWID_WRL

#unique(cholera_cases$Entity[cholera_cases$Code == "OWID_WRL"]) # World
#unique(cholera_cases$Entity[is.na(cholera_cases$Code)]) # Regions + Serbia and Montenegro (former)"

# correct geo code and filter out values that are not counties 
cholera_cases$Code[cholera_cases$Entity=="Serbia and Montenegro (former)"] <- "SCG"
cholera_cases_ctry_yr <- cholera_cases %>% filter(!(is.na(Code)) & Code != "OWID_WRL")

# prepare to merge
cholera_cases_ctry_yr <- cholera_cases_ctry_yr %>%
  rename(iso3_code = Code, year=Year, value = `Reported cholera cases`) %>% 
  mutate(vpd="Cholera", variable="cases") %>%
  select(-c("Entity","country_name_lower"))

cholera_df <- cholera_cases_ctry_yr[ , order(names(cholera_cases_ctry_yr))]

# save to processing folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/cholera-cntry-year-cases.rds",
                      obj=cholera_df)

#############################################################################
## COVID-19
#############################################################################
c19_cases <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                   file_loc = "data_raw/COVID-19-global-data-cases-and-deaths_WHO.csv")

#c19_cases[is.na(c19_cases$Country_code),]$Country %>% unique() # Namibia missing code (imported NA as missing)
c19_cases$Country_code[c19_cases$Country=="Namibia"] <- "NA"

#sort(setdiff(unique(c19_cases$Country_code),unique(country_ref$iso2))) # some unmatched to ref

# Unmatched geographies - international commercial vessel and conveyence - DROP
unmatched <- c19_cases[c19_cases$Country_code %in% setdiff(unique(c19_cases$Country_code),unique(country_ref$iso2)),]$Country %>% unique()
c19_cases <- c19_cases %>% filter(!Country %in% unmatched)

## check before joining
#country_ref$iso2 %>% duplicated %>% sum() # ok - NA for non-countries
#country_ref[duplicated(country_ref$iso2),] %>% View()
#is.na(c19_cases$iso2) %>% sum() # 0 - no missing vals

# add ISO3 code
c19_cases <- c19_cases %>% rename(iso2=Country_code) %>% left_join(
  country_ref %>% select(iso3_code, iso2),
  by = "iso2"
)

# reformat as date var & combine by year
c19_cases$year <- c19_cases$Date_reported %>% as.Date(format = "%m/%d/%Y") %>% year()
c19_cases_ctry_yr <- c19_cases %>% group_by(iso3_code, year) %>% 
  summarize(cases = sum(New_cases, na.rm=TRUE),
            cases_cum = sum(Cumulative_cases, na.rm=TRUE),
            deaths_new = sum(New_deaths, na.rm=TRUE),
            deaths_cum = sum(Cumulative_deaths, na.rm=TRUE),
            .groups = 'drop'
  )

c19_cases_ctry_yr <- c19_cases_ctry_yr %>% mutate(vpd="Coronavirus disease 2019 (COVID-19)") %>% 
  pivot_longer(c("cases", "cases_cum", "deaths_new", "deaths_cum"),
               names_to = "variable", values_to = "value")

c19_df <- c19_cases_ctry_yr[ , order(names(c19_cases_ctry_yr))]

# save to processing folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/covid19-cntry-year-cases.rds",
                      obj=c19_df)

#############################################################################
## MEASLES - 2024
#############################################################################

measles_cases <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                    file_loc = "data_raw/Measles-cases-by-month-year-country.csv")

measles_cases_ctry_2024 <- measles_cases %>% filter(Year==2024) %>% 
  pivot_longer(cols = names(measles_cases)[5:16], names_to = "month", values_to="cases") %>% 
  group_by(ISO3, Year) %>% summarize(cases = sum(cases, na.rm=TRUE), .groups = "drop")

measles_cases_ctry_2024 <- measles_cases_ctry_2024 %>%
  rename(iso3_code = ISO3, year=Year, value=cases) %>% 
  mutate(vpd = "Measles", variable="cases")

#sort(setdiff(unique(measles_cases_ctry_2024$iso3_code),unique(country_ref$iso3_code))) #0

measles_df <- measles_cases_ctry_2024[ , order(names(measles_cases_ctry_2024))]

# save to processing folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/measles-cntry-2024-cases.rds",
                      obj=measles_df)

#############################################################################
## Polio - 2024
#############################################################################

# polio AFP detections in 2024 by country
polio_2024cases <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                                         file_loc = "data_clean/polio.data.recent.rds")$pos |> 
  dplyr::filter(yronset == 2024, 
                source == "AFP", 
                virustype != "NPEV") |> 
  dplyr::group_by(admin0officialname) |> 
  dplyr::summarise(afp.cases = dplyr::n())

#sort(setdiff(unique(polio_2024cases$admin0officialname),unique(country_ref$country_name)))
polio_2024cases$admin0officialname[polio_2024cases$admin0officialname == "Cote d Ivoire"] <- "Côte d'Ivoire"

# add ISO3 code
pol_cases <- polio_2024cases %>% rename(country_name=admin0officialname) %>% left_join(
  country_ref %>% select(iso3_code, country_name),
  by = "country_name"
) %>% select(-country_name)

pol_cases <- pol_cases %>% rename(value=afp.cases) %>% 
  mutate(year=2024, vpd="Poliomyelitis", variable="cases") # AFP cases

polio_df <- pol_cases[ , order(names(pol_cases))]

# save to processing folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/polio-cntry-2024-cases.rds",
                      obj=polio_df)

#######################################################################
## MERGE DATA SETS & SAVE AS CLEAN DATA SET
## 1. IA2030 large or disruptive outbreak counts: lodos
## 2. WHO VPD case counts: vpd_cases
## 3. GID country reference data: country_ref
## 4. GID vpd reference data: vpd_ref
#######################################################################

# Read data from processing folder
lodos <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/country_vpd_year_lododata.rds")

vpd_cases <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/country_vpd_year_casedata.rds")

country_ref <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                   file_loc = "data_processing/country_ref.rds")

vpd_ref <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                   file_loc = "data_processing/vpd_ref.rds")

## ensure matching primary/foreign keys: country_name_lower and vpd
#sort(setdiff(unique(lodos$country_name_lower),unique(country_ref$country_name_lower))) # 0
#sort(setdiff(unique(vpd_cases$country_name_lower),unique(country_ref$country_name_lower))) # 0

# unique(vpd_ref$vpd)
# sort(setdiff(unique(vpd_cases$vpd),unique(vpd_ref$vpd)))
# sort(setdiff(unique(lodos$vpd),unique(vpd_ref$vpd)))

vpd_cases <- vpd_cases %>% mutate(vpd = case_when(
  vpd == "Congenital rubella syndrome" ~ "Congenital rubella syndrome (CRS)",
  vpd == "Neonatal tetanus" ~ "Tetanus (neonatal)",
  vpd == "Total tetanus" ~ "Tetanus (neonatal and/or non-neonatal)",
  vpd =="YellowFever" ~ "Yellow fever",
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

## Add LOD count for combined (cVDPV + WPV) polio rows
## where the VPD is "cVDPV" or "WPV" in the same country and year as where the VPD is ""Poliomyelitis"
## lod_count = cVDPV lodcount + WPV lodcount

disag_pol <- wide_data_ref %>%  filter(vpd %in% c('cVDPV', 'WPV')) %>%
  group_by(year, country_name_lower) %>%
  summarise(total_lod_count = sum(lod_count, na.rm = TRUE), .groups = 'drop')

agg_pol <- wide_data_ref %>% filter(vpd == 'Poliomyelitis')

updated_pol_rows <- agg_pol %>%
  left_join(disag_pol, by = c("year", "country_name_lower")) %>%
  mutate(lod_count = ifelse(is.na(total_lod_count), lod_count, total_lod_count)) %>%
  select(-total_lod_count) # Remove temporary column

wide_data_ref <- wide_data_ref %>%
  filter(!(vpd == 'Poliomyelitis')) %>% # Remove old Polio rows
  bind_rows(updated_pol_rows) # Add updated Polio rows

# check numeric
class(wide_data_ref$lod_count)
class(wide_data_ref$num_cases_WHOofficial)
class(wide_data_ref$incidence)

# add col for at least one LODO
wide_data_ref$lod_count_atleastone <- ifelse(is.na(wide_data_ref$lod_count), NA, 
                                             ifelse(wide_data_ref$lod_count >= 1, 1, 0))

## add col on % of total cases in each country (for each vpd-year)
case_counts_global <- wide_data_ref %>% group_by(year, vpd) %>% 
  summarize(global_case_count = sum(num_cases_WHOofficial, na.rm=TRUE))

# wide_data_ref[]
# 
# wide_data_ref$total_cntry_cases <- wide_data_ref %>% group
# wide_data_ref$total_glbl_cases <- 

## for PowerBI, need to change ROC variable so automaps correctly

# add population data to country reference data
ctry.pop <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                  file_loc = "data_clean/ctry_pop.rds")

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

# drop rows where value is missing
long_data_ref <- long_data_ref %>% filter(!is.na(value))
long_data_ref$variable[long_data_ref$variable == "num_cases_WHOofficial"] <- "cases"


# write clean data to ADLS
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_clean/country_vpd_year_casedata.rds", obj = long_data_ref)

# save as csv to be accessed in PowerBI
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_clean/country_vpd_year_casedata.csv", obj = long_data_ref)

# save as csv to be accessed in PowerBI
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_clean/country_vpd_year_casedata.csv", obj = long_data_ref)

#######################################################################
## ADD VPD CASE ADDITIONAL DATA - CHOLERA, COVID19, MPOX CASES; MEASLES & POLIO 2024 CASES
#######################################################################
rm(list=ls())

# READ IN DATA CLEANED ABOVE
data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                              file_loc = "data_clean/country_vpd_year_casedata.rds")

cholera <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                              file_loc = "data_processing/cholera-cntry-year-cases.rds")

covid19 <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                                 file_loc = "data_processing/covid19-cntry-year-cases.rds")

mpox <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                                         file_loc = "data_processing/mpox-cntry-year-cases.rds")

polio2024 <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                                 file_loc = "data_processing/polio-cntry-2024-cases.rds")

measles2024 <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                                   file_loc = "data_processing/measles-cntry-2024-cases.rds")

all_vpds <- data %>% select(iso3_code, value, variable, vpd, year)

## confirm cols in appropriate name order before row binding
# names(cholera) == names(covid19)
# names(covid19)== names(mpox)
# names(mpox)==names(polio2024)
# names(polio2024)==names(measles2024)
#names(polio2024)==names(all_vpds)

long_df <- all_vpds %>% rbind(
  cholera, covid19, mpox, polio2024, measles2024
)

long_df$variable[long_df$variable=="confirmed_cases"] <- "cases"

# save to processing folder version without reference data attached
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", 
                                     file_loc = "data_processing/vpd_cases_ctry_yr_1980-2024.rds",
                                     obj=long_df)

# remerge with most recent GID reference data for countries and VPDs
country_ref <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                                     file_loc = "data_processing/country_ref.rds")
vpd_ref <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                                     file_loc = "data_processing/vpd_ref.rds")

vpd_cases_ctry_yr_ref <- long_df %>% left_join(
  country_ref, by = "iso3_code") %>% left_join(
  vpd_ref, by = "vpd"
  )
  
# sort
cols_first <- c("iso3_code","country_name","country_abbrev","year","vpd","variable","value")
vpd_cases_ctry_yr_ref <-vpd_cases_ctry_yr_ref[,c(cols_first,
                                 setdiff(names(vpd_cases_ctry_yr_ref), cols_first)) # other cols
]


# save to clean data folder as rds and csv files
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", 
                        file_loc = "data_clean/country_vpd_year_casedata.rds",
                        obj=vpd_cases_ctry_yr_ref)

sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", 
                      file_loc = "data_clean/country_vpd_year_casedata.csv",
                      obj=vpd_cases_ctry_yr_ref)
