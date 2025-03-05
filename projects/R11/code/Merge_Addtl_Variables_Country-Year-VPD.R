## DETAILS ###############################################################
# Update long country-year-vpd dataset in ADLS to include additional variables
# Updated 2025-02-21
# Author: Lori Niehaus

## PURPOSE ###########################################################
## VARIABLES TO ADD
## coverage; Source: WUENIC (https://immunizationdata.who.int/)
## imm system indicator - functional NITAG
## US vaccine case data
## cost per case per year data (US MEASLES ONLY, based on DATT lit review data)
## . To produce total cost of outbreaks you will need to multiply them by the number of measles cases per year

## R SETUP #######################################################################
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


## IMPORT CLEAN DATA ############################################################################

# sirfunctions::edav_io(io = "list", default_dir = "GID/GIDMEA/giddatt/data_clean")  # view list of clean data files in ADLS

data_fi <- "data_clean/country_vpd_year_casedata.rds"
data_clean <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", #country-vpd-year-variable long dataframe
                                    file_loc = data_fi)
data <- data_clean
## Review dataset structure
#str(data)
#unique(data$vpd)
#unique(data$variable)


## IMPORT GID REFERENCE DATA #######################################################################

country_ref <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                              file_loc = "data_processing/country_ref.rds")

country_ref$country_name_lower <- tolower(country_ref$country_name)

vpd_ref <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                     file_loc = "data_processing/vpd_ref.rds")

## EBOLA CASE DATA ###########################################################################
#Source: "https://www.cdc.gov/ebola/outbreaks/index.html#cdc_listing_res-cases-and-outbreaks-of-ebola-disease-by-year"
# Data table from web scraping provided by Mary Choi NCEZID/VSBP

data_fi <- "data_raw/EbolaCases_Dat_12_6_21_LatLong_NICRD_Zaire.csv"

ebola_cases_raw <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                    file_loc = data_fi)

# str(ebola_cases_raw)
nrow(ebola_cases_raw)

## drop 3 rows at end of row data that were empty/incomplete (sums from excel)
ebola_cases <- ebola_cases_raw %>% filter(!is.na(Year))

# nrow(ebola_cases_raw) - nrow(ebola_cases) == 3 # Check TRUE

# make year 4 digits to extract year correctly using as.Date
ebola_cases$Year <- parse_date_time(ebola_cases$Year, orders = "mdy", tz = "UTC")

ebola_cases$date <- ebola_cases$Year %>% as.Date(format = "%m/%d/%Y") # change type & col name
ebola_cases$year <- ebola_cases$date  %>% lubridate::year() # extract year in new col

# Strip whitespace from all character columns
ebola_cases[] <- lapply(ebola_cases, function(x) if(is.character(x)) trimws(x) else x)

## Clean Values (cases, deaths, CFR)
ebola_cases$cases <-  gsub(",","",x=ebola_cases$Cases) %>% # remove commas
  as.numeric() # convert
ebola_cases$deaths_new <-  gsub(",","",x=ebola_cases$Deaths) %>% # remove commas
  as.numeric() # convert
ebola_cases$cfr <-  gsub("%","",x=ebola_cases$`Case Fatality Rate`) %>% # remove percent
  as.numeric()/100 # convert and keep as decimal

## Fix NA upon coercion because "space" between (trimws not working - not a blank space but diff character)
ebola_cases$deaths_new[ebola_cases$Deaths==" 0"] <- 0

## Remove original columns no longer needed
ebola_cases <- ebola_cases %>% select(-Year,-Deaths,-Cases, -`Case Fatality Rate`)

## add new column for species to simplify & add VPD name (matching GID ref data)
ebola_cases$species <- ebola_cases$Virus %>% str_extract("(?<=\\(species )[^ ]+")
ebola_cases$vpd <- 'Ebola virus disease'
ebola_cases$source <- "US CDC"

## clean country values
ebola_cases$country_name_lower <- tolower(ebola_cases$Country)
setdiff(ebola_cases$country_name_lower, country_ref$country_name_lower)

ebola_cases <- ebola_cases %>% mutate(country_name_lower = case_when(
  country_name_lower == "côte d’ivoire" ~ "côte d'ivoire",
  country_name_lower == "drc" ~ "democratic republic of the congo",
  country_name_lower == "russia" ~ "russian federation",
  country_name_lower == "united kingdom" ~ "united kingdom of great britain and northern ireland",
  country_name_lower == "republic of the congo" ~ "congo",
  T ~ country_name_lower
))

## Check - confirm TRUE
#length(setdiff(ebola_cases$country_name_lower, country_ref$country_name_lower))==0

## Add iso3 code for geography
ebola_cases <- ebola_cases %>% left_join(
  (country_ref %>% select(iso3_code, country_name, country_abbrev, country_name_lower)),
  by="country_name_lower"
)

# reorganize columns for easier use if direct access 
cols_first <- c("iso3_code","country_name","country_abbrev","year","vpd","species","cases","deaths_new","cfr","source")
ebola_cases <-ebola_cases[,c(cols_first, setdiff(names(ebola_cases), cols_first))] # other cols

# save wide data set to processing folder and then group & pivot longer
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/USA_measles_casedata.rds", obj = ebola_cases)

## Prepare for merging (aggregate data by country-year & pivot to long format)

# group by country and year, in case multiple outbreaks in same country-year
# also only include Zaire species outbreaks because vaccine-preventable
ebola_cases_merge <- ebola_cases %>% filter(species == "Zaire") %>% 
  group_by(iso3_code, year, vpd) %>% 
  summarize(
    cases = sum(cases, na.rm=TRUE), # total cases per cntry-year
    deaths_new = sum(deaths_new, na.rm=TRUE), # total deaths per cntry-year
    cfr = mean(cfr, na.rm=TRUE) # average CFR for all outbreaks in cntry-year
  )

# pivot longer so variables each have their own row in dataset
ebola_cases_merge <- ebola_cases_merge %>% pivot_longer(
  cols = c("cases", "deaths_new", "cfr"),
  names_to = "variable", values_to = "value"
)


## US MEASLES CASE DATA #######################################################################

## Source: US CDC, Downloaded from website as of 2/28/2025; Updated on February 21, 2025.
## note: 2023–2025 case counts are preliminary and subject to change.


us_measles_cases <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                              file_loc = "data_raw/2025-02-28_CDC-US-MeaslesCases_1985-2025.csv") %>% 
  mutate(iso3_code="USA", vpd="Measles")

## confirm that only one value per year - FALSE
#us_measles_cases$year %>% duplicated() %>% sum() #26 - because two "filters" applied

duplicated_year_rows <- us_measles_cases %>%
  filter(duplicated(year) | duplicated(year, fromLast = TRUE)) # don't just show the first instance of duplication (so both rows retained)

check <- duplicated_year_rows %>%
  group_by(year) %>%
  summarise(across(everything(), ~ n_distinct(.) == 1)) %>%
  ungroup()

unique_vals_columns <- colnames(check)[sapply(check, function(col) any(col == FALSE))] # filter - all other columns ok, so drop duplicates

# Drop duplicates of filter and then drop variable
us_measles_cases <- us_measles_cases %>% filter(!duplicated(us_measles_cases$year)) %>% select(-filter)

## confirm now years are unique
#us_measles_cases$year %>% duplicated() %>% sum() == 0 # TRUE

# Write data to clean folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                       file_loc = "data_clean/USA_measles_casedata.rds", obj = us_measles_cases)

## Prepare to merge with country-year-vpd-variable-value dataset
us_measles_merge <- us_measles_cases %>% 
  mutate(variable="cases", source="US CDC") %>% 
  select(iso3_code, year, vpd, variable, cases, source) %>%
  rename(value=cases)

# Write data to processing folder
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/USA_measles_casedata_long.rds", obj = us_measles_merge)

## VACCINATION COVERAGE#######################################################################

### Source 1:  CDC NIS-child (ChildVaxView) ####
# - >= 1 dose MMR coverage by age 24 mo by birth year, NIS-child, last updated Sept 26 2024
# https://data.cdc.gov/Child-Vaccinations/Vaccination-Coverage-among-Young-Children-0-35-Mon/fhky-rtsk/about_data
### NOTE - dose info still missing for some VPDs, need to find metadata or ask NCIRD

us_vaxcoverage <- get_cdc_childvaxview_data(geo_level='national')

## add vpd values corresponding to reference data where possible; for multiple antigens (MMR & DTaP), assign to "Multiple"
us_vaxcoverage$vpd <- NA
us_vaxcoverage$vpd[us_vaxcoverage$vaccine =="Hib"] <- "H. influenza type B disease"
us_vaxcoverage$vpd[us_vaxcoverage$vaccine %in% c("≥1 Dose MMR")] <- "Multiple" # Measles, Mumps, Rubella
us_vaxcoverage$vpd[us_vaxcoverage$vaccine =="Influenza"] <- "Influenza"
us_vaxcoverage$vpd[us_vaxcoverage$vaccine =="Rotavirus"] <- "Rotavirus"
us_vaxcoverage$vpd[us_vaxcoverage$vaccine %in% c("PCV")] <- "Pneumococcal disease"
us_vaxcoverage$vpd[us_vaxcoverage$vaccine %in% c("≥1 Dose Varicella")] <- "Varicella"
us_vaxcoverage$vpd[us_vaxcoverage$vaccine %in% c("Hep B")] <- "Hepatitis B"
us_vaxcoverage$vpd[us_vaxcoverage$vaccine %in% c("Hep A")] <- "Hepatitis A"
us_vaxcoverage$vpd[us_vaxcoverage$vaccine %in% c("Polio")] <- "Poliomyelitis"
us_vaxcoverage$vpd[us_vaxcoverage$vaccine %in% c("DTaP")] <- "Multiple" # Diphtheria, Tetanus, Pertussis
us_vaxcoverage$vpd[us_vaxcoverage$vaccine %in% c("Combined 7 Series")] <- "Multiple" # received combined 7-vaccine series by age 24 months

# Confirm all df rows have vpd value assigned
#sum(is.na(us_vaxcoverage$vpd))==0 # CHECK - TRUE

### Interpretation Notes for Combined 7 vaccine series (4:3:1:3*:3:1:4) includes:
## ≥4 doses of diphtheria and tetanus toxoids and acellular pertussis vaccine;
## ≥3 doses of poliovirus vaccine;
## ≥1 dose of measles-containing vaccine;
## ≥3 or ≥4 doses (depending upon product type) of Haemophilus influenzae type b conjugate vaccine;
## ≥3 doses of hepatitis B vaccine;
## ≥1 dose of varicella vaccine; and
## ≥4 doses of pneumococcal conjugate vaccine.

# Simplify values by separating dose from vaccine, where together
us_vaxcoverage$dose[us_vaxcoverage$vaccine %in% c("≥1 Dose MMR")] <- "≥1 Dose"
us_vaxcoverage$vaccine[us_vaxcoverage$vaccine %in% c("≥1 Dose MMR")] <- "MMR"
us_vaxcoverage$dose[us_vaxcoverage$vaccine %in% c("≥1 Dose Varicella")] <- "≥1 Dose"
us_vaxcoverage$vaccine[us_vaxcoverage$vaccine %in% c("≥1 Dose Varicella")] <- "Varicella"
us_vaxcoverage$dose[us_vaxcoverage$vaccine %in% c("Combined 7 Series")] <- "Full series"

# Value vales - convert to numeric
us_vaxcoverage$coverage_estimate <- as.numeric(us_vaxcoverage$coverage_estimate)
us_vaxcoverage$population_sample_size <- as.numeric(us_vaxcoverage$population_sample_size)
us_vaxcoverage$lb_95ci <- us_vaxcoverage$`_95_ci` %>% str_extract("^[0-9]+\\.[0-9]+") %>% as.numeric() # extract #s before "to"
us_vaxcoverage$ub_95ci <- us_vaxcoverage$`_95_ci` %>% str_extract("(?<=to )([0-9]+\\.[0-9]+)") %>% as.numeric() # extract #s after "to"

## Geography Values
#unique(us_vaxcoverage$geography) # US
us_vaxcoverage <- us_vaxcoverage %>% mutate(
  iso3_code = "USA"
)

## Year values
# "year_season" variable = The birth year, two-year birth cohort, or four-year birth cohort of children for which estimates are calculated.

## SAVE DATA 
# Write data to clean folder, may be accessed separately
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_clean/cdc_usnatlvaxcoverage.rds", obj = us_vaxcoverage)

## Further filtering of data set
## EXAMPLE - save MMR year-cohort dataset - age==24 months only (to match general/WHO def for MCV1 coverage)
# proportion of children less than 12-23 months of age receiving one dose of measles-containing vaccine.
mmr <- us_vaxcoverage %>% filter(vaccine=="MMR" & dimension_type =="Age" & dimension=="24 Months" & nchar(year_season)==4)
mmr$year_season <- as.numeric(mmr$year_season)

## CHECK - confirm no years duplicated
#mmr$year_season %>% duplicated() %>% sum() == 0 # TRUE

# write mmr data to processing
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/cdc_usnatlmmrcoverage.rds", obj = mmr)


### Source 2: WUENIC 2023 revision  ####
#(UNICEF Data warehouse) MCV, 1st dose = official and WUENIC (2000 - 2023)
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
unique(vpd_ref$vpd)

wuenic$vpd <- NA
wuenic$vpd[wuenic$vaccine =="BCG"] <- "Tuberculosis"
wuenic$vpd[wuenic$vaccine =="HPV"] <- "Cervical cancer (HPV)"
wuenic$vpd[wuenic$vaccine =="HIB3"] <- "H. influenza type B disease"
wuenic$vpd[wuenic$vaccine %in% c("MCV1","MCV2")] <- "Measles"
wuenic$vpd[wuenic$vaccine =="RCV1"] <- "Rubella"
wuenic$vpd[wuenic$vaccine =="ROTAC"] <- "Rotavirus"
wuenic$vpd[wuenic$vaccine =="YFV"] <- "Yellow fever"
wuenic$vpd[wuenic$vaccine %in% c("MENGA")] <- "Bacterial meningitis"
wuenic$vpd[wuenic$vaccine %in% c("PCV3")] <- "Pneumococcal disease"
wuenic$vpd[wuenic$vaccine %in% c("PAB")] <- "Tetanus (neonatal)" # CHECK
wuenic$vpd[wuenic$vaccine %in% c("HEPB3","HEPBB")] <- "Hepatitis B"
wuenic$vpd[wuenic$vaccine %in% c("IPV1","IPV2", "POL3")] <- "Poliomyelitis"
wuenic$vpd[wuenic$vaccine %in% c("DTP1","DTP3")] <- "Multiple"

## CHECK
#is.na(wuenic$vpd) %>% sum() #0 - no missing vpd vals

## Remove country aggregates (regions, economic status, etc.) - prefixes end with "_"
# UNICEF, UNSDG, AUREC, "AU_"
aggregates <- wuenic$geography[which(wuenic$geography %>% str_detect("_"))] %>% unique()
wuenic <- wuenic %>% filter(!geography %in% aggregates)

## extract iso3code and ensure matches iso3 codes in reference data
wuenic$iso3_code <- wuenic$geography %>% str_sub(1,3)
#setdiff(unique(wuenic$iso3_code), unique(country_ref$iso3_code)) # CHECK - 0


# Write data to clean folder, may be accessed separately
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_clean/wuenic_2023rev.rds", obj = wuenic)


## GLOBAL MEASLES CASE & MORTALITY ESTIMATES ###########################################
## Source: Penn State model (by Matt Ferrari, used in MMWR) - not public/shared by MET for internal use only

## US MEASLES COST PER CASE ESTIMATES ###########################################
## Source: 2025-03-04_GID_Average Cost per Measles Case_updated (generated by GID Econ Unit, based on lit review)
## interpolated values to get one per year from 2005-2018 and generated 5 diff scenarios for averaging cost estimates from each study to get one per year

measles_costs <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                file_loc = "data_raw/2025-03-04_GID_Average Cost per Measles Case_updated.csv") %>% 
  rename(year=Year) %>% mutate(vpd="Measles", iso3_code="USA", source="GID DATT lit review")

## merge with US measles case data
# read wide US measles case/outbreak data from clean data folder
measles_obr_cases <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                       file_loc = "data_clean/USA_measles_casedata.rds")

# full join on year cost per case data & case/outbreak count data
measles_costs <- measles_costs %>% full_join(measles_obr_cases %>% select(-iso3_code,-vpd), by="year")

# calculate cost per case and calculate cost per outbreak
measles_costs <- measles_costs %>% mutate(
  total_cost_cases_allpayers = avg_cost_per_case_allpayers * cases,
  total_cost_cases_govpayers = avg_cost_per_case_govpayers * cases,
)

# merge on year with US MMR coverage data
# read wide US MMR coverage data
mmr_cov <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt",
                                           file_loc = "data_processing/cdc_usnatlmmrcoverage.rds") %>% 
  rename(year = year_season, MMRcoverage_cohortage = dimension) %>% select(-vpd, -dimension_type, -geography_type, -iso3_code)

# full join on year cost per case data & case/outbreak count data
measles_costs <- measles_costs %>% full_join(mmr_cov, by="year")

# reorder columns
cols_first <- c("iso3_code","vpd","year")
measles_costs <- measles_costs[,c(cols_first, setdiff(names(measles_costs), cols_first))] # other cols

# Write data to clean folder, may be accessed separately
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_clean/USmeasles-avg-annual-cost-per-case_2005-2018.csv", obj = measles_costs)

# make long 
measles_costs_merge <- measles_costs %>% select(iso3_code,vpd,year, Scenario,avg_cost_per_case_allpayers,avg_cost_per_case_govpayers,
                                              total_cost_cases_allpayers,total_cost_cases_govpayers) %>% 
  pivot_longer(cols= c("avg_cost_per_case_allpayers", "avg_cost_per_case_govpayers",
                                                            "total_cost_cases_allpayers","total_cost_cases_govpayers"),
                                                    names_to = "variable", values_to = "value"
  ) %>% 
  filter(!is.na(value)) # drop year-variable rows where there is no data

# Write data to clean folder, may be accessed separately
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_processing/USmeasles-avg-annual-cost-per-case_2005-2018.rds", obj = measles_costs_merge)


## DENGUE CASE ESTIMATES ###########################################
## CERVICAL CANCER CASE ESTIMATES ###########################################

###Third merge - Addtl Cntry-Yr-VPD-Variable-Value Data ###############################################
## Ebola (ebola_cases_merge): Variables - Total Cases, Total Deaths, Mean CFR; (1976-2022); source: US CDC (shared by NCEZID); Zaire virus only
## US Measles cases (us_measles_merge): Variables - Cases; source: US CDC (NCIRD online public data)
## US Measles cost per case (measles_costs_merge): Variables - avg_cost_per_case_allpayers, avg_cost_per_case_govpayers, total_cost_cases_allpayers, total_cost_cases_govpayers
## wuenic and US coverage

#rm(list=ls())

# READ IN DATA CLEANED
data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                              file_loc = "data_clean/country_vpd_year_casedata.rds")

all_vpds <- data %>% select(iso3_code, value, variable, vpd, year) # select minimal columns (remove reference data)

# bind rows
# long_df <- all_vpds %>% bind_rows(
#   ebola_cases_merge, measles_costs_merge, wuenic
# )

# temp -add ebola for fig
long_df <- all_vpds %>% bind_rows(
  ebola_cases_merge
)

# replace existing rows for measles cases
us_measles_merge



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

### Save clean ctry-year-var-val dataset ###############################################
# save to clean data folder as rds and csv files
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", 
                      file_loc = "data_clean/country_vpd_year_casedata.rds",
                      obj=vpd_cases_ctry_yr_ref)

sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", 
                      file_loc = "data_clean/country_vpd_year_casedata.csv",
                      obj=vpd_cases_ctry_yr_ref)

