# Author: Lori Niehaus
# M&E Data dictionary version as of 3/12/2025

## PURPOSE ###############################################################
## Extract GID M&E Data Dictionary (reference data) from GID MEA KX Document Library
## Select data cleaning and joining (e.g., country shape file data to be matched)
## Save each Key separately in clean DATT ADLS data folder (all sheets with 'key' in sheet name)
## R SETUP ###########################################################################

rm(list=ls()) # clear

## Install dependencies
required.packages <- c("tidyverse", "AzureStor","Microsoft365R", "readxl", "janitor","scales", "devtools")
if(length(packages.to.install) >0) install.packages(packages.to.install)

# GID PEB SIR Team functions - use to read data from ADLS
devtools::install_github("nish-kishore/sirfunctions")
# https://github.com/nish-kishore/sirfunctions

# load libraries
lapply(c("tidyverse", "readxl", "AzureStor", "Microsoft365R", "janitor", "scales", "sirfunctions", "sf"), library, character.only = TRUE)

### READ GID REFERENCE DATA ####
##  Excel file source - eventually to update to automate, for now, download most recent version
# kxsite_url <- "https://cdc.sharepoint.com/:x:/r/sites/CGH-GID-Knowledge-Exchange/OD/_layouts/15/Doc.aspx?sourcedoc=%7B3DCC0D8A-792B-4AF7-9770-4301566DA43D%7D&file=GID_M%26E_Data_Dictionary.xlsx&action=default&mobileredirect=true"

## replace to upload excel when function updated
# file name: GID_M&E_Data_Dictionary.xlsx
gid_ref <- explore_edav("GID/GIDMEA/giddatt/data_raw") # 
# Response 1 - 3 (download R object); Response 2 - 13 (line number)

# simplify table names for easy reference
gid_ref <- gid_ref %>% clean_names() # make table names lowercase and replace spaces with underscores


## CLEAN REFERENCE DATA

## CTRY KEY ####
# For country key, attach color hex codes and merge with shape file
gid_ref$country_key <- gid_ref$country_key %>% clean_names()

### Update shape file data ####

## read in shape data
ctry.sp <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/ctry_shapes.rds")

setdiff(ctry.sp$iso_a3, gid_ref$country_key$iso3_code)
norefdata <- ctry.sp[ctry.sp$iso_a3 %in% setdiff(ctry.sp$iso_a3,gid_ref$country_key$iso3_code), ] # geographies that won't match; -99 missing
norefdata$admin # geometries that don't match country reference

# update iso3 codes for geometries that are "true countries"
# for geographies without officialiso3 codes, set to 3-digit number so no missing
ctry.sp <- ctry.sp %>% mutate(iso_a3 = case_when(
  admin == "Norway" ~ "NOR",
  admin == "France" ~ "FRA",
  admin == "Kosovo" ~ "RKS",
  admin == "Somaliland" ~ "000",
  admin == "Northern Cyprus" ~ "111",
  admin == "Indian Ocean Territories" ~ "222",
  admin == "Ashmore and Cartier Islands" ~ "333",
  admin == "Siachen Glacier" ~ "444",
  TRUE ~ iso_a3
)) %>% rename(iso3_code = iso_a3)

# for non "true countries", update country ref data so no data loss if vaues exist
norefdata <- ctry.sp[ctry.sp$iso3_code %in% setdiff(ctry.sp$iso3_code,gid_ref$country_key$iso3_code), ]
#setdiff(ctry.sp$iso3_code, gid_ref$country_key$iso3_code)

# Prepare data to integrate with country ref table
new_rows <- norefdata %>%
  select(admin, iso3_code, region_un) %>% # Select relevant columns
  rename(country_name = admin) %>% # Rename for consistency
  mutate(gid_primary_geography = "Excluded", # Fill in default values for other columns
         country_abbrev = iso3_code, # set as same
         who_member = "not",
         gid_region_abbr = NA,
         gid_region_name_primary = NA,
         iso2 = NA,
         gid_critical_countries = "No",
         who_region = NA,
         unicef_region = NA) %>% 
  select(gid_primary_geography, iso3_code, country_name, country_abbrev, everything()) # Ensure column order matches

# Where considered part of WHO region, add; otherwise, leave as NA
# others are contested regions or territories w/o perm. populations - not considered as part of WHO region
new_rows <- new_rows %>% mutate(gid_region_abbr = case_when(
  country_name == "Somaliland" ~ "EMR", # part of Somalia
  country_name == "Aland" ~ "EUR", # autonomous region of Finland
  country_name == "Northern Cyprus" ~ "EUR", # recognized only by Turkey; others part of Cyprus
  country_name == "Indian Ocean Territories" ~ "WPR",
  country_name == "Norfolk Island" ~ "WPR",
  TRUE ~ gid_region_abbr # keep others as missing
  )
)

new_rows <- new_rows %>% mutate(gid_region_name_primary = case_when(
  country_name == "Somaliland" ~ "Eastern Mediterranean (EMR)", # part of Somalia
  country_name == "Aland" ~ "Europe (EUR)", # autonomous region of Finland
  country_name == "Northern Cyprus" ~ "Europe (EUR)", # recognized only by Turkey; others part of Cyprus
  country_name == "Indian Ocean Territories" ~ "Western Pacific (WPR)",
  country_name == "Norfolk Island" ~ "Western Pacific (WPR)",
  TRUE ~ gid_region_name_primary # keep others as missing
  )
)

new_rows <- new_rows %>% mutate(who_region = case_when(
  country_name == "Somaliland" ~ "EMRO", # part of Somalia
  country_name == "Aland" ~ "EURO", # autonomous region of Finland
  country_name == "Northern Cyprus" ~ "EURO", # recognized only by Turkey; others part of Cyprus
  country_name == "Indian Ocean Territories" ~ "WPRO",
  country_name == "Norfolk Island" ~ "WPRO",
  TRUE ~ who_region # keep others as missing
  )
)

new_rows <- new_rows %>% mutate(unicef_region = case_when(
  country_name == "Somaliland" ~ "ESARO", # part of Somalia
  country_name == "Aland" ~ "ECARO", # autonomous region of Finland
  country_name == "Northern Cyprus" ~ "ECARO", # recognized only by Turkey; others part of Cyprus
  country_name == "Indian Ocean Territories" ~ "EAPRO",
  country_name == "Norfolk Island" ~ "EAPRO",
  TRUE ~ unicef_region # keep others as missing
  )
)

new_rows <- new_rows %>% mutate(iso2 = case_when(
  country_name == "South Georgia and the Islands" ~ "GS", 
  country_name == "British Indian Ocean Territory" ~ "IO", 
  country_name == "French Southern and Antarctic Lands" ~ "TF", 
  country_name == "Aland" ~ "AX",
  country_name == "Heard Island and McDonald Islands" ~ "HM",
  country_name == "Norfolk Island" ~ "NF",
  TRUE ~ iso2 # keep others as missing
  )
)

# Bind new rows with existing country ref data
all_country_data <- bind_rows(gid_ref$country_key, new_rows)

## CHECK - NO ISO3 DUPLICATES
#duplicated(all_country_data$iso3_code) %>% unique() # FALSE

## CHECK- ALL ROWS RETAINED - TRUE
#nrow(gid_ref$country_key)+nrow(norefdata)==nrow(all_country_data)

# Calculate centroids for each geometry & extract coords and add to shapefile
ctry.sp <- ctry.sp %>%
  mutate(centroid_longitude = centroid_coords[, 1],   # Longitude from centroid_coords
         centroid_latitude = centroid_coords[, 2],    # Latitude from centroid_coords
         centroid_geometry = st_geometry(centroids) # centroid point geo
  ) %>% select(-gid_priority) # remove

# attach country names with matching iso3_codes
ctry.sp_updated <- ctry.sp %>% left_join(
  all_country_data, by="iso3_code"
) %>% select(-admin) # duplicative column with country name


### Add color scheme for critical countries ####
all_country_data <- all_country_data %>% mutate(country_hex = case_when(
  iso3_code=="AFG" ~ "#a51d42",
  iso3_code=="BRA" ~ "#D5006D",
  iso3_code=="COD" ~ "#00a266",
  iso3_code=="ETH" ~ "#007B7A",
  iso3_code=="IDN" ~ "#582d90",
  iso3_code=="NGA" ~ "#334a54",
  iso3_code=="PAK" ~ "#D76B6E",
  iso3_code=="PHL" ~ "#A76DAB",
  TRUE ~ NA # no colors assigned yet to other countries
  )
)

# Binary color scheme
all_country_data <- all_country_data %>% mutate(cc_hex_bin = case_when(
  gid_critical_countries=="Yes" ~ "#007B7A", # is GID CC
  gid_critical_countries=="No"~ "#afabab", # is NOT GID CC
  TRUE ~ NA # no colors assigned yet to other countries
  )
)

## resave country shape file 
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", 
                      file_loc = "data_clean/ctry_shapes_updated.rds", obj=ctry.sp_updated)


## Join ctry reference  & spatial data, not retaining as sf class
all_country_data <- all_country_data %>% left_join(
  ctry.sp, by="iso3_code"
)

## resave country reference data
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", 
                      file_loc = "data_clean/ref_country.rds", obj=all_country_data)

## VPD KEY ####
### Add color scheme ####
# For VPD Key, attach color hex codes and covert variables to factors
gid_ref$vpd_key <- gid_ref$vpd_key %>% clean_names() %>% mutate(
  vpd_hex = case_when(
    vpd_short_name=="Measles" ~ "#a51d42",
    vpd_short_name=="Rubella" ~ "#CC79A7",
    vpd_short_name=="CRS" ~ "#FF9999",
    vpd_short_name=="Polio" ~ "#009E73",
    vpd_short_name=="Cholera" ~ "#0072B2",
    vpd_short_name=="Ebola" ~ "#FF5733",
    vpd_short_name=="Mpox" ~ "#C77CFF",
    vpd_short_name=="Meningitis" ~ "#D55E00",
    vpd_short_name=="Diphtheria" ~ "#6A5ACD",
    vpd_short_name=="Yellow fever" ~ "#E69F00",
    vpd_short_name=="Neonatal tetanus" ~ "#86e0b5",
    vpd_short_name=="Tetanus (all)" ~ "#A5C1BA",
    vpd_short_name=="Typhoid" ~ ,"#8DA0CB",
    vpd_short_name=="COVID-19" ~ ,"#A3A500",
    vpd_short_name=="Jap encephalitis" ~ ,"#007B7A",
    vpd_short_name=="Pertussis" ~ ,"#041c3a",
    vpd_short_name=="Mumps" ~ ,"#66B3FF",
    TRUE ~ "#afabab" # temp - use gray for others/not assigned
  )
)

top_priority_vpds <- c("Polio","cVDPV","WPV","Measles", # GID appropriations
                   "Rubella", "CRS", # GID appropriations adjacent
                   "Ebola","Cholera","Yellow fever", "Mpox","Diphtheria", "COVID-19", # large or disruptive outbreaks
                   "Typhoid", "Hep B", "Neonatal Tetanus", "Tetanus (all)", "Cervical cancer") # other vaccine priorities

lower_priority_vpds <- setdiff(unique(gid_ref$vpd_key$vpd_short_name),priority_vpds)

std_vpd_order <- c(top_priority_vpds, lower_priority_vpds)

# new column to specify
gid_ref$vpd_key <- gid_ref$vpd_key %>% mutate(gid_priority_bin=case_when(
  vpd_short_name %in% top_priority_vpds ~ "Yes",
  TRUE ~ "No"
))

# convert to factor
gid_ref$vpd_key$vpd_short_name <- factor(gid_ref$vpd_key$vpd_short_name,
                                         levels = std_vpd_order)


## SAVE REF DATA TO ADLS ####
## Save key tables as R dataframes to GID DATT ADLS
for (table_name in names(gid_ref) ) {
  
  # if the table is the country key or not a key, skip it
  if ( table_name == "country_key" | (!grepl("key", table_name)) ) { # exclude (skip in loop)
    next
  }
  
  # otherwise, save to ADLS folder
  prefix <- "data_clean/ref_"
  fi_name <- paste0(prefix, sub("_key$", "", table_name),".rds")
  
  # save
  sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", 
                        file_loc = fi_name, obj=gid_ref[[table_name]])
  
  cat("Saved:", fi_name, "\n")  # Print confirmation message
  
}


