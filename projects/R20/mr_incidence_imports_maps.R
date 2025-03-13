# R20 - Number of Mealses Cases Imported into the US 
# Center US to the middle of the map 

# Load packages: 
source("./reference/obx_packages_01.R", local = T)
library(ggarrow)
# Load Data 
#all vpd
vpd.case.data <- sirfunctions::edav_io(io = "read", default_dir = NULL, file_loc = "GID/GIDMEA/giddatt/data_clean/country_vpd_year_casedata.rds")

#county shapes, filter to 2025 to keep maps consistent
ctry.shapes <- load_clean_ctry_sp(type = "long") |>
  select(WHO_REGION, ADM0_NAME, STARTDATE, ENDDATE, active.year.01) |>
  filter(active.year.01 == 2025)

#subset all vpd to measles cases, fixing names to conform with shapes
measles.cases <- vpd.case.data |>
  filter(vpd == "Measles" & variable == "cases") |>
  mutate(country = toupper(country_name), 
         country = case_when(country == "CÔTE D'IVOIRE" ~ "COTE D IVOIRE",
                             country == ""
         ) |>
  left_join(ctry.shapes, by = c("country" = "ADM0_NAME"))
  
missed.ctry <- measles.cases |>
  filter(is.na(active.year.01))
#mr import data
mr_data <- read_excel("reference/mr_import_01_24.xlsx")
  

#prepping data for mapping
mr_data <- left_join(mr_data, ctry.shapes, by = c("country" = "ADM0_NAME"))

measles.18.19 <- measles.cases |>
  filter(year %in% c("2018", "2019")) 