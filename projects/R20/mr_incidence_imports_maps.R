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
  select(WHO_REGION, ADM0_NAME, CENTER_LON, CENTER_LAT, active.year.01) |>
  filter(active.year.01 == 2025)

#subset all vpd to measles cases, fixing names to conform with shapes
measles.cases <- vpd.case.data |>
  filter(vpd == "Measles" & variable == "cases") |>
  mutate(country = toupper(country_name),
         country = case_when(country == "CÔTE D'IVOIRE" ~ "COTE D IVOIRE",
                             country == "KOSOVO (IN ACCORDANCE WITH UN SECURITY COUNCIL RESOLUTION 1244 (1999))" ~ "KOSOVO", 
                             country == "NETHERLANDS (KINGDOM OF THE)" ~ "NETHERLANDS", 
                             country == "NORTHERN MARIANA ISLANDS" ~ "NORTHERN MARIANA ISLANDS (COMMONWEALTH OF THE)", 
                             country == "SINT MAARTEN (DUTCH PART)" ~ "SINT MAARTEN", 
                             country == "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND" ~ "THE UNITED KINGDOM", 
                             .default = country)) 

#mr import data
mr.data <- read_excel("reference/mr_import_01_24.xlsx")
  
#prepping data for mapping
ctry.mr.data <- left_join(mr.data, ctry.shapes, by = c("country" = "ADM0_NAME")) |>
  filter(!is.na(CENTER_LON))
  
#calculate sum for years specified by country and join to shapes
measles.18.19 <- measles.cases |>
  filter(year %in% c("2018", "2019")) |>
  select(-year) |>
  group_by(country) |>
  mutate(value = sum(value)) |>
  ungroup() |>
  unique() |>
  left_join(ctry.shapes, by = c("country" = "ADM0_NAME")) |>
  left_join(mr.data, by = c("country")) |>
  group_by(who_region) |>
  mutate(region_imports_13_24 = sum(mr_im_13_24, na.rm = T),
         mr_im_13_24 = ifelse(mr_im_13_24 == 0, NA, mr_im_13_24)) |>
  ungroup()

importations.region <- measles.18.19 |>
  select(who_region, region_imports_13_24) |>
  unique() |>
  mutate(CENTER_LAT = case_when(who_region == "AFRO" ~ 15.00, 
                                who_region == "AMRO" ~ -15.00, 
                                who_region == "EMRO" ~ 30.00, 
                                who_region == "EURO" ~ 50.00,
                                who_region == "SEARO" ~ 23.00,
                                who_region == "WPRO" ~ 36.00), 
         CENTER_LON = case_when(who_region == "AFRO" ~ 20.00, 
                                who_region == "AMRO" ~ -55.00, 
                                who_region == "EMRO" ~ 42.00, 
                                who_region == "EURO" ~ 20.00, 
                                who_region == "SEARO" ~ 80.00, 
                                who_region == "WPRO" ~ 100.00))

US_CENTER_LON <- ctry.shapes |>
  filter(ADM0_NAME == "UNITED STATES OF AMERICA") |>
  pull(CENTER_LON)
US_CENTER_LAT <- ctry.shapes |>
  filter(ADM0_NAME == "UNITED STATES OF AMERICA") |>
  pull(CENTER_LAT)

ggplot(data = measles.18.19$Shape) +
  geom_sf(aes(fill = measles.18.19$value)) +
  geom_sf_text(aes(label = measles.18.19$mr_im_13_24)) +
  scale_fill_viridis_c(trans = scales::log10_trans(), labels = scales::comma, option = "cividis") 

ggplot(data = measles.18.19$Shape) +
  geom_sf(aes(fill = measles.18.19$value)) +
  geom_sf_text(aes(label = measles.18.19$mr_im_13_24)) +
  scale_fill_viridis_c(trans = scales::log10_trans(), labels = scales::comma, option = "cividis") +
  geom_arrow_curve(data = importations.region , aes(x = CENTER_LON, y = CENTER_LAT, 
                                     xend = US_CENTER_LON, yend = US_CENTER_LAT, 
                                     linewidth = region_imports_13_24), 
                   mid_place = 1, col = "black", curvature = .3,
                   lineend = "square") +
  theme_bw()

ggplot(data = measles.18.19$Shape) +
  geom_sf(aes(fill = measles.18.19$mr_im_13_24)) +
  geom_sf_text(aes(label = measles.18.19$mr_im_13_24)) + 
  scale_fill_viridis_c(trans = scales::log10_trans(), labels = scales::comma, option = "cividis") +
  theme_bw()
 
  