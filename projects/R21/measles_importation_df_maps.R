#dependencies
library(tidyverse)
library(tidyr)
library(readxl)
library(sirfunctions)
library(ggplot2)
library(here)

global.ctry <- sirfunctions::load_clean_ctry_sp()
long.global.ctry <- sirfunctions::load_clean_ctry_sp(type = "long")

#read in and process data
#region.imports.count <- readxl::read_excel("C:/Users/uic3/Desktop/DATT/measles_importation.xlsx", 
 #                                          sheet = "region_imports")
#region.import.per <- readxl::read_excel("C:/Users/uic3/Desktop/DATT/measles_importation.xlsx", 
 #                                       sheet = "region_imports_per")
#import.cases.count <- readxl::read_excel("C:/Users/uic3/Desktop/DATT/measles_importation.xlsx", 
 #                                        sheet = "import_cases")
#import.cases.per <- readxl::read_excel("C:/Users/uic3/Desktop/DATT/measles_importation.xlsx", 
 #                                      sheet = "import_cases_per")

#measles.import.data.list <- list()
#measles.import.data.list$region.imports.count <- region.imports.count
#measles.import.data.list$region.import.percentage <- region.import.per
#measles.import.data.list$import.cases.count <- import.cases.count
#measles.import.data.list$import.cases.percentage <- import.cases.per

#sirfunctions::edav_io(io = "write", obj = measles.import.data.list, default_dir = "GID/GIDMEA/giddatt", 
 #                     file_loc = "data_clean/measles.import.data.list.rds")

#import data list
measles.import.data.list <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                                                  file_loc = "data_clean/measles.import.data.list.rds")

#make importation case counts long and attach to 2024 country shapes by region
#using 2024 country shapes because maps are at region level so year specific countries aren't that important 
#compared to keeping shapes constant for year to year maps
ctry.24.shapes <- long.global.ctry |>
  dplyr::filter(active.year.01 == 2024) 

region.imports.count <- measles.import.data.list$region.imports.count |>
  tidyr::pivot_longer("Total":"2024") |>
  dplyr::rename("year" = "name", 
                "case_count" = "value") |>
  dplyr::arrange(year) 

region.counts.shape <- dplyr::left_join(ctry.24.shapes, region.imports.count, by = c("WHO_REGION" = "Source_WHO_region")) |>
  dplyr::mutate(case_count = ifelse(ADM0_NAME == "UNITED STATES OF AMERICA", 0, case_count), 
                WHO_REGION = case_when(WHO_REGION == "AMRO" ~ "AMR",
                                       WHO_REGION == "AFRO" ~ "AFR",
                                       WHO_REGION == "EMRO" ~ "EMR", 
                                       WHO_REGION == "EURO" ~ "EUR", 
                                       WHO_REGION == "SEARO" ~ "SEAR", 
                                       WHO_REGION == "WPRO" ~ "WPR"))
  
#total importations
ggplot(region.counts.shape |> dplyr::filter(year == "Total")) +
  geom_sf(aes(fill = WHO_REGION), color = NA) +
  scale_fill_manual(values = c("AFR" = "#d8b365", "AMR" = "#5ab4ac", "EMR" = "#ef8a62", "EUR" = "#91cf60", "SEAR" = "#998ec3", "WPR" = "#f1a340")) +
  geom_sf_text(data = region.counts.shape |> 
                 dplyr::filter(ADM0_NAME %in% c("BRAZIL", "DEMOCRATIC REPUBLIC OF THE CONGO", "SAUDI ARABIA", "BELARUS", "CHINA", "INDIA") & year == "Total"), 
               aes(label = case_count), color = "black") +
  geom_sf(data = ctry.24.shapes |> dplyr::filter(ADM0_NAME == "UNITED STATES OF AMERICA")) + 
  theme_bw() +
  labs(y = "", x = "") +
  labs(fill = "Region") +
  ggtitle("Measles Cases Imported to US by WHO Region, 2020 - 2024")

ggsave(here("projects/R21/outputs/measles_total_import_num.png"), width = 10, height = 6, dpi = 300)

#facet wrap for each year 2020 - 2024
ggplot(region.counts.shape |> dplyr::filter(!year %in% c("Total", "2020–2023"))) +
  geom_sf(aes(fill = WHO_REGION), color = NA) +
  scale_fill_manual(values = c("AFR" = "#d8b365", "AMR" = "#5ab4ac", "EMR" = "#ef8a62", "EUR" = "#91cf60", "SEAR" = "#998ec3", "WPR" = "#f1a340")) +
  geom_sf_text(data = region.counts.shape |> 
                 dplyr::filter(ADM0_NAME %in% c("BRAZIL", "DEMOCRATIC REPUBLIC OF THE CONGO", "SAUDI ARABIA", "BELARUS", "CHINA", "INDIA") & !year %in% c("Total", "2020–2023")), 
               aes(label = case_count), color = "black") +
  geom_sf(data = ctry.24.shapes |> dplyr::filter(ADM0_NAME == "UNITED STATES OF AMERICA")) + 
  facet_wrap(vars(year)) +
  labs(y = "", x = "") +
  theme_bw() +
  labs(fill = "Region") +
  ggtitle("Measles Cases Imported to US by WHO Region and Year, 2020 - 2024")

ggsave(here("projects/R21/outputs/measles_yearly_import_num.png"), width = 15, height = 6, dpi = 300)

#total percentages of imported measles cases
region.imports.per <- measles.import.data.list$region.import.percentage |>
  tidyr::pivot_longer("Total":"2024") |>
  dplyr::rename("year" = "name", 
                "percentage" = "value") |>
  dplyr::arrange(year)

region.percents.shape <- dplyr::left_join(ctry.24.shapes, region.imports.per, by = c("WHO_REGION" = "Source_WHO_region")) |>
  dplyr::mutate(percentage = ifelse(ADM0_NAME == "UNITED STATES OF AMERICA", 0, percentage), 
                WHO_REGION = case_when(WHO_REGION == "AMRO" ~ "AMR",
                                       WHO_REGION == "AFRO" ~ "AFR",
                                       WHO_REGION == "EMRO" ~ "EMR", 
                                       WHO_REGION == "EURO" ~ "EUR", 
                                       WHO_REGION == "SEARO" ~ "SEAR", 
                                       WHO_REGION == "WPRO" ~ "WPR"))


#percentage of importations
ggplot(region.percents.shape |> dplyr::filter(year == "Total")) +
  geom_sf(aes(fill = WHO_REGION), color = NA) +
  scale_fill_manual(values = c("AFR" = "#d8b365", "AMR" = "#5ab4ac", "EMR" = "#ef8a62", "EUR" = "#91cf60", "SEAR" = "#998ec3", "WPR" = "#f1a340")) +
  geom_sf_text(data = region.percents.shape |> 
                 dplyr::filter(ADM0_NAME %in% c("BRAZIL", "DEMOCRATIC REPUBLIC OF THE CONGO", "SAUDI ARABIA", "BELARUS", "CHINA", "INDIA") & year == "Total"), 
               aes(label = paste0(100 * percentage, "%")), color = "black") +
  geom_sf(data = ctry.24.shapes |> dplyr::filter(ADM0_NAME == "UNITED STATES OF AMERICA")) + 
  theme_bw() +
  labs(y = "", x = "") +
  labs(fill = "Region") +
  ggtitle("Percentage of Measles Cases Imported to US by WHO Region, 2020 - 2024, N = 93")

ggsave(here("projects/R21/outputs/measles_total_import_per.png"), width = 10, height = 6, dpi = 300)

#facet wrap percentage of importations by year
ggplot(region.percents.shape |> dplyr::filter(!year %in% c("Total", "2020–2023"))) +
  geom_sf(aes(fill = WHO_REGION), color = NA) +
  scale_fill_manual(values = c("AFR" = "#d8b365", "AMR" = "#5ab4ac", "EMR" = "#ef8a62", "EUR" = "#91cf60", "SEAR" = "#998ec3", "WPR" = "#f1a340")) +
  geom_sf_text(data = region.percents.shape |> 
                 dplyr::filter(ADM0_NAME %in% c("BRAZIL", "DEMOCRATIC REPUBLIC OF THE CONGO", "SAUDI ARABIA", "BELARUS", "CHINA", "INDIA") & !year %in% c("Total", "2020–2023")), 
               aes(label = paste0(100 * percentage, "%")), color = "black") +
  geom_sf(data = ctry.24.shapes |> dplyr::filter(ADM0_NAME == "UNITED STATES OF AMERICA")) + 
  facet_wrap(vars(year)) +
  labs(y = "", x = "") +
  theme_bw() +
  labs(fill = "Region") +
  ggtitle("% of Measles Cases Imported to US by WHO Region and Year, 2020 - 2024, N = 93")

ggsave(here("projects/R21/outputs/measles_yearly_import_per.png"), width = 15, height = 6, dpi = 300)


#adding flextable code for reproducibility
library(flextable)
library(sirfunctions)
library(dplyr)
library(tidyr)

raw.data <- get_all_polio_data()
pos22.24 <- raw.data$pos |>
  filter(yronset %in% c("2022", "2023", "2024"))

summary.22.24 <- pos22.24 |>
  filter(measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3", "WILD 1") & source %in% c("AFP", "ENV")) |>
  group_by(measurement, yronset, source) |>
  mutate(serotype_count = n()) |>
  ungroup() |>
  select(measurement, source, yronset, serotype_count) |>
  unique() |>
  pivot_wider(id_cols = measurement, names_from = c("source", "yronset"), values_from = serotype_count) |>
  arrange(measurement) |>
  mutate(AFP_2023 = ifelse(is.na(AFP_2023), 0, AFP_2023),
         ENV_2023 = ifelse(is.na(ENV_2023), 0, ENV_2023), 
         ENV_2024 = ifelse(is.na(ENV_2024), 0, ENV_2024), 
         measurement = case_when(measurement == "WILD 1" ~ "WPV1", 
                                 measurement == "cVDPV 1" ~ "cVDPV1", 
                                 measurement == "cVDPV 2" ~ "cVDPV2", 
                                 measurement == "cVDPV 3" ~ "cVDPV3")) |>
  select(measurement, AFP_2022, ENV_2022, AFP_2023, ENV_2023, AFP_2024, ENV_2024) 

table <- flextable(summary.22.24) 
table <- set_header_labels(table, measurement = NA, AFP_2022 = "Cases", ENV_2022 = "ES", AFP_2023 = "Cases", ENV_2023 = "ES", AFP_2024 = "Cases", ENV_2024 = "ES")
table <- add_header_row(table, values = c("", "2022", "2023", "2024"), colwidths = c(1, 2, 2, 2))
table <- align(table, part = "all", align = "center")
print(table)

