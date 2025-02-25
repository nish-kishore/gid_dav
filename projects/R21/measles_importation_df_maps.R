#dependencies
library(tidyverse)
library(tidyr)
library(readxl)
library(sirfunctions)
library(ggplot2)

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
region.imports.count <- measles.import.data.list$region.imports.count |>
  tidyr::pivot_longer("Total":"2024") |>
  dplyr::rename("year" = "name", 
                "case_count" = "value") |>
  dplyr::arrange(year)

ctry.24.shapes <- long.global.ctry |>
  dplyr::filter(active.year.01 == 2024)

region.counts.shape <- dplyr::left_join(ctry.24.shapes, region.imports.count, by = c("WHO_REGION" = "Source_WHO_region"))
  
#total importations
ggplot(region.counts.shape |> dplyr::filter(year == "Total")) +
  geom_sf(aes(fill = factor(case_count))) +
  theme_bw() +
  scale_fill_manual(values = c("2" = "green", "11" = "yellow", "24" = "orange", "48" = "red"))


#total percentages of imported measles cases
region.imports.per <- measles.import.data.list$region.import.percentage |>
  tidyr::pivot_longer("Total":"2024") |>
  dplyr::rename("year" = "name", 
                "percentage" = "value") |>
  dplyr::arrange(year)

region.percents.shape <- dplyr::left_join(ctry.24.shapes, region.imports.per, by = c("WHO_REGION" = "Source_WHO_region"))  

#percentage of importations
ggplot(region.percents.shape |> dplyr::filter(year == "Total")) +
  geom_sf(aes(fill = factor(percentage))) +
  theme_bw() +
  scale_fill_manual(values = c("0.02" = "green", "0.12" = "yellow", "0.26" = "orange", "0.52" = "red"))
