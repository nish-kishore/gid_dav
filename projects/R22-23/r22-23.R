library(sf)
library(tidyverse)
library(sirfunctions)
library(readxl)
library(here)

# data <- st_read("C:/Users/ynm2/OneDrive - CDC/QGIS Projects/resources/gpln_labs_georef.shp") |>
#   dplyr::select(-id)
#
# edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/gpln_labs.rds", obj = data)
#
# grmln <- read_excel("C:/Users/ynm2/Downloads/Global Network Countries-VVPDB.xlsx", skip = 1)
#
# edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/gmrln_labs.rds", obj = grmln)

data <- edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/gpln_labs.rds", obj = data)

data |>
  as_tibble() |>
  group_by(NAME, type) |>
  summarise(n_labs = n()) |>
  ungroup() |>
  pivot_wider(names_from = "type", values_from = "n_labs", values_fill = 0) |>
  rowwise() |>
  mutate("Total Num. GPLN Labs" = sum(NSL, RRL, GSL)) |>
  ungroup() |>
  rename("Country" = NAME, "Global Specialized Lab" = GSL, "Regional Reference Lab" = RRL, "National / Subnational Lab" = NSL) |>
  write_csv(here("projects/R22-23/gpln_labs_ctry.csv"))
