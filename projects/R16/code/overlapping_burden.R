#dependencies
library(tidyverse)
library(readxl)
library(rnaturalearth)
library(here)

#read in and process data

priority.countries <- c("ETH", "NGA", "COD", "IDN", "BRA", "PHL", "AFG", "PAK")
polio.data <- sirfunctions::get_all_polio_data()



