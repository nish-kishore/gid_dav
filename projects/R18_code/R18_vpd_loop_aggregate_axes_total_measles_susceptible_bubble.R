#### R18: Graph with total number axes and ZD Bubbles Script ####

#Author: Erin Goucher

#### Install and Load R Packages ####
required.packages <- c("tidyverse", "AzureStor", "Microsoft365R", "readxl", "janitor", "scales", 
                       "devtools", "dplyr", "stringr", "ggbeeswarm", "ggrepel", "scales")

packages.to.install <- setdiff(required.packages, installed.packages())
if (length(packages.to.install) > 0) install.packages(packages.to.install)

# Load libraries
library(tidyverse)
library(readxl)
library(AzureStor)
library(Microsoft365R)
library(janitor)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggbeeswarm)
library(scales)


#### Install and Load Custom Packages from GithUb ####
install.packages("devtools")
library(devtools)
devtools::install_github("nish-kishore/sirfunctions")


#### General Formatting ####
#### Define Priority Countries 
priority_country_names <- c("Afghanistan", "Pakistan", "Nigeria", "Ethiopia", 
                            "Democratic Republic of the Congo", "Indonesia", "Philippines", "Brazil")

#### Define Color Palette 
# Define the exact color mappings based on the legend image
color_pal_9 <- c(
  "Afghanistan" = "#A51D42",
  "Brazil" = "#D5006D",
  "Democratic Republic of the Congo" = "#007B7A",
  "Ethiopia" = "#00A266",
  "Indonesia" = "#582D90",
  "Nigeria" = "#334A54",
  "Pakistan" = "#D76B6E",
  "Philippines" = "#A76DAB",
  "Other Country" = "#afabab"  # Ensure 'Other Country' is mapped too
)

#### Sort Priority Countries in alphabetical order for legend 
sorted_priority_names <- sort(names(color_pal_9)[names(color_pal_9) != "Other Country"])

#### VPD Dataset ####

#### Read in Data from ADLS #
dataset_vpd <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/country_vpd_year_casedata.rds")

#### Filter for Years 2019-2023 
dataset_vpd <- dataset_vpd %>% 
  filter(year >= 2019 & year <= 2023)

#### Filter for VPD cases only 
dataset_vpd <- dataset_vpd %>% 
  filter(variable == "cases")

#### Measles Susceptibles Dataset ####
data_sus <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/Measles immunity profile data_AsOfEndof2025.csv")

#### Traveler Dataset ####
data_trvlr <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/traveler_outbound.rds")

#### Summarize Travelers ####
traveler_summary <- data_trvlr %>%
  filter(year >= 2019 & year <= 2023) %>%
  group_by(ctry) %>%
  summarize(total_travelers_2019_2023 = sum(count, na.rm = TRUE), .groups = "drop")

#### Summarize Susceptible Children ####
sus_summary <- data_sus %>%
  filter(birthYear %in% 2020:2025) %>%
  group_by(`Country name`) %>%
  summarize(sus_u5 = sum(NumberNotProtectedByVaccine, na.rm = TRUE), .groups = "drop")

#### Summarize VPD Cases ####
vpd_summary <- dataset_vpd %>%
  group_by(country_name, vpd) %>%
  summarize(total_unique_vpd_cases_2019_2023 = sum(value, na.rm = TRUE), .groups = "drop")

#### Join Summarized Datasets ####
summary_dataset <- dataset_vpd %>%
  left_join(vpd_summary, by = c("country_name", "vpd")) %>%
  left_join(traveler_summary, by = c("country_name" = "ctry")) %>%
  left_join(sus_summary, by = c("country_name" = "Country name"))

#### Create Graph ####
# Get list of unique VPDs
unique_vpds <- unique(summary_dataset$vpd)

# Loop through each VPD
for (this_vpd in unique_vpds) {
  
  #Filter 2023 Zero-Dose Data
  vpd_data <- summary_dataset %>%
    filter(vpd == this_vpd, year == 2023) %>%
    select(country_name, sus_u5) %>%
    distinct()
  
  #Join with Aggregated 2019–2023 Totals
  vpd_summary <- summary_dataset %>%
    filter(vpd == this_vpd) %>%
    group_by(country_name) %>%
    summarize(
      total_travelers_2019_2023 = first(total_travelers_2019_2023),
      total_unique_vpd_cases_2019_2023 = first(total_unique_vpd_cases_2019_2023),
      sus_u5 = first(sus_u5),
      .groups = "drop"
    ) %>%
    mutate(
      country_category = ifelse(country_name %in% priority_country_names, country_name, "Other Country")
    ) %>%
    drop_na(total_travelers_2019_2023, total_unique_vpd_cases_2019_2023, sus_u5)
  
  
  
  # Filter out 0s and NAs
  vpd_summary <- vpd_summary %>%
    drop_na(total_travelers_2019_2023, total_unique_vpd_cases_2019_2023, sus_u5) %>%
    filter(
      total_travelers_2019_2023 > 0,
      total_unique_vpd_cases_2019_2023 > 0,
      sus_u5 > 0
    )
  
  # Skip if no usable data
  if (nrow(vpd_summary) == 0) next
  
  # Calculate dynamic breaks (use quantiles for spread)
  size_breaks <- vpd_summary$sus_u5 %>%
    quantile(probs = c(0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE) %>%
    round() %>%
    unique()
  
  # Filter out 0s (can’t use log scale with 0s)
  size_breaks <- size_breaks[size_breaks > 0]
  
  # Match visual bubble sizes to number of breaks
  legend_sizes <- scales::rescale(size_breaks, to = c(5, 20))
  
  #Axis & Size Scaling
  x_limits <- c(min(vpd_summary$total_travelers_2019_2023, na.rm = TRUE) * 0.9,
                max(vpd_summary$total_travelers_2019_2023, na.rm = TRUE) * 1.1)
  y_limits <- c(min(vpd_summary$total_unique_vpd_cases_2019_2023, na.rm = TRUE) * 0.9,
                max(vpd_summary$total_unique_vpd_cases_2019_2023, na.rm = TRUE) * 1.1)
  max_zd <- max(vpd_summary$sus_u5, na.rm = TRUE)
  
  #Titles and labels
  y_axis_title <- paste0("Total ", this_vpd, " VPD Cases (2019–2023)")
  title_text <- paste0("Plot of ", this_vpd, " VPD Cases vs. Travelers to the US by Country\n(Bubble Size = Children Susecptible to Measles <5 (2025)")
  
  #Create graph
  plot <- ggplot(vpd_summary, aes(x = total_travelers_2019_2023, y = total_unique_vpd_cases_2019_2023,
                                  color = country_category, size = sus_u5)) +
    
    # Plot Other Countries first
    geom_point(data = vpd_summary %>% filter(country_category == "Other Country"),
               aes(size = sus_u5), color = "#afabab", alpha = 0.4, show.legend = FALSE) +
    
    # Plot Priority Countries
    geom_point(data = vpd_summary %>% filter(country_category != "Other Country"),
               aes(color = country_category, size = sus_u5), alpha = 1) +
    
    scale_x_log10(
      labels = comma_format(),
      breaks = trans_breaks("log10", function(x) 10^x)
    ) +
    scale_y_log10(
      labels = comma_format(),
      breaks = trans_breaks("log10", function(x) 10^x)
    ) +
    scale_size_continuous(
      name = "Children Susceptible to Measles <5 (2025)",
      range = c(5, 20),
      breaks = size_breaks,
      labels = scales::comma_format()(size_breaks),
      guide = guide_legend(
        override.aes = list(size = legend_sizes)
      )
    ) +
    scale_color_manual(
      name = "Priority Countries",
      values = color_pal_9,
      breaks = sorted_priority_names,
      guide = guide_legend(order = 1, override.aes = list(size = 10))
    ) +
    coord_cartesian(xlim = x_limits, ylim = y_limits) +
    labs(
      title = title_text,
      x = "Total Travelers to the US (2019–2023)",
      y = y_axis_title
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 12),
      legend.position = "right",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Save each plot
  file_name <- paste0("R18_", tolower(gsub(" ", "_", this_vpd)), "_aggregate_susceptible.png")
  ggsave(
    filename = file.path("C:/Users/tqa4/OneDrive - CDC/Summer 2022/EPMPs/DATT Visuals/R18_aggregate_susceptible", file_name),
    plot = plot,
    width = 10, height = 6.5, dpi = 300, bg = "white"
  )
}





