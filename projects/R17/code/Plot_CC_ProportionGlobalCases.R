## Horizontal bar graph showing % of vpd cases in GID priority countries (compared to all countries)
# Updated 2025-02-24
# Author: Lori Niehaus / GID Data Analytics Task Team (DATT)


#######################################################################
#######################################################################
## R SETUP
#######################################################################
rm(list=ls()) # clear

## Install dependencies
required.packages <- c("tidyverse", "AzureStor","Microsoft365R", "readxl", "janitor","scales", "devtools")
packages.to.install <- setdiff(required.packages, installed.packages())
if(length(packages.to.install) >0) install.packages(packages.to.install)

# GID PEB SIR Team functions - use to read data from ADLS
# devtools::install_github("nish-kishore/sirfunctions")
# https://github.com/nish-kishore/sirfunctions

library(tidyverse)
library(readxl)
library(AzureStor)
library(Microsoft365R)
library(janitor)
library(scales)
library(sirfunctions)

# getwd()

datt_task_id <- "R17"

#######################################################################
############################################################################
## PLOT FIGURES
############################################################################

# Data set to use for plotting
data_fi <- "data_clean/country_vpd_year_casedata.rds"

# read clean data from ADLS
# sirfunctions::edav_io(io = "list", default_dir = "GID/GIDMEA/giddatt/data_clean")

data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                              file_loc = data_fi)
## Observe data
#head(data,3)
#names(data)
#unique(data$vpd)
#unique(data$variable)

## Assign parameters for figures
# Filters to apply to figures
plot_countries <- unique(data$country_abbrev[data$gid_critical_countries == "Yes"]) # 8 GID Critical Countries
plot_years <- c(2018,2019,2020,2021,2022,2023, 2024) #2024 ony for measles and polio
year_range <- paste0(min(plot_years),"-",max(plot_years))
plot_vpds_priority <- c("Measles","Rubella","Congenital rubella syndrome (CRS)", "Poliomyelitis","Yellow fever",
                        "Invasive meningococcal disease","Cholera","Ebola virus disease","Mpox")

## COLOR PALETTE INFO
show_col(pal_viridis()(9)) # colors for priority countries + all other countries

# change x-var, y-var, and fill var based on desired figure
# save figure using DATT naming conventions: fig#_type_year(s)_yvar_by_xvar1_xvar2_otherinfo.png 

##############################################################################
## Fig 1: VPD case counts
plot_variable <- "cases"
type <- "stackedbar"
# facet by vpd
#############################################################################
# Calculate total cases globally and proportion of 'vpd' cases
total_cases <- data %>%
  filter(year %in% plot_years & variable == plot_variable) %>%
  group_by(year, vpd) %>%
  summarise(total_value = sum(value, na.rm = TRUE))

vpd_cases <- data %>%
  filter(year %in% plot_years & vpd %in% plot_vpds_priority & variable == plot_variable) %>%
  mutate(country_group = ifelse(country_abbrev %in% plot_countries, 
                                country_abbrev, 
                                "Other Countries")) %>% # Categorize countries
  group_by(year, vpd, country_group) %>%
  summarise(vpd_value = sum(value, na.rm = TRUE), .groups = 'drop') 

# Join to get proportions
proportions <- vpd_cases %>%
  left_join(total_cases, by = c("year","vpd")) %>%
  mutate(proportion = vpd_value / total_value * 100)

# Create horizontal bar graph
fig1 <- ggplot(proportions, aes(x = reorder(country_group, -proportion), y = proportion, fill = country_group)) +
  geom_bar(stat="identity") +
  coord_flip() + # Flip coordinates for horizontal bars
  ggtitle("Proportion of VPD Cases Out of Total Cases Globally by VPD", subtitle="GID Critical Countries (2018-2023)") +
  xlab("Country/Group") +
  ylab("Proportion of VPD Cases (%)") +
  labs(caption="Note: CRS surveillance data is limited due to incomplete country reporting.") +
  guides(fill=guide_legend(title="Country/Group")) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18)) +
  facet_wrap(~vpd) # Facet by VPD

print(fig1)

fig1
fig_name <- paste("fig1",type,year_range, plot_variable, "by", "year","country","vpd", sep="_")

ggsave(filename=paste0(datt_task_id,"/outputs/",fig_name,".png"), fig1)

#########################################################################
## Write cleaned figs to Teams folder (once finals decided)
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

## Save figure 4 to Teams (update if want to save others)
filename = paste0(datt_task_id,"/outputs/",fig_name,".png")
teams_fig_folder$upload(paste0(filename))

