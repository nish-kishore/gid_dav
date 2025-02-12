## Quick plotting of figures for cc one-pagers
# Updated 2025-02-12
# Author: Lori Niehaus / GID Data Analytics Task Team (DATT)

# NEED TO UPDATE COLOR SCHEMES, FONTS, ETC. (PENDING INPUT FROM GID STAKEHOLDERS)

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

datt_task_id <- "R12"

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
plot_years <- c(2018,2019,2020,2021,2022,2023)
year_range <- paste0(min(plot_years),"-",max(plot_years))
plot_vpds_priority <- c("Measles","Rubella","Congenital rubella syndrome (CRS)", "Poliomyelitis")
plot_vpds_ia2030outbreak <- c("Measles","WPV","cVDPV","Yellow fever","Invasive meningococcal disease",
                              "Cholera","Ebola virus disease")

# plot_variable <- "num_cases_WHOofficial"
# plot_variable <- "lod_count"

## COLOR PALETTE INFO
# show_col(pal_viridis()(8)) # countries for priority countries

# change x-var, y-var, and fill var based on desired figure
# save figure using DATT naming conventions: fig#_type_year(s)_yvar_by_xvar1_xvar2_otherinfo.png 

##############################################################################
## Fig 1: VPD case counts
plot_variable <- "num_cases_WHOofficial"
type <- "stackedbar"
# facet by vpd
#############################################################################
fig1 <- data %>% 
  filter(country_abbrev %in% plot_countries & year %in% plot_years & vpd %in% plot_vpds_priority & variable==plot_variable) %>% 
  ggplot(aes(x=year, y=value, fill=country_abbrev))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  #geom_text(aes(label = paste(format(round(Projection_Grand_Total/1e6, 1), trim=TRUE),"M"))) +
  #theme_void()+
  ggtitle("Cumulative VPD Case Counts",subtitle="GID Critical Countries (2018-2023)")+
  #ggtitle("Cumulative VPD Case Counts, 2018-2023",subtitle=paste("Total (USD$): ",format(round(GID_total/1e6, 1), trim=TRUE),"M"))+
  #xlab("GID Critical Countries")+
  xlab("Year")+
  ylab("Number of VPD cases (Value in thousands)")+
  labs(caption="Note: CRS surveillance data is limited due to incomplete country reporting.")+
  #scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))+
  #scale_y_continuous(labels=unit_format(unit="M", scale=1e-6))+
  #scale_x_discrete(labels =c(""))+
  guides(fill=guide_legend(title="Country"))+
  # scale_fill_manual(values=PAPcolors)+
  #guides(fill="none")+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))+
  facet_grid(rows=vars(vpd_short_name), scales = "free_y")
#facet_grid(.~vpd)

fig1
fig_name <- paste("fig1",type,year_range, plot_variable, "by", "year","country","vpd", sep="_")

ggsave(filename=paste0(datt_task_id,"/outputs/",fig_name,".png"), fig1)

############################################################################
## Fig 2: LoD VPD outbreaks
plot_variable <- "lod_count"
type <- "stackedbar"
# facet by country
#############################################################################
fig2 <- data %>% 
  filter(country_abbrev %in% plot_countries & year %in% plot_years & vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
  ggplot(aes(x=year, y=value, fill=vpd))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  #geom_text(aes(label = paste(format(round(Projection_Grand_Total/1e6, 1), trim=TRUE),"M"))) +
  #theme_void()+
  ggtitle("Large or Disruptive VPD Outbreaks",subtitle="GID Critical Countries (2018-2023)")+
  #ggtitle("Cumulative VPD Case Counts, 2018-2023",subtitle=paste("Total (USD$): ",format(round(GID_total/1e6, 1), trim=TRUE),"M"))+
  #xlab("GID Critical Countries")+
  xlab("Year")+
  ylab("Number of large or disruptive outbreaks")+
  labs(caption="Note: Definition of 'large or disruptive' varies by VPD.")+
  # scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))+
  #scale_y_continuous(labels=unit_format(unit="M", scale=1e-6))+
  scale_y_continuous(labels = number_format(accuracy = 1)) + 
  #scale_x_discrete(labels =c(""))+
  guides(fill=guide_legend(title="VPD"))+
  # scale_fill_manual(values=PAPcolors)+
  #guides(fill="none")+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))+
  #facet_grid(rows=vars(country_name_lower), scales = "free_y")
  facet_grid(rows=vars(country_abbrev))
#facet_grid(.~vpd)
fig2

fig_name <- paste("fig2",type,year_range, plot_variable, "by", "year","country","vpd", sep="_")

ggsave(filename=paste0(datt_task_id,"/outputs/",fig_name,".png"), fig2)

##############################################################################
## Fig 3: VPD LoD outbreaks
plot_variable <- "lod_count"
type <- "stackedbar"
# no facet (don't disaggregate by country)
#############################################################################
fig3 <- data %>% 
  filter(country_abbrev %in% plot_countries & year %in% plot_years & vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
  ggplot(aes(x=year, y=value, fill=vpd))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  #geom_text(aes(label = paste(format(round(Projection_Grand_Total/1e6, 1), trim=TRUE),"M"))) +
  #theme_void()+
  ggtitle("Large or Disruptive VPD Outbreaks",subtitle="GID Critical Countries (2018-2023)")+
  #ggtitle("Cumulative VPD Case Counts, 2018-2023",subtitle=paste("Total (USD$): ",format(round(GID_total/1e6, 1), trim=TRUE),"M"))+
  #xlab("GID Critical Countries")+
  xlab("Year")+
  ylab("Number of large or disruptive outbreaks")+
  labs(caption="Note: Definition of 'large or disruptive' varies by VPD.")+
  # scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))+
  #scale_y_continuous(labels=unit_format(unit="M", scale=1e-6))+
  scale_y_continuous(labels = number_format(accuracy = 1)) + 
  #scale_x_discrete(labels =c(""))+
  guides(fill=guide_legend(title="VPD"))+
  # scale_fill_manual(values=PAPcolors)+
  #guides(fill="none")+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))

fig3

fig_name <- paste("fig3",type,year_range, plot_variable, "by", "year","vpd", sep="_")

ggsave(filename=paste0(datt_task_id,"/outputs/",fig_name,".png"), fig3)

##############################################################################
## Fig 4: VPD LoD outbreaks
plot_variable <- "lod_count_atleastone"
type <- "stackedbar"
# no facet (not years)
#############################################################################
## add var so counting 1 per country per year
## Count number of countries experiencing instead of LODs; Combine polio?
# data %>% group_by(country,)
# data %>% pivot_wider(id_cols="country_abbrev","year","vpd", ) %>% mutate()
# ia2030_lod_vpds <- vpds
# 
# cc_lodos <- lodos %>% filter(country_name %in% cc)
# cc_lodos$lod_count_atleastone <- 0
# cc_lodos$lod_count_atleastone[cc_lodos$lod_count >= 1] <- 1
# 
# lodos$lod_count_atleastone <- 0
# lodos$lod_count_atleastone[lodos$lod_count >= 1] <- 1
# 
# cc_lower <- tolower(cc)

fig4 <- data %>% 
  filter(country_abbrev %in% plot_countries & year %in% plot_years & vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
  ggplot(aes(x=country_abbrev, y=value, fill=vpd))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  #geom_text(aes(label = paste(format(round(Projection_Grand_Total/1e6, 1), trim=TRUE),"M"))) +
  #theme_void()+
  ggtitle("Large or Disruptive VPD Outbreaks",subtitle="GID Critical Countries (2018-2023)")+
  #ggtitle("Cumulative VPD Case Counts, 2018-2023",subtitle=paste("Total (USD$): ",format(round(GID_total/1e6, 1), trim=TRUE),"M"))+
  #xlab("GID Critical Countries")+
  xlab("Country")+
  ylab("# Years country experienced LoD outbreak\n  of each VPD between 2018-2023")+
  labs(caption="Note: Definition of 'large or disruptive' varies by VPD.")+
  # scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))+
  #scale_y_continuous(labels=unit_format(unit="M", scale=1e-6))+
  scale_y_continuous(labels = number_format(accuracy = 1)) + 
  #scale_x_discrete(labels =c(""))+
  guides(fill=guide_legend(title="VPD"))+
  # scale_fill_manual(values=PAPcolors)+
  #guides(fill="none")+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))

fig4

fig_name <- paste("fig4",type,year_range, plot_variable, "by", "country","vpd", sep="_")

ggsave(filename=paste0(datt_task_id,"/outputs/",fig_name,".png"), fig4)

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

