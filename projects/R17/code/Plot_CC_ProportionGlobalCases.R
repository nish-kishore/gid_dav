## R17 - Horizontal bar graphs showing % of vpd cases in GID priority countries (compared to all countries)
# Updated 2025-03-20
# Author: Lori Niehaus / GID Data Analytics Task Team (DATT)

## R SETUP ################################################################
#
rm(list=ls()) # clear

## Install dependencies
required.packages <- c("tidyverse", "AzureStor","Microsoft365R", "readxl", "janitor","scales", "devtools")
packages.to.install <- setdiff(required.packages, installed.packages())
if(length(packages.to.install) >0) install.packages(packages.to.install)

# GID PEB SIR Team functions - use to read data from ADLS
# devtools::install_github("nish-kishore/sirfunctions")
# https://github.com/nish-kishore/sirfunctions

# load libraries
lapply(c("tidyverse", "readxl", "AzureStor", "Microsoft365R", "janitor", "scales", "sirfunctions"), library, character.only = TRUE)

# Data & Analytics Task Team ID
datt_task_id <- "R17"
sub_folder <- "3. Figures"

## IMPORT CLEAN DATA ################
# sirfunctions::edav_io(io = "list", default_dir = "GID/GIDMEA/giddatt/data_clean")  # view list of clean data files in ADLS

data_fi <- "data_clean/country_vpd_year_casedata.rds"
data_clean <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", #country-vpd-year-variable long dataframe
                              file_loc = data_fi)
data <- data_clean
## Review dataset structure
#str(data)
#unique(data$vpd)
#unique(data$variable)


## SET UP FOR FIGURES ##############

### Set colors for fill variables ##########################################################################
color_pal_9 <- c("#afabab","#A76DAB","#D76B6E","#334a54","#582d90", # Other Country, 8 GID CCs (individual)
               "#007B7A","#00a266","#D5006D","#a51d42")

color_pal_2 <- c("#afabab","#007B7A") # Other Country, GID CC (combined)

# set year range for data inclusion
plot_years <- c(2019,2020,2021,2022,2023,2024)
year_range <- paste0(min(plot_years),"-",max(plot_years))

### data set modifications

# Add columns to distinguish country groups for plotting: cc_cat and cc_cat_bin
data <- data %>% mutate(cc_cat = case_when(
                           gid_critical_countries=="Yes" ~ country_abbrev,
                           T ~ "Other Country"),
                        cc_cat_bin = case_when(
                          gid_critical_countries=="Yes" ~ "GID Critical Country",
                          T ~ "Other Country")
                        )

# convert country cats to factor vars to set order for plotting
data$cc_cat = factor(data$cc_cat, levels = rev(c("AFG", "BRA", "DRC", "ETH","IDN", "NGA","PAK", 'PHL',"Other Country")))
names(color_pal_9) <- levels(data$cc_cat) # use color_pal_9[data$cc_cat] in figs 1 & 2

data$cc_cat_bin = factor(data$cc_cat_bin, levels = rev(c("GID Critical Country", "Other Country")))
names(color_pal_2) <- levels(data$cc_cat_bin) # use color_pal_2[data$cc_cat] in fig 3

### Calculate global total ##########################################################################
# Calculate global total value for each variable-vpd-year (total across all geos)
global_totals <- data %>%
  group_by(year, vpd, variable) %>%
  summarise(global_total_value = sum(value, na.rm = TRUE))

# Calculate proportion of global total value for each variable-vpd-year in each country
data <- data %>% left_join(global_totals, by=c("year","vpd","variable")) %>% # add global totals as col
  mutate(proportion_of_global = value / global_total_value * 100)

## Minor vpd label changes to data per request
# change label for tetanus
data$vpd_short_name[data$vpd_short_name== "Tetanus (neonatal and/or non-neonatal)"] <- "Tetanus (all)" # value too long
data$vpd_short_name[data$vpd_short_name== "CRS"] <- "CRS*" # value too long

### Group data - A ##########################################################################
## collapse data sets for figures 2 & 3 to combine VALUE across years (plot_year period)

fig_data <- data %>% filter(year %in% plot_years) %>% 
  group_by(variable, vpd_short_name, country_abbrev, cc_cat, cc_cat_bin) %>% 
  summarize(cum_period_value = sum(value, na.rm = TRUE))

# Calculate global total value for variable-vpd within plot_years period
global_totals <- fig_data %>%
  group_by(variable, vpd_short_name) %>%
  summarise(global_total_value = sum(cum_period_value, na.rm = TRUE))

# Calculate proportion of global total value for each variable-vpd-year in each country
fig_data <- fig_data %>% left_join(global_totals, by=c("vpd_short_name","variable")) %>% # add global totals as col
  mutate(proportion_of_global = cum_period_value / global_total_value * 100)

### Group data - B ##########################################################################
### collapse dataset by geographic categories  - for figure 3 to cc_bin

fig3_data <- fig_data %>% 
  group_by(variable, vpd_short_name, cc_cat_bin) %>% 
  summarize(cum_period_value = sum(cum_period_value))

# Calculate global total value for variable-vpd within plot_years period
global_totals <- fig3_data %>%
  group_by(variable, vpd_short_name) %>%
  summarise(global_total_value = sum(cum_period_value, na.rm = TRUE))

# Calculate proportion of global total value for each variable-vpd-year in each country cat
fig3_data <- fig3_data %>% left_join(global_totals, by=c("vpd_short_name","variable")) %>% # add global totals as col
  mutate(proportion_of_global = cum_period_value / global_total_value * 100)

## add text labels to figure 3
fig3_data <- fig3_data %>%
  mutate(label_y = ifelse(cc_cat_bin == "GID Critical Country", proportion_of_global, 99), # Set y position conditionally
         hjust_value = ifelse(cc_cat_bin == "GID Critical Country", -0.25, 1.5)) # Adjust hjust conditionally


### Group data - C ##########################################################################
### collapse dataset by geographic categories  - for figure 2 to cc
fig2_data <- fig_data %>% 
  group_by(variable, vpd_short_name, cc_cat) %>% 
  summarize(cum_period_value = sum(cum_period_value))

# Calculate global total value for variable-vpd within plot_years period
global_totals <- fig2_data %>%
  group_by(variable, vpd_short_name) %>%
  summarise(global_total_value = sum(cum_period_value, na.rm = TRUE))

# Calculate proportion of global total value for each variable-vpd-year in each country cat
fig2_data <- fig2_data %>% left_join(global_totals, by=c("vpd_short_name","variable")) %>% # add global totals as col
  mutate(proportion_of_global = cum_period_value / global_total_value * 100)


### Update vpd_short_name as factor ############################################

# create vector for vpd as factor variable to organization x-axis from highest value to lowest value for cc proportion
ordered_vpd_levels <- fig3_data %>%
  filter(cc_cat_bin == "GID Critical Country") %>%
  arrange(desc(proportion_of_global)) %>% # descending order by vpd value
  pull(vpd_short_name) %>%
  unique()

# Reverse the order of levels so that they appear in ascending order on the plot
ordered_vpd_levels <- rev(ordered_vpd_levels)

ordered_vpd_levels <- factor(ordered_vpd_levels, levels = ordered_vpd_levels)

# Update datasets to use vpd_short_name as a factor with specified levels
data_all <- data %>% mutate(vpd_short_name = factor(vpd_short_name, levels = ordered_vpd_levels))
fig_data <- fig_data %>% mutate(vpd_short_name = factor(vpd_short_name, levels = ordered_vpd_levels))
fig2_data <- fig2_data %>% mutate(vpd_short_name = factor(vpd_short_name, levels = ordered_vpd_levels))
fig3_data <- fig3_data %>% mutate(vpd_short_name = factor(vpd_short_name, levels = ordered_vpd_levels))

#levels(fig_data$vpd_short_name) # confirm


## PLOT AND SAVE FIGURES ############################################################################
# use vpd_short_name as x-var, proportion_of_global as y-var, and fill var based on desired figure
# save figure using DATT naming conventions: dattid_fig#_type_year(s)_yvar_by_xvar1_xvar2_otherinfo.pn

### Fig 1: VPD case counts ##############################################################################
data <- data_all # years separated by facet
plot_variable <- "cases"
type <- "propbar" # horizontal bars
# facet by year

fig1 <- data %>% filter(variable==plot_variable, year %in% plot_years) %>% 
  ggplot(aes(x = vpd_short_name, y = proportion_of_global, fill = cc_cat)) +
  geom_bar(stat="identity") +
  coord_flip() + # Flip coordinates for horizontal bars
  ggtitle("Proportion of Total Global VPD Cases in each Geography, by year", subtitle=paste0("GID Priority Countries (",year_range,")*")) +
  xlab("VPD") +
  ylab("Proportion of Cases (%)") +
  scale_y_continuous(labels = percent_format(scale = 1)) + # Scale set to avoid multiplying by hundred
  labs(caption="*Preliminary 2024 case data is only included for measles, polio, Mpox, and COVID-19.\n 
       Note: VPD case-based surveillance data not available in NGA for typhoid, pertussis, meningitis, JE, & CRS; 
       in DRC for meningitis and JE; in IDN for typhoid; in PHL for YF, & CRS; in BRA for JE") +
  scale_fill_manual(values=color_pal_9)+
  guides(fill=guide_legend(title="Geography")) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=20),
        plot.caption = element_text(hjust = 1),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))+
 facet_wrap(~year)

fig1
fig1_name <- paste(datt_task_id,"fig1",type,year_range, plot_variable, "by", "year","country","vpd.png", sep="_")
ggsave(filename=paste0(datt_task_id,"/outputs/",fig1_name), fig1, width = 12, height = 8, dpi = 300)

temp_path1 <- file.path(tempdir(), fig1_name)
ggsave(filename=temp_path1, width = 12, height = 8, dpi = 300)

### Fig 2A: VPD cumulative case counts over 6-year period, each CC separate as proportion of global ##################
data <- fig2_data
plot_variable <- "cases"
type <- "propbar" # horizontal bars
exclude_vpds <- c("Mumps")
full_cntry_names <-c("Other Country", "Philippines","Pakistan","Nigeria","Indonesia","Ethiopia","Democratic Republic\nof the Congo", "Brazil", "Afghanistan")
# NO FACET

desired_legend_order <- c("Afghanistan", "Brazil", "Democratic Republic\nof the Congo",
                          "Ethiopia", "Indonesia", "Nigeria",
                          "Pakistan", "Philippines", "Other Country")

fig2A <- data %>% filter(variable==plot_variable, !vpd_short_name %in% exclude_vpds) %>% 
  ggplot(aes(x = vpd_short_name, y = proportion_of_global, fill = cc_cat)) +
  geom_bar(stat="identity") +
  coord_flip() + # Flip coordinates for horizontal bars
  ggtitle("Proportion of Total Global Cases\nin GID Priority Countries") +
  xlab("") +
  ylab(paste0("% of Reported Cases (",year_range,")")) +
  scale_y_continuous(labels = percent_format(scale = 1)) + # Scale set to avoid multiplying by hundred
  labs(caption="
  *CRS = Congenital Rubella Syndrome. Vaccine-preventable disease case estimates are based on available\ncountry-reported surveillance data reported and may underestimate true burden. Data is not available for all\ncountries and VPDs in period. Mpox and COVID-19 include all reported cases during global pandemics.") +
  scale_fill_manual(values=color_pal_9, labels=full_cntry_names)+
  guides(fill=guide_legend(title="", reverse=TRUE)) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=20),
        plot.caption = element_text(hjust=0.5,size=8),
        plot.subtitle =element_text(hjust=0.5, size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(vjust=0.5,size=14),
        legend.text=element_text(size=8),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key.size = unit(1.1, 'line'),
        legend.box="horizontal",
        legend.position = "top")

fig2A
fig2A_name <- paste(datt_task_id,"fig2A",type,year_range, plot_variable, "by","country","vpd.png", sep="_")
ggsave(filename=paste0(datt_task_id,"/outputs/",fig2A_name,".png"), fig2A, width = 10, height = 8, dpi = 300)

temp_path2A <- file.path(tempdir(), fig2A_name)
ggsave(filename=temp_path2A, width = 12, height = 8, dpi = 300)


### Fig 2B: Subset of VPDs - VPD cumulative case counts over 6-year period, each CC separate as proportion of global ##################

## Alternate version including only VPDs where >=50% of cases in GID Priority Countries
temp <- fig3_data %>% mutate(highpropvpd = case_when(
  (cc_cat_bin=="GID Critical Country" & proportion_of_global>=49.5) ~ "Yes",
  TRUE ~ "No")
) %>% filter(highpropvpd=="Yes")


data <- fig2_data
plot_variable <- "cases"
type <- "propbar" # horizontal bars
exclude_vpds <- c("Mumps", setdiff(unique(fig2_data$vpd_short_name),temp$vpd_short_name))
full_cntry_names <-c("Other Country", "Philippines","Pakistan","Nigeria","Indonesia","Ethiopia","Democratic Republic\nof the Congo", "Brazil", "Afghanistan")
# NO FACET


fig2B <- data %>% filter(variable==plot_variable, !vpd_short_name %in% exclude_vpds) %>% 
  ggplot(aes(x = vpd_short_name, y = proportion_of_global, fill = cc_cat)) +
  geom_bar(stat="identity") +
  coord_flip() + # Flip coordinates for horizontal bars
  ggtitle("Proportion of Total Global Cases\nin GID Priority Countries") +
  xlab("") +
  ylab(paste0("% of Reported Cases (",year_range,")")) +
  scale_y_continuous(labels = percent_format(scale = 1)) + # Scale set to avoid multiplying by hundred
  labs(caption="
  *CRS = Congenital Rubella Syndrome. Vaccine-preventable disease case estimates are based on available\nsurveillance data reported by countries and may underestimate true burden. Reported case data\nis not available for all countries and VPDs in time period.") +
  scale_fill_manual(values=color_pal_9, labels=full_cntry_names)+
  guides(fill=guide_legend(title="Geography")) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=20),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(vjust=0.5,size=14))

fig2B
fig2B_name <- paste(datt_task_id,"fig2B",type,year_range, plot_variable, "by","country","vpd.png", sep="_")
ggsave(filename=paste0(datt_task_id,"/outputs/",fig2B_name,".png"), fig2B, width = 10, height = 8, dpi = 300)

temp_path2B <- file.path(tempdir(), fig2B_name)
ggsave(filename=temp_path2B, width = 12, height = 8, dpi = 300)

### Fig 3A: VPD cumulative case counts over 6-year period, all CCs combined together as proportion of global ####
data <- fig3_data
plot_variable <- "cases"
type <- "propbar" # horizontal bars
# NO FACET

fig3A <- data %>% filter(variable==plot_variable) %>% 
  ggplot(aes(x = vpd_short_name, y = proportion_of_global, fill = cc_cat_bin)) +
  geom_bar(stat="identity") +
  geom_col() +
  geom_text(aes(label = ifelse(cc_cat_bin == "GID Critical Country", paste0(round(proportion_of_global), "%"), ""),
                y = label_y,
                hjust = hjust_value),
            vjust = .25,
            colour = "white") + # Only show text for GID Critical Countries
  coord_flip() + # Flip coordinates for horizontal bars
  ggtitle("Proportion of Total Global VPD Cases\nin GID Priority Countries", 
          subtitle=paste0("Reported cumulative cases between ", year_range, "*")) +
  xlab("Vaccine-Preventable Disease") +
  ylab("Proportion of Cases (%)") +
  scale_y_continuous(labels = percent_format(scale = 1)) + # Scale set to avoid multiplying by hundred
  labs(caption="*Preliminary 2024 case data is only included for measles, polio, Mpox, and COVID-19.\n 
       Note: VPD case estimates are based on available surveillance data reported by countries and may underestimate\ntrue burden in some countries. Reported case data is not available in NGA for typhoid, pertussis, meningitis, JE, & CRS; 
       in DRC for meningitis and JE; in IDN for typhoid; in PHL for YF, & CRS; in BRA for JE") +
  scale_fill_manual(values=color_pal_2)+
  guides(fill=guide_legend(title="Geography")) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=20), # hjust centers title
        plot.caption = element_text(hjust = 0),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))

fig3A
fig3A_name <- paste(datt_task_id,"fig3A",type,year_range, plot_variable, "by","country","vpd.png", sep="_")
ggsave(filename=paste0(datt_task_id,"/outputs/",fig3A_name,".png"), fig3A, width = 10, height = 8, dpi = 300)

temp_path3A <- file.path(tempdir(), fig3A_name)
ggsave(filename=temp_path3A, width = 12, height = 8, dpi = 300)


### Fig 3B: Subset of VPDs - VPD cumulative case counts over 6-year period, all CCs combined together as proportion of global ####
data <- fig3_data
plot_variable <- "cases"
exclude_vpds <- c("Mumps", setdiff(unique(fig2_data$vpd_short_name),temp$vpd_short_name))
type <- "propbar" # horizontal bars
# NO FACET

fig3B <- data %>% filter(variable==plot_variable, !(vpd_short_name %in% exclude_vpds)) %>% 
  ggplot(aes(x = vpd_short_name, y = proportion_of_global, fill = cc_cat_bin)) +
  geom_bar(stat="identity") +
  geom_col() +
  geom_text(aes(label = ifelse(cc_cat_bin == "GID Critical Country", paste0(round(proportion_of_global), "%"), ""),
                y = label_y,
                hjust = hjust_value),
            vjust = .25,
            colour = "white") + # Only show text for GID Critical Countries
  coord_flip() + # Flip coordinates for horizontal bars
  ggtitle("Proportion of Total Global VPD Cases\nin GID Priority Countries", 
          subtitle=paste0("Reported cumulative cases between ", year_range, "*")) +
  xlab("Vaccine-Preventable Disease") +
  ylab("Proportion of Cases (%)") +
  scale_y_continuous(labels = percent_format(scale = 1)) + # Scale set to avoid multiplying by hundred
  labs(caption="*Preliminary 2024 case data is only included for measles, polio, Mpox, and COVID-19.\n 
       Note: VPD case estimates are based on available surveillance data reported by countries and may underestimate\ntrue burden in some countries. Reported case data is not available in NGA for typhoid, pertussis, meningitis, JE, & CRS; 
       in DRC for meningitis and JE; in IDN for typhoid; in PHL for YF, & CRS; in BRA for JE") +
  scale_fill_manual(values=color_pal_2)+
  guides(fill=guide_legend(title="Geography")) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=20), # hjust centers title
        plot.caption = element_text(hjust = 0),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))

fig3B
fig3B_name <- paste(datt_task_id,"fig3B",type,year_range, plot_variable, "by","country","vpd.png", sep="_")
ggsave(filename=paste0(datt_task_id,"/outputs/",fig3B_name,".png"), fig3B, width = 10, height = 8, dpi = 300)

temp_path3B <- file.path(tempdir(), fig3B_name)
ggsave(filename=temp_path3B, width = 12, height = 8, dpi = 300)


### Fig 4: VPD case counts for ONE VPD (e.g., measles) #######################
data <- data_all
plot_variable <- "cases"
type <- "propbar" # horizontal bars
# facet by year

fig4 <- data %>% filter(variable==plot_variable, year %in% plot_years)  %>%
  ggplot(aes(x = year, y = proportion_of_global, fill = cc_cat)) +
  geom_bar(stat="identity") +
  coord_flip() + # Flip coordinates for horizontal bars
  ggtitle("Proportion of Total Global VPD Cases in each Geography, by year", subtitle=paste0("GID Priority Countries (",year_range,")*")) +
  xlab("Year") +
  ylab("Proportion of Cases (%)") +
  scale_y_continuous(labels = percent_format(scale = 1)) + # Scale set to avoid multiplying by hundred
  labs(caption="*Preliminary 2024 case data is only included for measles, polio, Mpox, and COVID-19.\n 
       Note: VPD case-based surveillance data not available in NGA for typhoid, pertussis, meningitis, JE, & CRS; 
       in DRC for meningitis and JE; in IDN for typhoid; in PHL for YF, & CRS; in BRA for JE") +
  scale_fill_manual(values=color_pal_9)+
  guides(fill=guide_legend(title="Geography")) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=20),
        plot.caption = element_text(hjust = 1),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))+
  facet_wrap(~vpd_short_name)

fig4
fig4_name <- paste(datt_task_id,"fig4",type,year_range, plot_variable, "by", "year","country","vpd.png", sep="_")
ggsave(filename=paste0(datt_task_id,"/outputs/",fig4_name,".png"), fig1, width = 12, height = 8, dpi = 300)

temp_path4 <- file.path(tempdir(), fig4_name)
ggsave(filename=temp_path4, width = 12, height = 8, dpi = 300)



## Write figs to Teams#########################################################
### Access to Task Team Teams Channel ("DATT")

figs_upload <- c(fig1_name,fig2A_name,fig2B_name,fig3A_name,fig3B_name, fig4_name)  
temp_upload <- c(temp_path1,temp_path2A,temp_path2B,temp_path3A,temp_path3B,temp_path4)
y <- length(figs_upload)

for (n in 1:y){
  temp_path <- paste0(temp_upload[n])
  fig_name <- paste0(figs_upload[n])
  
  sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")
  
  
  upload_to_sharepoint(
    file_to_upload = temp_path,  
    sharepoint_file_loc = sp_path,
    site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
    drive = "Documents")
  
}
