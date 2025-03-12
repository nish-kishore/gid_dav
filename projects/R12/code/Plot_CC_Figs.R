## Bar plots of all VPDs in GID Critical Counties - Cumulative of cases and large or disruptive outbreaks
# Updated 2025-03-10

# Updates - 10 March 2025 
# See code updates, switched local save path to the temp directory for each
# Updated looping code to upload to sharepoint R12 folder 
# Missing for Figure 6 - if this loop produces multiple figures then it will need to be updated below as well 

# Author: Lori Niehaus / GID Data Analytics Task Team (DATT)

# change x-var, y-var, and fill var based on desired figure
# save figure using DATT naming conventions: fig#_type_year(s)_yvar_by_xvar1_xvar2_otherinfo.png

## R SETUP#######################################################################
rm(list=ls()) # clear

## Install dependencies
required.packages <- c("tidyverse", "AzureStor","Microsoft365R", "readxl", "janitor","scales", "devtools", "sf")
packages.to.install <- setdiff(required.packages, installed.packages())
if(length(packages.to.install) >0) install.packages(packages.to.install)

# GID PEB SIR Team functions - use to read data from ADLS
# devtools::install_github("nish-kishore/sirfunctions")
# https://github.com/nish-kishore/sirfunctions

# load libraries
lapply(c("tidyverse", "readxl", "AzureStor", "Microsoft365R", "janitor", "scales", "sirfunctions", "sf"), library, character.only = TRUE)

datt_task_id <- "R12"
sub_folder <- "3. Figures"

## IMPORT CLEAN DATA ###########################################

# sirfunctions::edav_io(io = "list", default_dir = "GID/GIDMEA/giddatt/data_clean")  # view list of clean data files in ADLS

data_fi <- "data_clean/country_vpd_year_casedata.rds"
data_clean <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", 
                              file_loc = data_fi)

data <- data_clean
## Review dataset structure
#str(data)
#unique(data$vpd)
#unique(data$variable)

## SET UP FOR FIGURES ##############

## Assign universal filters for figures
plot_countries <- unique(data$country_abbrev[data$gid_critical_countries == "Yes"]) # 8 GID Critical Countries
plot_years <- c(2018,2019,2020,2021,2022,2023)
year_range <- paste0(min(plot_years),"-",max(plot_years))

## Set up CC data so includes CC countries only in years of interest
cc_data <- data %>% filter(country_abbrev %in% plot_countries, year %in% plot_years)

## Set color palettes and factor variables for fill variables
color_pal_ccs <- rev(c("#A76DAB","#D76B6E","#334a54","#582d90", # 8 GID CCs (individual) - backwards
                 "#007B7A","#00a266","#D5006D","#a51d42"))

color_pal_vpds <- rev(c("#a51d42","#CC79A7","#FF9999","#009E73","#0072B2","#FF5733", # VPDs
                 "#C77CFF","#D55E00","#6A5ACD","#E69F00","#86e0b5","#A5C1BA","#8DA0CB","#A3A500","#007B7A","#041c3a","#66B3FF",
                 "#afabab")) # gray in case for other/na



# convert country abbrev to factor vars to set order for plotting
cc_data$country_abbrev = factor(cc_data$country_abbrev, levels = c("AFG", "BRA", "DRC", "ETH","IDN", "NGA","PAK", 'PHL'))
names(color_pal_ccs) <- levels(cc_data$country_abbrev)

# convert vpds to factor vars to set order for plotting
cc_data$vpd_short_name = factor(cc_data$vpd_short_name, 
                             levels = rev(c("Measles", "Rubella", "CRS", "Polio","Cholera", "Ebola","Mpox", 
                                            "Meningitis","Diphtheria","Yellow fever", "Neonatal tetanus", "Tetanus (neonatal and/or non-neonatal)",
                                            "Typhoid","COVID-19", "Jap encephalitis", "Pertussis","Mumps", "NA")))
names(color_pal_vpds) <- levels(cc_data$vpd_short_name)

## Calculate totals as 'allccs_value_total' to use in subtitles
# total across all 8 GID critical countries (for same variable and vpd)
cc_totals <- cc_data %>% group_by(variable, vpd) %>% summarize(allccs_value_total = sum(value, na.rm=TRUE))
cc_data <- cc_data %>% left_join(cc_totals, by=c('variable','vpd')) # join totals to df

### Basic Statistics ##############
## Proportion of all VPD lodos in 2023 (IA2030 VPDs only) that occurred in 8 GID CCs
plot_vpds_ia2030outbreak <- c("Measles","Poliomyelitis","Yellow fever","Invasive meningococcal disease",
                              "Cholera","Ebola virus disease")

total_lod_count_2023_all <- data_clean %>% filter(variable=="lod_count", year==2023, vpd %in% plot_vpds_ia2030outbreak) %>% # total across all countries
  summarize(n_global_lodos = sum(value, na.rm = TRUE)) %>% as.numeric()

total_lod_count_2023_ccs <- data_clean %>% filter(variable=="lod_count", gid_critical_countries=="Yes", year==2023, vpd %in% plot_vpds_ia2030outbreak) %>% # total across all countries
  summarize(n_global_lodos = sum(value, na.rm = TRUE)) %>% as.numeric()

prop_lodos_ccs_2023 <-total_lod_count_2023_ccs /  total_lod_count_2023_all 

# proportion of all countries (WHO Member States, N=194) that are GID priority countries
prop_countries_ccs <- 8/194

## PRODUCE & SAVE FIGURES ##############

### Fig 1: VPD case counts in CCs, facet VPD ################
plot_variable <- "cases"
type <- "stackedbar"
plot_vpds_priority <- c("Measles","Rubella","Congenital rubella syndrome (CRS)", "Poliomyelitis")
# facet by vpd

fig1 <- cc_data %>% 
  filter(vpd %in% plot_vpds_priority & variable==plot_variable) %>% 
  ggplot(aes(x=year, y=value, fill=country_abbrev))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  ggtitle("Cumulative VPD Case Counts",subtitle="GID Critical Countries (2018-2023)")+
  xlab("Year")+
  ylab("Number of VPD cases")+
  labs(caption="Note: CRS surveillance data is not available for Nigeria or the Philippines.")+
  scale_fill_manual(values=color_pal_ccs)+
  guides(fill=guide_legend(title="Country"))+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))+
  facet_grid(rows=vars(vpd_short_name), scales = "free_y")

fig1
fig1_name <- paste("fig1",type,year_range, plot_variable, "by", "year","vpd","ccsonly.png", sep="_")
temp_path1 <- file.path(tempdir(), fig1_name)

ggsave(filename=temp_path1, width = 12, height = 8, dpi = 300)

### Fig 2: LoD outbreaks, facet CC ################
plot_variable <- "lod_count"
type <- "stackedbar"
plot_vpds_ia2030outbreak <- c("Measles","Poliomyelitis","Yellow fever","Invasive meningococcal disease",
                              "Cholera","Ebola virus disease")
# facet by GID critical country

fig2 <- cc_data %>% 
  filter(vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
  ggplot(aes(x=year, y=value, fill=vpd_short_name))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  ggtitle("Cumulative Count of Large or Disruptive VPD Outbreaks",subtitle="Select VPDs in GID Critical Countries (2018-2023)")+
  xlab("Year")+
  ylab("Number of outbreaks")+
  labs(caption="Note: Definition of 'large or disruptive' varies by VPD. For measles and polio, large or disruptive
       outbreak threshold is measured by 12-month incidence so maximum value per year is one.")+
  scale_y_continuous(labels = number_format(accuracy = 1)) + 
  guides(fill=guide_legend(title="VPD"))+
  scale_fill_manual(values=color_pal_vpds)+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))+
  facet_grid(rows=vars(country_abbrev))
fig2

fig2_name <- paste("fig2",type,year_range, plot_variable, "by","year","country","vpd.png", sep="_")
temp_path2 <- file.path(tempdir(), fig2_name)
ggsave(filename=temp_path2, width = 10, height = 8, dpi = 300)


### Fig 3: LoD outbreaks (n) in all GID CCs, stacked VPD ################
plot_variable <- "lod_count"
type <- "stackedbar"
# no facet (don't disaggregate by country)

total_allvpd_lodos_dat <- cc_data %>% filter(variable==plot_variable & vpd %in% plot_vpds_ia2030outbreak)# use in subtitle
total <- sum(total_allvpd_lodos_dat$value, na.rm=TRUE)

fig3 <- cc_data %>% 
  filter(vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
  ggplot(aes(x=year, y=value, fill=vpd_short_name))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  #geom_text(aes(label = paste(format(round(Projection_Grand_Total/1e6, 1), trim=TRUE),"M"))) +
  ggtitle(paste0("Large or Disruptive VPD Outbreaks from ", year_range,"\n in GID Priority Countries, N=", total),
                 subtitle="Select VPDs in Afghanistan, Brazil, Democratic Republic of the Congo,\nEthiopia, Indonesia, Nigeria, Pakistan, and the Philippines.")+
  xlab("Year")+
  ylab("Number of outbreaks")+
  labs(caption="Note: Definition of 'large or disruptive' varies by VPD. WPV and cVDPV (any type) are counted\nas separate outbreaks. Maximum number of outbreaks any country could have in six-year\nperiod is 36 if WPV is excluded.")+
  scale_y_continuous(labels = number_format(accuracy = 1)) + 
  guides(fill=guide_legend(title="VPD"))+
  scale_fill_manual(values=color_pal_vpds)+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14, face="italic"),
        plot.caption = element_text(hjust=0, vjust=-.5, size=11),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))

fig3
fig3_name <- paste("fig3",type,year_range, plot_variable, "by","year","vpd.png", sep="_")
temp_path3 <- file.path(tempdir(), fig3_name)
ggsave(filename= temp_path3, width = 10, height = 8, dpi = 300)


### Fig 4: LoD outbreaks (>=1) in all GID CCs, stacked VPD ################
plot_variable <- "lod_count_atleastone"
type <- "stackedbar"
# no facet (not years)

cc_data$ctry_labs <- cc_data$country_name
cc_data$ctry_labs[cc_data$country_name=="Democratic Republic of the Congo"] <- "Democratic Republic\nof the Congo"

fig4 <- cc_data %>% 
  filter(vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
  ggplot(aes(x=ctry_labs, y=value, fill=vpd_short_name))+
  geom_text(aes(label = value),
            vjust = .25,
            colour = "white") +
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  ggtitle("Large or Disruptive VPD Outbreaks",subtitle="Select VPDs in GID Priority Countries, 2018-2023")+
  xlab("GID Priority Country")+
  ylab("Cumulative # years country experienced\nat least one outbreak for each VPD")+
  labs(caption="Note: Definition of 'large or disruptive' varies by VPD.")+
  scale_y_continuous(labels = number_format(accuracy = 1)) + 
  guides(fill=guide_legend(title="Disease"))+
  scale_fill_manual(values=color_pal_vpds)+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(vjust=0.5,size=12))

fig4
fig4_name <- paste("fig4",type,year_range, plot_variable, "by","year","vpd.png", sep="_")
temp_path4 <- file.path(tempdir(), fig4_name)
ggsave(filename=temp_path4,width = 10, height = 8, dpi = 300)


### Fig 5: LoD outbreaks (n) in all GID CCs, stacked VPD ################
plot_variable <- "lod_count"
type <- "stackedbar"
# no facet (not years)

fig5 <- cc_data %>% 
  filter(vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
  ggplot(aes(x=ctry_labs, y=value, fill=vpd_short_name))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  ggtitle("Large or Disruptive VPD Outbreaks",subtitle="Select VPDs in GID Priority Countries, 2018-2023")+
  xlab("Country")+
  ylab("Cumulative number of outbreaks")+
  labs(caption="Note: Definition of 'large or disruptive' varies by VPD.")+
  scale_y_continuous(labels = number_format(accuracy = 1)) + 
  guides(fill=guide_legend(title="Disease"))+
  scale_fill_manual(values=color_pal_vpds)+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(vjust=0.5,size=12))

fig5

fig5_name <- paste("fig5",type,year_range, plot_variable, "by","year","vpd.png", sep="_")
temp_path5 <- file.path(tempdir(), fig5_name)
ggsave(filename=temp_path5, width = 10, height = 8, dpi = 300)



### Fig 6: LoD outbreaks (>=1) in all countries in region ###############
plot_variable <- "lod_count_atleastone"
type <- "stackedbar"
# no facet (not years)

data_grouped <- data_clean %>% filter(year %in% plot_years, variable==plot_variable) %>% 
  group_by(gid_region_abbr,country_abbrev, vpd_short_name) %>% summarise(value= sum(value), na.rm=TRUE)

## calculate values for total # of countries that experienced LoD outbreaks of mealses and polio, respectively, every year in 6-year period
# data_grouped %>% filter(vpd_short_name=="Measles", value ==6 ) %>% nrow() # 7 countries
# data_grouped %>% filter(vpd_short_name=="Measles", value ==6 ) %>% head()
# 
# data_grouped %>% filter(vpd_short_name=="Polio", value ==6 ) %>% nrow() # 6 countries
# data_grouped %>% filter(vpd_short_name=="Polio", value ==6 ) %>% head()

# for each region:

for (region in unique(data_grouped$gid_region_abbr)) {

  fig6 <- data_grouped %>% filter(!(is.na(vpd_short_name)) & gid_region_abbr==region) %>% # drop cVDPV", "WPV"
  #  filter(country_abbrev %in% plot_countries & year %in% plot_years & vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
    ggplot(aes(x=country_abbrev, y=value, fill=vpd_short_name))+
    geom_text(aes(label = value),
              #y = label_y,
              # hjust = hjust_value),
              vjust = .25,
              colour = "white") +
    geom_bar(stat="identity", position="dodge")+
    geom_col()+
    #geom_text(aes(label = paste(format(round(Projection_Grand_Total/1e6, 1), trim=TRUE),"M"))) +
    ggtitle(paste0("Large or Disruptive VPD Outbreaks in ", region), subtitle="Select VPDs between 2018-2023")+
    #ggtitle("Cumulative VPD Case Counts, 2018-2023",subtitle=paste("Total (USD$): ",format(round(GID_total/1e6, 1), trim=TRUE),"M"))+
    xlab("Country")+
    ylab("Cumulative # years country experienced\noutbreaks of each VPD")+
    labs(caption="Note: Definition of 'large or disruptive' varies by VPD.")+
    scale_y_continuous(labels = number_format(accuracy = 1)) + 
    guides(fill=guide_legend(title="Disease"))+
    scale_fill_manual(values=color_pal_vpds)+
    theme_bw()+
    theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
          plot.subtitle=element_text(hjust=0.5, size=14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.title.x = element_text(vjust=0.5,size=12))
  
  fig6
  # fig6_name <- paste(datt_task_id,"fig6",region,type,year_range, plot_variable, "by","year","vpd", sep="_")
  # ggsave(filename=paste0(datt_task_id,"/outputs/",fig6_name,".png"), fig6, width = 10, height = 8, dpi = 300)
}

### Fig 7: LoD outbreaks maps ###############
plot_variable <- "lod_count_atleastone"
type <- "stackedbar"
# no facet (not years)


## ALL COUNTRIES
fig_data <- data_clean %>% filter(year %in% plot_years, variable==plot_variable)

fig_vpd <- "Measles"
fig_dat <- fig_data %>% filter(vpd_short_name=="Measles") # filter in loop



# add count of lodo at least one across years in plot_years
data_grouped_measles <- data_clean %>% filter(year %in% plot_years, variable=="lod_count_atleastone", vpd_short_name==fig_vpd) %>% 
  group_by(gid_region_abbr,gid_critical_countries,iso3_code, country_name, country_abbrev,vpd,vpd_short_name, variable) %>%
  summarise(value_lodoatl= sum(value, na.rm=TRUE))

# group cases by year
data_grouped_cases <- data_clean %>% filter(year %in% plot_years, variable=="cases", vpd_short_name==fig_vpd) %>% 
  group_by(gid_region_abbr,gid_critical_countries,iso3_code, country_name, country_abbrev,vpd,vpd_short_name, variable) %>%
  summarise(value_cases= sum(value, na.rm=TRUE))

## Join shapefile with dataframe
joined_dat <- ctry.sp %>% left_join(
  data_grouped_measles, by="iso3_code"
) %>% left_join(data_grouped_cases, by="iso3_code")

## test
ggplot(data = joined_dat) +
  geom_sf(aes(fill = value_lodoatl, col=gid_critical_countries.x)) +
  theme_minimal()

ggplot(data = joined_dat) +
  geom_sf(aes(fill = value_cases, col=gid_critical_countries.y)) +
  theme_minimal()


### Fig 7: LoD outbreaks (>=1) in all countries ###############
plot_variable <- "lod_count_atleastone"
type <- "stackedbar"
# no facet (not years)

# View first few rows to check results
head(joined_data_with_centroids)

joined_dat$gid_critical_countries.x[is.na(joined_dat$gid_critical_countries.x)] <- "No"

# Define a function to create range labels for the legend
create_range_labels <- function(cases) {
  if (is.na(cases)) {
    return("Missing")
  } else if (cases == 0) {
    return("0")
  } else {
    # Create ranges based on cumulative cases
    lower_bound <- floor(cases / 100) * 100 + 1
    upper_bound <- ceiling(cases / 100) * 100
    
    # Format as "X-Y K"
    return(paste(lower_bound, "-", upper_bound, "K", sep = ""))
  }
}

# Assuming 'joined_dat' has a column 'value_cases'
# Create a new column for range labels based on value_cases
breaks <- seq(0, max(joined_dat$value_cases, na.rm = TRUE), by = 100)
labels <- sapply(breaks[-1], create_range_labels)


  fig7 <- joined_dat  %>% # measles alone
    ggplot(aes())+
    geom_sf(aes(fill = value_lodoatl, col=gid_critical_countries.x)) +
    geom_point(data = joined_data_with_centroids, aes(x = longitude, y = latitude, size = value_cases), 
               color = "blue", alpha = 0.5) + 
    ggtitle(paste0("Countries with Large or Disruptive\n", fig_vpd, " Outbreaks, ", year_range),
            subtitle=paste0("Total # Cases Globally: 2.39 M"))+
    scale_fill_gradient(low='#e5c0ca',high='#a51d42',
                        labels=c("1 of 6 years", "2 of 6 years", "3 of 6 years", "4 of 6 years", "5 of 6 years", "6 of 6 years"))+
    scale_color_manual(values=c("black","#dfe320","gray"), name="GID Priority Country")+
    guides(fill=guide_legend(title="# of Years between 2018-2023\nthat Outbreak Reached\nLarge or Disruptive\nOutbreak Incidence Threshold"))+
    scale_size_continuous(breaks=breaks[-1], labels=labels,
                          name="Cumulative Reported\nCases") +  # Format size labels here
    theme_bw()+
    theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
          plot.subtitle=element_text(hjust=0.5, size=14),
          axis.title.x = element_blank(),  # Remove x-axis title
          axis.title.y = element_blank(),  # Remove y-axis title
          axis.text.x = element_blank(),   # Optionally remove x-axis text (tick marks)
          axis.text.y = element_blank()    # Optionally remove y-axis text (tick marks)
          )
  
  fig7
  fig7_name <- paste(datt_task_id,"fig7",region,type,year_range, plot_variable, "by","year","vpd", sep="_")
  ggsave(filename=paste0(datt_task_id,"/outputs/",fig7_name,".png"), fig6, width = 10, height = 8, dpi = 300)

  size=guide_legend(title="Bubble Size Title")
  
  
  ### Fig 8: >=1 LoD outbreaks - facet each VPD alone ################
  plot_variable <- "lod_count_atleastone"
  type <- "bar"
  # will only show VPDs with large or disruptive outbreak data
  # removing NA excludes separate counts for cVDPV and WPV

  fig8 <- cc_data %>% filter(year %in% plot_years, !(is.na(vpd_short_name)),
                             variable==plot_variable) %>% 
    ggplot(aes(x=country_abbrev, y=value, fill=vpd_short_name))+
    geom_text(aes(label = value),
              vjust = .25,
              colour = "white") +
    geom_bar(stat="identity", position="dodge")+
    geom_col()+
    ggtitle(paste0("Number of Years Country Experienced\nLarge or Disruptive VPD Outbreaks"),
            subtitle="GID Priority Countries between 2018-2023 (Max = 6)")+
    xlab("GID Priority Country")+
    ylab("Number of years")+
    labs(caption="Note: Definition of 'large or disruptive' varies by VPD.")+
    scale_y_continuous(labels = number_format(accuracy = 1)) + 
    guides(fill=guide_legend(title="Disease"))+
    scale_fill_manual(values=color_pal_vpds)+
    theme_bw()+
    theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
          plot.subtitle=element_text(hjust=0.5, size=14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.title.x = element_text(vjust=0.5,size=12))+
    facet_wrap(~vpd_short_name)
  
  fig8
  fig8_name <- paste(datt_task_id,"fig8",type,year_range, plot_variable, "by","year", "vpd", sep="_")
  ggsave(filename=paste0(datt_task_id,"/outputs/",fig8_name,".png"), fig4, width = 10, height = 8, dpi = 300)
  
  
## Write figs to Teams#########################################################
### Access to Task Team Teams Channel ("DATT")

figs_upload <- c(fig1_name,fig2_name,fig3_name,fig4_name,fig5_name) # Taking out 6 
temp_upload <- c(temp_path1,temp_path2,temp_path3,temp_path4,temp_path5) # Taking out 6 
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

# dstt <- get_team("GHC_GID_Data_&_Strategy_Tiger_Team")
# dstt_channels <- dstt$list_channels()
# datt <- dstt$get_channel("Data Analytics Task Team")
# 
# # get sharepoint site and default document library associated with team
# dstt_site <- dstt$get_sharepoint_site()
# 
# datt_docs <- datt$get_folder()
# doc_path <- datt_docs$get_path() # "/Data%20Analytics%20Task%20Team"
# items <- datt_docs$list_items()
# 
# teams_data_folder <- datt_docs$get_item("2. Datasets_clean")
# teams_data_files <- teams_data_folder$list_files()
# 
# teams_fig_folder <- datt_docs$get_item("3. Figures")
# teams_fig_files <- teams_fig_folder$list_files()
# 
# ## Save figure 4 to Teams (update if want to save others)
# filename = paste0(datt_task_id,"/outputs/",fig_name,".png")
# teams_fig_folder$upload(paste0(filename))

