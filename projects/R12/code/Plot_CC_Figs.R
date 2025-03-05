## Bar plots of all VPDs in GID Critical Counties - Cumulative of cases and large or disruptive outbreaks
# Updated 2025-03-05
# Author: Lori Niehaus / GID Data Analytics Task Team (DATT)

# change x-var, y-var, and fill var based on desired figure
# save figure using DATT naming conventions: fig#_type_year(s)_yvar_by_xvar1_xvar2_otherinfo.png

## R SETUP#######################################################################
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

datt_task_id <- "R12"

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
plot_vpds_priority <- c("Measles","Rubella","Congenital rubella syndrome (CRS)", "Poliomyelitis")
plot_vpds_ia2030outbreak <- c("Measles","Poliomyelitis","Yellow fever","Invasive meningococcal disease",
                              "Cholera","Ebola virus disease")

## FILTER DATA 9FOR ALL FIGS)
data <- data %>% filter(country_abbrev %in% plot_countries, year %in% plot_years)

## Set color palettes and factor variables for fill variables
color_pal_ccs <- rev(c("#A76DAB","#D76B6E","#334a54","#582d90", # Other Country, 8 GID CCs (individual) - backwards
                 "#007B7A","#00a266","#D5006D","#a51d42"))

color_pal_vpds <- rev(c("#6A5ACD","#CC79A7","#FF9999","#009E73","#0072B2","#FF5733", # VPDs
                 "#C77CFF","#D55E00","#a51d42","#E69F00","#86e0b5","#A5C1BA","#8DA0CB","#A3A500","#007B7A","#041c3a","#66B3FF",
                 "#afabab")) # gray in case for other/na



# convert country abbrev to factor vars to set order for plotting
data$country_abbrev = factor(data$country_abbrev, levels = c("AFG", "BRA", "DRC", "ETH","IDN", "NGA","PAK", 'PHL'))
names(color_pal_ccs) <- levels(data$country_abbrev)

# convert vpds to factor vars to set order for plotting
data$vpd_short_name = factor(data$vpd_short_name, 
                             levels = rev(c("Measles", "Rubella", "CRS", "Polio","Cholera", "Ebola","Mpox", 
                                            "Meningitis","Diphtheria","Yellow fever", "Neonatal tetanus", "Tetanus (neonatal and/or non-neonatal)",
                                            "Typhoid","COVID-19", "Jap encephalitis", "Pertussis","Mumps", "NA")))
names(color_pal_vpds) <- levels(data$vpd_short_name)

## Calculate totals to use in subtitles
# total cumulative LoDO outbreak count, across all 8 countries, in plot_years (2018-2023)
cc_lodo_total <- sum(
  (data %>% filter(variable=="lod_count"))$value, na.rm=TRUE
  )

# total cumulative LoDO outbreak at least one count, across all 8 countries
# aka cumulative # years country experienced outbreaks of each VPD
cc_lodoatlo_total <- sum(
  (data %>% filter(variable=="lod_count_atleastone"))$value, na.rm=TRUE
)


## PRODUCE & SAVE FIGURES ##############

### Fig 1: VPD case counts in CCs, facet VPD ################
plot_variable <- "cases"
type <- "stackedbar"
# facet by vpd

fig1 <- data %>% 
  filter(country_abbrev %in% plot_countries & year %in% plot_years & vpd %in% plot_vpds_priority & variable==plot_variable) %>% 
  ggplot(aes(x=year, y=value, fill=country_abbrev))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  #geom_text(aes(label = paste(format(round(Projection_Grand_Total/1e6, 1), trim=TRUE),"M"))) +
  #theme_void()+
  ggtitle("Cumulative VPD Case Counts",subtitle="GID Critical Countries (2018-2023)")+
  #ggtitle("Cumulative VPD Case Counts, 2018-2023",subtitle=paste("Total (USD$): ",format(round(GID_total/1e6, 1), trim=TRUE),"M"))+
  xlab("Year")+
  ylab("Number of VPD cases")+
  labs(caption="Note: CRS surveillance data is not available for Nigeria or the Philippines.")+
  scale_fill_manual(values=color_pal_ccs)+
  guides(fill=guide_legend(title="Country"))+
  #guides(fill="none")+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))+
  facet_grid(rows=vars(vpd_short_name), scales = "free_y")

fig1
fig1_name <- paste(datt_task_id,"fig1",type,year_range, plot_variable, "by", "year","vpd","ccsonly", sep="_")
ggsave(filename=paste0(datt_task_id,"/outputs/",fig1_name,".png"), fig1, width = 12, height = 8, dpi = 300)

### Fig 2: LoD outbreaks, facet CC ################
plot_variable <- "lod_count"
type <- "stackedbar"
# facet by GID critical country

fig2 <- data %>% 
  filter(country_abbrev %in% plot_countries & year %in% plot_years & vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
  ggplot(aes(x=year, y=value, fill=vpd))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  #geom_text(aes(label = paste(format(round(Projection_Grand_Total/1e6, 1), trim=TRUE),"M"))) +
  #theme_void()+
  ggtitle("Cumulative Count of Large or Disruptive VPD Outbreaks",subtitle="GID Critical Countries (2018-2023)")+
  #ggtitle("Cumulative VPD Case Counts, 2018-2023",subtitle=paste("Total (USD$): ",format(round(GID_total/1e6, 1), trim=TRUE),"M"))+
  #xlab("GID Critical Countries")+
  xlab("Year")+
  ylab("Number of outbreaks")+
  labs(caption="Note: Definition of 'large or disruptive' varies by VPD.")+
  # scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))+
  #scale_y_continuous(labels=unit_format(unit="M", scale=1e-6))+
  scale_y_continuous(labels = number_format(accuracy = 1)) + 
  #scale_x_discrete(labels =c(""))+
  guides(fill=guide_legend(title="VPD"))+
  scale_fill_manual(values=color_pal_vpds)+
  #guides(fill="none")+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))+
  facet_grid(rows=vars(country_abbrev))
#facet_grid(.~vpd)
fig2

fig2_name <- paste(datt_task_id,"fig2",type,year_range, plot_variable, "by","year","country","vpd", sep="_")
ggsave(filename=paste0(datt_task_id,"/outputs/",fig2_name,".png"), fig2, width = 10, height = 8, dpi = 300)


### Fig 3: LoD outbreaks (>=1) in all GID CCs, stacked VPD ################
plot_variable <- "lod_count_atleastone"
type <- "stackedbar"
# no facet (don't disaggregate by country)

fig3 <- data %>% 
  filter(country_abbrev %in% plot_countries & year %in% plot_years & vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
  ggplot(aes(x=year, y=value, fill=vpd_short_name))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  #geom_text(aes(label = paste(format(round(Projection_Grand_Total/1e6, 1), trim=TRUE),"M"))) +
  ggtitle("Large or Disruptive VPD Outbreaks",subtitle="GID Critical Countries (2018-2023)")+
  #ggtitle("Cumulative VPD Case Counts, 2018-2023",subtitle=paste("Total (USD$): ",format(round(GID_total/1e6, 1), trim=TRUE),"M"))+
  xlab("Year")+
  ylab("Number of outbreaks")+
  labs(caption="Note: Definition of 'large or disruptive' varies by VPD.")+
  scale_y_continuous(labels = number_format(accuracy = 1)) + 
  guides(fill=guide_legend(title="VPD"))+
  scale_fill_manual(values=color_pal_vpds)+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(vjust=0.5,size=18))

fig3
fig3_name <- paste(datt_task_id,"fig3",type,year_range, plot_variable, "by","year","vpd", sep="_")
ggsave(filename=paste0(datt_task_id,"/outputs/",fig3_name,".png"), fig3, width = 10, height = 8, dpi = 300)

### Fig 4: LoD outbreaks (>=1) in all GID CCs, stacked VPD ################
plot_variable <- "lod_count_atleastone"
type <- "stackedbar"
# no facet (not years)

fig4 <- data %>% 
  filter(country_abbrev %in% plot_countries & year %in% plot_years & vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
  ggplot(aes(x=country_abbrev, y=value, fill=vpd_short_name))+
  geom_text(aes(label = value),
                #y = label_y,
               # hjust = hjust_value),
            vjust = .25,
            colour = "white") +
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  #geom_text(aes(label = paste(format(round(Projection_Grand_Total/1e6, 1), trim=TRUE),"M"))) +
  ggtitle("Large or Disruptive VPD Outbreaks",subtitle="Select VPDs in GID Priority Countries, 2018-2023")+
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

fig4
fig4_name <- paste(datt_task_id,"fig4",type,year_range, plot_variable, "by","year","vpd", sep="_")
ggsave(filename=paste0(datt_task_id,"/outputs/",fig4_name,".png"), fig4, width = 10, height = 8, dpi = 300)


### Fig 5: LoD outbreaks (n) in all GID CCs, stacked VPD ################
plot_variable <- "lod_count"
type <- "stackedbar"
# no facet (not years)

fig5 <- data %>% 
  filter(country_abbrev %in% plot_countries & year %in% plot_years & vpd %in% plot_vpds_ia2030outbreak & variable==plot_variable) %>% 
  #mutate(country_abbrev = factor(country_abbrev, levels = country_abbrev[order(-value)])) %>% 
  ggplot(aes(x=country_abbrev, y=value, fill=vpd_short_name))+
  geom_bar(stat="identity", position="dodge")+
  geom_col()+
  #geom_text(aes(label = paste(format(round(Projection_Grand_Total/1e6, 1), trim=TRUE),"M"))) +
  ggtitle("Large or Disruptive VPD Outbreaks",subtitle="Select VPDs in GID Priority Countries, 2018-2023")+
  #ggtitle("Cumulative VPD Case Counts, 2018-2023",subtitle=paste("Total (USD$): ",format(round(GID_total/1e6, 1), trim=TRUE),"M"))+
  xlab("Country")+
  ylab("Cumulative number of outbreaks")+
  labs(caption="Note: Definition of 'large or disruptive' varies by VPD.")+
  scale_y_continuous(labels = number_format(accuracy = 1)) + 
  #scale_x_discrete(labels =c(""))+
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

fig5_name <- paste(datt_task_id,"fig5",type,year_range, plot_variable, "by","year","vpd", sep="_")
ggsave(filename=paste0(datt_task_id,"/outputs/",fig5_name,".png"), fig5, width = 10, height = 8, dpi = 300)


### Fig 6: LoD outbreaks (>=1) in all countried in region ###############
plot_variable <- "lod_count_atleastone"
type <- "stackedbar"
# no facet (not years)

data_grouped <- data_clean %>% filter(year %in% plot_years, variable==plot_variable) %>% 
  group_by(gid_region_abbr,country_abbrev, vpd_short_name) %>% summarise(value= sum(value), na.rm=TRUE)

## calculate values for total # of countries that experienced LoD outbreaks of mealses and polio, respectively, every year in 6-year period
data_grouped %>% filter(vpd_short_name=="Measles", value ==6 ) %>% nrow() # 7 countries
data_grouped %>% filter(vpd_short_name=="Measles", value ==6 ) %>% head()

data_grouped %>% filter(vpd_short_name=="Polio", value ==6 ) %>% nrow() # 6 countries
data_grouped %>% filter(vpd_short_name=="Polio", value ==6 ) %>% head()

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
  fig6_name <- paste(datt_task_id,"fig6",region,type,year_range, plot_variable, "by","year","vpd", sep="_")
  ggsave(filename=paste0(datt_task_id,"/outputs/",fig6_name,".png"), fig6, width = 10, height = 8, dpi = 300)
}
## Write figs to Teams#########################################################
### Access to Task Team Teams Channel ("DATT")

figs_upload <- c(fig1_name,fig2_name,fig3_name,fig4_name,fig5_name,fig6_name)

for (n in 1:length(figs_upload)){
  
  fig_name <- paste0(figs_upload[n],".png")
  
  file_path = paste(getwd(),datt_task_id, "outputs", fig_name, sep="/" ) # fig output in GitHub folder
  
  sp_path <- paste("./Data Analytics Task Team/","3. Figures/", datt_task_id,"/", sep ="") # save location in MS Teams folder
  
  temp_path <- file.path(tempdir(), fig_name)
  
  upload_to_sharepoint(
    file_to_upload = file_path,  
    sharepoint_file_loc = sp_path,
    site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
    drive = "Documents")
  
  upload_to_sharepoint(
    file_to_upload = temp_path,  # Writes to temp directory 
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

