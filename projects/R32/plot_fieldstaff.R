## R32 - GID Field staff
# Updated 2025-04-21
# Author: Lori Niehaus / GID Data Analytics Task Team (DATT)
# Purpose: Map of GID field staff footprint

## R SETUP ################################################################

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
datt_task_id <- "R32"
sub_folder <- "3. Figures" # for saving output

## IMPORT DATA ################

# dataset manually extracted from field staff slide shared in Aug 2024 and saved as csv to ADLS

data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt/data_raw", 
                               file_loc = "Aug2024-GID_Field_Staff_List.csv") %>% clean_names()


## GID Reference Data
country_ref <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt/data_clean/", 
                                     file_loc = "ref_country.rds") %>% clean_names()

## GID country shape files dataset
country_ref_shp <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt/data_clean/", 
                                     file_loc = "ctry_shapes_updated.rds") %>% clean_names()

## CLEAN & TRANSFORM DATA ################
# Clean country geo values to match
setdiff(unique(data$country),unique(country_ref$country_name))

data <- data %>% mutate(country = case_when(
  country =="Vietnam" ~ "Viet Nam",
  country =="Republic of the Congo" ~ "Congo",
  TRUE ~ country)
)

## CHECK - 0
#setdiff(unique(data$country),unique(country_ref$country_name))
# setdiff(unique(data$country),unique(country_ref_shp$country_name)) # confirm same for shape file


## Count the # of field staff per country
data_table <- data %>% group_by(country) %>% 
  summarize(
    n_anytype = n(),
    status = case_when(
      any(type=="Detailed") & !any(type =="NSDD-38"| type=="LES") ~ "Detailed",
      any(type=="NSDD-38" | type=="LES") & !any(type =="Detailed") ~ "CDC Country Office",
      all(type=="Contractor") ~ "Contractors Only",
      any(type=="Detailed") & any(type =="NSDD-38" | type=="LES") ~ "CDC Country Office + Detailed", # may include contractors
      TRUE ~ "None"
    )
    ) %>% 
  rename(country_name = country)
# unless specified, any personnel type may include contractors

## Merge with shape file
data_table_shp <- country_ref_shp %>% left_join(data_table, by="country_name")

## Add column for yes or no (binary value)
data_table_shp <- data_table_shp %>% mutate(field_staff_any = case_when(
  n_anytype >=1 ~ "Yes",
  TRUE ~ "No"
  ))

## PLOT MAP ################
plot_variable <- "field-staff-types"
type <- "map"

total_field = sum(data_table_shp$n_anytype, na.rm=TRUE)

# Modify variable before plotting
data_table_shp$status[is.na(data_table_shp$status)] <- "None"  # Replace NA with "None"

status_levels <- c(
  "CDC Country Office" = "#205aa9" ,  # red
  "Detailed" = "#E53935",        # CDC blue
  "CDC Country Office + Detailed" = "#8E44AD",                              
  "Contractors Only" = "#FFA500",                                
  "None" = "#afabab"  # gray (no field staff)
)

data_table_shp <- data_table_shp %>%
  mutate(status = factor(status, levels = names(status_levels)))

fig1 <- data_table_shp  %>% 
  ggplot(aes())+
  geom_sf(data = (data_table_shp),
          aes(fill = status, col="black")) +
  ggtitle(paste0("Countries with GID Field Staff Positions"),
          subtitle=paste0("As of August 2024, N=", total_field, "*"))+
  labs(fill="GID Personnel Type",
    caption="*Total includes 19 CDC Country Office, 39 Detailed, and 27 Contractor positions. Positions may be vacant and\nfootprint in countries with personnel in CDC Country Offices, detailed, or both, may also have additional contractors.")+
  theme_bw()+
  guides(color="none")+ # hide for color attribute
  scale_fill_manual(values=status_levels)+
  scale_color_manual(values='black')+
  theme(plot.title=element_text(hjust=0.5, face="bold",size=20),
        plot.subtitle=element_text(hjust=0.5, size=14),
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.x = element_blank(),   # Optionally remove x-axis text (tick marks)
        axis.text.y = element_blank()    # Optionally remove y-axis text (tick marks)
       # legend.position = "none" # hide legend
  )

fig1
fig1_name <- paste(datt_task_id,"fig1",type,plot_variable, "asofAug2024.png", sep="_")
temp_path <- file.path(tempdir(), fig1_name)
ggsave(filename=paste0(datt_task_id,"/outputs/",fig1_name), fig1, width = 10, height = 8, dpi = 300)

## Save to Teams ####

# Create the output directory if it doesn't exist
output_dir <- file.path(datt_task_id, "outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig1_name, sep ="")

upload_to_sharepoint(
  file_to_upload = file.path(output_dir, paste0(fig1_name, ".png")),  
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")


