# Map for GID Policy 
# Countries with Polio detections in the last 12 months 
# Use an anchor point of most recent detection and then floor the date to start 

install.packages("giscoR")
library("giscoR")
# Load Packages 
source("./reference/obx_packages_01.R", local = T)
# Load Data 
if (exists("raw.data") == TRUE){
  cli::cli_alert("Core data already loaded")
}else{
  raw.data <- get_all_polio_data(size = "small")
}

# Output 
datt_task_id <- "R36"

sub_folder <- "3. Figures"

fig_name <- paste("map_policy_pv_12", format(today(), "%y%m%d"),".png", sep="")

temp_path <- file.path(tempdir(), fig_name)

sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")



# Do you want to upload to sharepoint 
uploadtosp <- "yes"

## Load Sharepoint Site & Writing Functions 

get_sharepoint_site(
  site_url = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  tenant = Sys.getenv("CLIMICROSOFT365_TENANT", "common"),
  app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
  scopes = c("Group.ReadWrite.All", "Directory.Read.All", "Sites.ReadWrite.All",
             "Sites.Manage.All"),
  token = NULL)


# Load Inputs 
# Dates (last 12 months)
date_end <- floor_date(raw.data$metadata$download_time, "week", week_start = 1) %m+% days(1)
date_start <- floor_date(date_end %m-% months(13), "month")


# Virus types 
o_vtype <- c("cVDPV 1", "cVDPV 2", "cVDPV 3")
# Surveillance sources 
s1 <- c("AFP", "ENV")
# Endemic Countries to highlight  

corrected.global.ctry <- giscoR::gisco_get_countries() |> 
  filter(NAME_ENGL != "Antarctica")

# Any countries with poliovirus detections
data1 <- raw.data$pos %>% 
        filter(measurement %in% o_vtype & 
               source %in% s1 &
               dateonset <= date_end &
               dateonset >= date_start) %>%
        distinct(epid, measurement, .keep_all =T)

ctry_data <- data1 %>% 
               arrange(place.admin.0, dateonset) %>%
               group_by(place.admin.0) %>% 
               summarise(pv_last = last(dateonset)) %>% 
               mutate(pv_yes = case_when(
                 pv_last >= floor_date(date_end %m-% months(7), unit = "month") ~ "6_months",
                 TRUE ~ "12 month"))

corrected.global.ctry <- corrected.global.ctry %>% 
                           mutate(eng_name = toupper(NAME_ENGL),
                                  eng_name2 = case_when(
                                    NAME_ENGL == "Côte D’Ivoire" ~ "COTE D IVOIRE",
                                    NAME_ENGL == "United Kingdom" ~ "THE UNITED KINGDOM",
                                    NAME_ENGL == "PALESTINE " ~ "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM",
                                    TRUE ~ eng_name))
                                  

corrected.global.ctry <- left_join(corrected.global.ctry, 
                                   ctry_data, 
                                   by = c("eng_name2" = "place.admin.0"))

test <- anti_join(ctry_data, corrected.global.ctry, 
                                   by = c("place.admin.0" = "eng_name2"))
   

#mercator projection
map1  <- ggplot() + 
  geom_sf(data = corrected.global.ctry, aes(fill=pv_yes), color = "grey30", lwd = 0.5) + 
  coord_sf(crs = "EPSG:3857") +
  scale_fill_manual(name = "Time since last\ncVDPV detections:", 
                     values = c("6_months" = "#009ACD",
                                "12 month" = "#8DEEEE"), 
                     breaks = c("6_months", "12 month"),
                     labels = c("<6 months", "6-12 months"),
                     na.value = "grey90") +
  coord_sf(xlim = c(-25, 160), ylim = c(-45, 90), expand = FALSE) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_text(face="bold", size = 18),
        legend.text = element_text(size = 20),
        axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(),
        panel.background = element_rect(fill = "#E3EBFF"),
        legend.key = element_blank(),
        plot.title = element_markdown(hjust = 0.5, face="bold", size = 24),
        plot.caption = element_text(size =18)) + 
  labs(title = paste("Title here"))
print(map1)

# Save Locally
if(uploadtosp == "yes"){
  
  ggsave(filename = temp_path, height = 8, width = 8, units = "in", scale = 1.5, dpi = 300)
  cli::cli_alert("Beep Beep - Image Saved to Temp Folder")
  
  upload_to_sharepoint(
    file_to_upload = temp_path,  # Writes to temp directory 
    sharepoint_file_loc = sp_path,
    site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
    drive = "Documents")
  
  cli::cli_alert("Beep Beep - Image uploaded to Sharepoint")
  
}else{
  cli::cli_alert("No output saved")
}
