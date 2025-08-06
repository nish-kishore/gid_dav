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
o_vtype <- c("cVDPV 1", "cVDPV 2", "cVDPV 3", "WILD 1")
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

cvdpv_data <- data1 %>% 
               arrange(place.admin.0, dateonset) %>%
               filter(measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3")) %>%
               group_by(place.admin.0) %>% 
               summarise(pv_last = last(dateonset)) %>% 
               mutate(pv_yes = case_when(
                 pv_last >= floor_date(date_end %m-% months(7), unit = "month") ~ "c_6_months",
                 TRUE ~ "c_12_month"))

wpv_data <- data1 %>% 
  arrange(place.admin.0, dateonset) %>%
  filter(measurement %in% c("WILD 1")) %>%
  group_by(place.admin.0) %>% 
  summarise(pv_last = last(dateonset)) %>% 
  mutate(pv_yes = case_when(
    pv_last >= floor_date(date_end %m-% months(7), unit = "month") ~ "w_6_months",
    TRUE ~ "w_12_month"))

ctry_data <- bind_rows(cvdpv_data, wpv_data) %>%
                mutate(pv_yes = factor(pv_yes, levels = c("c_6_months", "w_6_months", "c_12_month", "w_12_month")))

counts <- ctry_data %>% 
            group_by(pv_yes) %>%
            count() %>%
            

corrected.global.ctry <- corrected.global.ctry %>% 
                           mutate(eng_name = toupper(NAME_ENGL),
                                  eng_name2 = case_when(
                                    NAME_ENGL == "Côte D’Ivoire" ~ "COTE D IVOIRE",
                                    NAME_ENGL == "United Kingdom" ~ "THE UNITED KINGDOM",
                                    NAME_ENGL == "Palestine" ~ "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM",
                                    NAME_ENGL == "Guyana" ~ "FRENCH GUIANA",
                                    TRUE ~ eng_name))
                                  

corrected.global.ctry <- left_join(corrected.global.ctry, 
                                   ctry_data, 
                                   by = c("eng_name2" = "place.admin.0"))

test <- anti_join(ctry_data, corrected.global.ctry, 
                                   by = c("place.admin.0" = "eng_name2"))
   

#mercator projection
map1  <- ggplot() + 
  geom_sf(data = corrected.global.ctry, aes(fill=pv_yes), color = "grey30", lwd = 0.5, show.legend = T) + 
  coord_sf(crs = "EPSG:3857") +
  scale_fill_manual(name = "Time since last\n poliovirus detection (#):", 
                     values = c("c_6_months" = "#009ACD",
                                "w_6_months" = "#b22222",
                                "c_12_month" = "#8DEEEE",
                                "w_12_month" = "#ffa07a"), 
                     breaks = c("c_6_months",  "w_6_months", "c_12_month", "w_12_month"),
                     labels = c(paste0("cVDPV: <6 months (N=",counts$n[1],")"), 
                                paste0("WPV: <6 months (N=",counts$n[2],")"),
                                paste0("cVDPV: 6-12 months (N=",counts$n[3],")"),
                                paste0("WPV: 6-12 months (N=0)")),
                            na.value = "grey90", 
                    drop = FALSE) +
  coord_sf(xlim = c(-65, 160), ylim = c(-45, 90), expand = FALSE) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_text(face="bold", size = 18),
        legend.text = element_text(size = 14),
        axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(),
        panel.background = element_rect(fill = "#E3EBFF"),
        legend.key = element_blank(),
        plot.title = element_markdown(hjust = 0.5, face="bold", size = 24),
        plot.caption = element_text(size =12)) +
  guides(fill=guide_legend(ncol=2)) +
  labs(title = paste0("Countries with poliovirus detections in the last 12 months* (N=", nrow(ctry_data), ")"),
       caption = paste0("*paralysis onset / ES sample collection from ", format(date_start, "%d %b %y"), " to ", format(date_end, "%d %b %y"),"\nES: environmental surveillance; cVDPV: circulating variant poliovirus; WPV: wild poliovirus
                       Produced by CDC-GHC-GID-PEB; Data available through GPEI as of ", format(date_end, "%d %b %y"), ""))
print(map1)

# Save Locally
if(uploadtosp == "yes"){
  
  ggsave(filename = temp_path, height = 9, width = 14, units = "in",  dpi = 300,bg = 'white' )
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
