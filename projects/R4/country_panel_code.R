# Create Emergence Color Scheme: 

source("./reference/obx_packages_01.R", local = T)
# Read Data 
data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/pv_ob_table.rds")
raw.data <- get_all_polio_data()

# Inputs 
# Output 
datt_task_id <- "R4"
sub_folder <- "3. Figures"
date_end <- floor_date(raw.data$metadata$download_time, "week", week_start = 1) %m+% days(1)

# With table 
e_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")
fig_name <- paste("pv_ctry_panel",e_w1,"_", format(today(), "%y%m%d"),".png", sep="")
temp_path <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")


## Load Sharepoint Site & Writing Functions 
get_sharepoint_site(
  site_url = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  tenant = Sys.getenv("CLIMICROSOFT365_TENANT", "common"),
  app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
  scopes = c("Group.ReadWrite.All", "Directory.Read.All", "Sites.ReadWrite.All",
             "Sites.Manage.All"),
  token = NULL)


# Dates to be updated 
date_1 = data$metadata$gpei_data_update
v_type <- c("cVDPV 1", "cVDPV 2", "cVDPV 3")

# Runs Locally off of the GPEI polio outbreak table 
## Adding other parameters
#Pull Country Shape 
ctry <- raw.data$global.ctry



# Update the data 
#Add "OutBreakStatus" variable
ob_d <- data$ob_data %>%
  filter(measurement %in% v_type) %>%
  # Updated Veribage - within 13 months: Active within 6 months / active_monitor 6-13 mon / >13 inactive 
  mutate(OutBreakStatus = 
           case_when(
             most_recent > date_1 - months(6) ~ "active",
             most_recent >= date_1 - months(13) ~ "active_monitor",
             TRUE ~ "inactive"),
         # Need to Validate the Open/Closed Status 
         OutBreakStatus2 = 
           case_when(
             most_recent > date_1 - months(6) ~ "active",
             most_recent >= date_1 - months(13) ~ "active_monitor",
             most_recent < date_1 - months(13)  ~"inactive"),
         # Calculate if "OutBreakStatus" changed to "closed" in the past three months
         ChangedToInactive= ifelse(
           OutBreakStatus == "inactive" & most_recent >= (date_1 - months(16)),
           "Yes",
           "No"))


data1 <- ob_d %>% filter(OutBreakStatus == "active" | OutBreakStatus == "active_monitor")



ctry_a <- data1 %>%
  group_by(place.admin.0, measurement) %>%
  mutate(measurement = gsub(" ", "", measurement)) %>%
  count() %>% 
  pivot_wider(names_from =measurement, values_from = n) %>% 
  mutate(cir_status = case_when(
    is.na(cVDPV1) == F & is.na(cVDPV2)== F ~ "cVDPV1 & cVDPV2",
    is.na(cVDPV3) == F & is.na(cVDPV2)== F ~ "cVDPV2 & cVDPV3",
    is.na(cVDPV1) == F  ~ "cVDPV1",
    is.na(cVDPV2) == F  ~ "cVDPV2", 
    is.na(cVDPV3) == F  ~ "cVDPV3"))  %>% 
  ungroup() %>%
  select(place.admin.0, cir_status)

# Graphing Parameters 
theme_1 <- theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # centers graph 
        axis.title.x=element_blank(), # removes the coordinates / lines 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom")


# Figure
ctry <- left_join(ctry, ctry_a, by = c("ADM0_NAME" = "place.admin.0"))


ctry <- ctry %>%
           mutate(
             cir_status_fact = factor(cir_status, levels = c("cVDPV1", "cVDPV1 & cVDPV2", "cVDPV2", 
                                                              "cVDPV2 & cVDPV3", "cVDPV3")))

#filter to AFRO and EMRO regions
ds1 <- ctry %>% filter(WHO_REGION == "AFRO" | WHO_REGION == "EMRO")
ds2 <- ctry %>% filter(WHO_REGION == "SEARO" | WHO_REGION == "WPRO")
ds3 <- ctry %>% filter(WHO_REGION %in% c("EURO"))
ds4 <- ctry %>% filter(WHO_REGION %in% c("AMRO"))
           

l_mp <- ggplot() + 
  geom_sf(data=ds1, aes(fill = as.factor(cir_status)), fill = "grey90") + 
  geom_sf(data=ds1, aes(fill = as.factor(cir_status_fact)), color = "black", show.legend = T) + 
  scale_fill_manual(name = "Outbreak Virus Type:", 
                    values = c("cVDPV1" = "#F9DB6D", 
                               "cVDPV2" = "#26532B" , 
                               "cVDPV3" = "darkorange", 
                               "cVDPV1 & cVDPV2" =  "dodgerblue1", 
                               "cVDPV2 & cVDPV3" =  "violet"),
                    na.translate = F,
                    drop = F) +
  # coord_sf(xlim = c(60, 180), ylim = c(-40, 40), expand = FALSE) + # use for SEARO
  # coord_sf(xlim = c(-15, 60), ylim = c(35, 75), expand = FALSE) + # use for EMRO 
  # coord_sf(xlim = c(-100, -10), ylim = c(-70, 20), expand = FALSE) + # use for AMRO 
  theme_1 + 
  guides(
    fill = guide_legend(nrow = 2))
legend <- get_legend(l_mp)


gs1 <- ggplot() + 
  geom_sf(data=ds1, aes(fill = as.factor(cir_status)), fill = "grey90") + 
  geom_sf(data=ds1, aes(fill = as.factor(cir_status_fact)), color = "black", show.legend = FALSE) + 
  scale_fill_manual(name = "Outbreak Virus Type:", 
                    values = c("cVDPV1" = "#F9DB6D", 
                               "cVDPV2" = "#26532B" , 
                               "cVDPV3" = "darkorange", 
                               "cVDPV1 & cVDPV2" =  "dodgerblue1", 
                               "cVDPV2 & cVDPV3" =  "violet"),
                    na.translate = F,
                    drop = F) +
  # coord_sf(xlim = c(60, 180), ylim = c(-40, 40), expand = FALSE) + # use for SEARO
  # coord_sf(xlim = c(-15, 60), ylim = c(35, 75), expand = FALSE) + # use for EMRO 
  # coord_sf(xlim = c(-100, -10), ylim = c(-70, 20), expand = FALSE) + # use for AMRO 
  theme_1 + 
  guides(
    fill = guide_legend(nrow = 2))
  # labs(caption = paste("Source: POLIS. Data as of", date))


# SEARO / WPRO 
gs2 <- ggplot() + 
  geom_sf(data=ds2, aes(fill = as.factor(cir_status)), fill = "grey90") + 
  geom_sf(data=ds2, aes(fill = as.factor(cir_status_fact)), color = "black", show.legend = FALSE) + 
  scale_fill_manual(name = "Outbreak Virus Type:", 
                    values = c("cVDPV1" = "#F9DB6D", 
                               "cVDPV2" = "#26532B" , 
                               "cVDPV3" = "darkorange", 
                               "cVDPV1 & cVDPV2" =  "dodgerblue1", 
                               "cVDPV2 & cVDPV3" =  "violet"),
                    na.translate = F,
                    drop = F) +
  coord_sf(xlim = c(90, 160), ylim = c(-30, 30), expand = FALSE) + # use for SEARO
  # coord_sf(xlim = c(-15, 60), ylim = c(35, 75), expand = FALSE) + # use for EMRO 
  # coord_sf(xlim = c(-100, -10), ylim = c(-70, 20), expand = FALSE) + # use for AMRO 
  theme_1 +
  labs(caption = paste("Produced by CDC-GHC-GID-PEB. Data available through GPEI as of", date_1))


gs3 <- ggplot() + 
  geom_sf(data=ds3, aes(fill = as.factor(cir_status)), fill = "grey90") + 
  geom_sf(data=ds3, aes(fill = as.factor(cir_status_fact)), color = "black", show.legend = FALSE) + 
  scale_fill_manual(name = "Outbreak (OBX)\n Virus Type:", 
                    values = c("cVDPV1" = "#F9DB6D", 
                               "cVDPV2" = "#26532B" , 
                               "cVDPV3" = "darkorange", 
                               "cVDPV1 & cVDPV2" =  "dodgerblue1", 
                               "cVDPV2 & cVDPV3" =  "violet"),
                    na.translate = F,
                    drop = F) +
  coord_sf(xlim = c(-15, 60), ylim = c(35, 75), expand = FALSE) + # use for EURO
  theme_1 +
  guides(
    fill = guide_legend(nrow = 2))

gs4 <- ggplot() + 
  geom_sf(data=ds4, aes(fill = as.factor(cir_status)), fill = "grey90") + 
  geom_sf(data= raw.data$global.prov %>% filter(ADM0_NAME == "UNITED STATES OF AMERICA"), fill = NA) +
  geom_sf(data=ds4, aes(fill = as.factor(cir_status_fact)), color = "black", show.legend = FALSE) + 
  scale_fill_manual(name = "Outbreak Virus Type:", 
                    values = c("cVDPV1" = "#F9DB6D", 
                               "cVDPV2" = "#26532B" , 
                               "cVDPV3" = "darkorange", 
                               "cVDPV1 & cVDPV2" =  "dodgerblue1", 
                               "cVDPV2 & cVDPV3" =  "violet"),
                    na.translate = F,
                    drop = F) +
 
  coord_sf(xlim = c(-100, -30), ylim = c(-10, 40), expand = FALSE) + # use for AMRO
  theme_1 + 
  guides(
    fill = guide_legend(nrow = 2)) 

# print(gs4)

combined_plot <- (gs1 | gs4 | gs3 | gs2)  
# print(combined_plot)

final <- plot_grid(combined_plot, legend, ncol=1,rel_heights =   c(9,1.4))


ggsave(filename = temp_path, plot = final, height = 4.1, width = 10.08, scale = 1.75, dpi = 300, bg = 'white')

upload_to_sharepoint(
  file_to_upload = temp_path,  # Writes to temp directory 
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")

