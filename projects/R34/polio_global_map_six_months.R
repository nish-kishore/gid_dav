## R2 - Update weekly
## Global Polio Map - need to check output and reference 
## Updated - 13-02-2025 
## By: JPB 

# Load Packages 
source("./reference/obx_packages_01.R", local = T)


# Load Data 
if (exists("raw.data") == TRUE){
  cli::cli_alert("Core data already loaded")
}else{
  raw.data <- get_all_polio_data(size = "small")
}


# # Load Inputs 
# # Dates (last 12 months)
# date_end <- floor_date(raw.data$metadata$download_time, "week", week_start = 1) %m+% days(1)
# date_start <- date_end %m-% days(365)
# # Epi Week 
# e_w <- paste0("W",as.character(epiweek(date_end)),",", sub="")
# e_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")
# Virus types 
o_vtype <- c("cVDPV 2")
# Surveillance sources 
s1 <- c("AFP", "ENV")
# Endemic Countries to highlight  
endem_ctry <- c("AFGHANISTAN", "PAKISTAN")

d4 <- raw.data$metadata$download_time
d3 <- d4 %m-% months(6)
d2 <- d4 %m-% months(12)
d1 <- d4 %m-% months(18)

# Output 
datt_task_id <- "R34"

sub_folder <- "3. Figures"

fig_name <- paste("c2map","_", format(today(), "%y%m%d"),".png", sep="")

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





# Get dataset 
pos1 <- raw.data$pos %>% 
            filter(dateonset >= d3 & 
                   dateonset <= d4 & 
                   measurement %in%  o_vtype &
                   source %in% s1) %>%
            distinct(epid, measurement, .keep_all = T) %>%
            mutate(
              vtype_all = case_when(
                measurement == "cVDPV 1" &  source == "AFP" ~ "cVDPV1 case", 
                measurement == "cVDPV 1" &  source == "ENV" ~ "cVDPV1 ES", 
                measurement == "cVDPV 2" &  source == "AFP" ~ "cVDPV2 case", 
                measurement == "cVDPV 2" &  source == "ENV" ~ "cVDPV2 ES", 
                measurement == "cVDPV 3" &  source == "AFP" ~ "cVDPV3 case", 
                measurement == "cVDPV 3" &  source == "ENV" ~ "cVDPV3 ES", 
                measurement == "WILD 1"  &  source == "AFP" ~  "WPV1 case", 
                measurement == "WILD 1"  &  source == "ENV" ~  "WPV1 ES"),
                vtype_all1 = factor(vtype_all, levels = 
                                    c("WPV1 case","WPV1 ES", "cVDPV1 ES", "cVDPV1 case", 
                                      "cVDPV2 ES", "cVDPV2 case", "cVDPV3 ES", "cVDPV3 case")))

# Pull coordinates for map sizing - will need to check each week: 
# Some suggested ones based on current data 
# coord_sf(xlim = c(-55, 145), ylim = c(-35, 68), expand = FALSE) + 

t1a <- pos1 %>% 
       mutate(latitude = as.numeric(latitude)) %>% 
      arrange(latitude) %>% 
      summarise(min_lat = min(latitude), 
                max_lat = max(latitude)) 

t1b <- pos1 %>% 
  mutate(longitude = as.numeric(longitude)) %>% 
  arrange(longitude) %>% 
  summarise(min_long = min(longitude), 
            max_long = max(longitude)) 
  # Fix proxy to get shape files back 


ctry <- raw.data$global.ctry %>% 
         mutate(endemic = if_else(ADM0_NAME %in% endem_ctry, "Y", "N"))

ctry_sub <- ctry %>% filter(endemic == "Y")

# Create graph 
g2 <- ggplot() + 
  geom_sf(data=ctry, fill = "white", color = "grey70", lwd = 0.5, show.legend = NA) +
  geom_sf(data=ctry_sub, aes(fill=endemic), color = "grey70", lwd = 0.5, show.legend = NA) +
  geom_point(data = pos1 %>% filter(source=="ENV"), aes(x = as.numeric(longitude),
                                y = as.numeric(latitude),
                                color=vtype_all1, shape = vtype_all1), size = 0.9, show.legend = T) +
  geom_point(data = pos1 %>% filter(source=="AFP"), aes(x = as.numeric(longitude),
                              y = as.numeric(latitude),
                              color=vtype_all1, shape = vtype_all1), size = 0.9, show.legend = NA) +
  # coord_sf(xlim = c(-55, 145), ylim = c(-35, 68), expand = FALSE) + 
  coord_sf(xlim = c(-25.4, 147+5), ylim = c(-19.7-10.5, 61.4+7), expand = FALSE) + 
  scale_color_manual(name = "Poliovirus\ndetection:", 
                    values = c("cVDPV2 case"= "#38A800", 
                               "cVDPV2 ES"= "#61DB2C"), 
                    breaks = c("cVDPV2 case", "cVDPV2 ES"), 
                    drop = FALSE) +
  scale_shape_manual(name = "Poliovirus\ndetection:",
                     values = c(
                                "cVDPV2 case"= 16,
                                "cVDPV2 ES"= 15),
                     breaks = c(
                                "cVDPV2 case", "cVDPV2 ES"),
  drop = FALSE)  +
  scale_fill_manual(name = " \n \n", values = c("Y"="#FFFFBE"), labels = "Endemic country (WPV1)") +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face="bold", size = 10),
        legend.text = element_text(size = 8),
        axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(),
        panel.background = element_rect(fill = "#E3EBFF"),
        legend.key = element_blank(),
        plot.title = element_markdown(hjust = 0.5, face="bold", size = 10),
        plot.caption = element_text(size =8)) +
  guides(
    color = guide_legend(override.aes = list(size = 1), ncol = 2),
     fill = guide_legend(override.aes = list(size = 0), ncol = 1)) + 
  ggtitle(paste0("2024-12-12", " to ", d4))

  # labs(caption = paste("*paralysis onset / ES sample collection from", format(date_start, "%d %b %y"), "to", format(date_end, "%d %b %y"),"\nES: environmental surveillance; cVDPV: circulating variant poliovirus; WPV: wild poliovirus 
  #                      Produced by CDC-GHC-GID-PEB; Data available through GPEI as of", format(date_end, "%d %b %y"), ""), 
  #      title = paste0("Overview of global WPV1 and cVDPV detections at ", as.character(year(date_end)),"-",e_w," previous 12 months*", sub=" ")) 
  

# Check graph
print(g2)

map_a <- g1 
map_b <- g2 
map_c <- g2 

# Get legeng 
leg <- get_legend(g2, position = NULL)
leg1 <- as_ggplot(leg)

print(leg1)

datt_task_id <- "R34"
sub_folder <- "3. Figures"
fig_name <- paste("cVDPV2_leg_", raw.data$metadata$download_time,"v2.png", sep = "")
out_file <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")
# 
# ggsave(filename = paste("./output/tiger/wpv1_graph_3panel_", d4,".png"), dpi = 300, height = 4, width = 9)
ggplot2::ggsave(out_file, plot = leg1, dpi = 300, height = 1, width = 6, bg = "white")

upload_to_sharepoint(
  file_to_upload = out_file,  # Writes to temp directory
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")
