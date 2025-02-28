# Create Emergence Color Scheme: 
# New Map For Transition Polio 
library(sirfunctions)
source("./reference/obx_packages_01.R", local = T)


data <- read_excel("./special_projects/obx_unit/orpg_monthly_update/who_obdashboard_250225.xlsx")

# Runs Locally off of the GPEI polio outbreak table 
## Adding other parameters
#Pull Country Shape 
ctry <- raw.data$global.ctry

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

theme_1a <- theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), # centers graph 
        #axis.title.x=element_blank(), # removes the coordinates / lines 
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        #axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.y=element_blank(),
        legend.position = "bottom")

theme_1c <- theme_bw() +
  theme(
    axis.title.x=element_blank(), # removes the coordinates / lines 
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "right")

# Inputs 
# Output 
datt_task_id <- "R4"
sub_folder <- "3. Figures"

# with table 
fig_name2 <- paste("pv_ctry_panel",e_w1,"_", format(today(), "%y%m%d"),".png", sep="")
temp_path2 <- file.path(tempdir(), fig_name2)
sp_path2 <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name2, sep ="")


ctry <- ctry %>%
           mutate(
             cir_status_fact = factor(cir_status, levels = c("cVDPV1", "cVDPV1 & cVDPV2", "cVDPV2", 
                                                              "cVDPV2 & cVDPV3", "cVDPV3")))

#filter to AFRO and EMRO regions
ds1 <- ctry %>% filter(WHO_REGION == "AFRO" | WHO_REGION == "EMRO")
ds2 <- ctry %>% filter(WHO_REGION == "SEARO" | WHO_REGION == "WPRO")
ds3 <- ctry %>% filter(WHO_REGION %in% c("EURO"))
ds4 <- ctry %>% filter(WHO_REGION %in% c("AMRO"))
           

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

print(gs4)

combined_plot <- (gs1 | gs4 | gs3 | gs2)
ggsave(filename = paste("./output/tiger/ctry_maps", date_1,".png"), height = 4.1, width = 10.08, scale = 1.75, dpi = 300)

print(combined_plot)
