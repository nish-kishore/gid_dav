# R20 - Number of Mealses Cases Imported into the US 
# Center US to the middle of the map 

# Load packages: 
source("./reference/obx_packages_01.R", local = T)
library(ggarrow)
# Load Data 
if (exists("raw.data") == TRUE){
  cli::cli_alert("Core data already loaded")
}else{
  raw.data <- get_all_polio_data(size = "small")
}
date_end <- floor_date(raw.data$metadata$download_time, "week", week_start = 1) %m+% days(1)

e_w <- paste0("W",as.character(epiweek(date_end)),",", sub="")
e_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")


# Set up Output 
# Output 
datt_task_id <- "R20"
sub_folder <- "3. Figures"

fig_name <- paste("mr_imports_",e_w1,"_", format(today(), "%y%m%d"),".png", sep="")
temp_path <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")

get_sharepoint_site(
  site_url = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  tenant = Sys.getenv("CLIMICROSOFT365_TENANT", "common"),
  app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
  scopes = c("Group.ReadWrite.All", "Directory.Read.All", "Sites.ReadWrite.All",
             "Sites.Manage.All"),
  token = NULL)

# Get country shape files: 
ctry <- raw.data$global.ctry 

# Load in MR data to merge
cty_sub <- ctry %>% tibble() %>% select(ADM0_NAME, ENDDATE) 

mr_data <- read_excel("reference/mr_import_01_24.xlsx")

mr_test <- left_join(mr_data, cty_sub, by = c("country"="ADM0_NAME"))


# Ctry
ctry_mr <- left_join(ctry, mr_data, by = c("ADM0_NAME"="country"))

ctry_mr <- ctry_mr %>% 
             mutate(
               mr_im_01_24_cat = case_when(
                 mr_im_01_24 == 1 ~ "1",
                 mr_im_01_24 >1 & 
                   mr_im_01_24 <= 10 ~ "2-10",
                 mr_im_01_24 >10 & 
                   mr_im_01_24 <= 25 ~ "11-25",
                 mr_im_01_24 >25 & 
                   mr_im_01_24 <= 50 ~ "26-50",
                 mr_im_01_24 >50 & 
                   mr_im_01_24 <= 100 ~ "51-100",
                 mr_im_01_24 >100 ~ ">100"),
               mr_im_01_24_cat = factor(mr_im_01_24_cat, 
                                        levels = c("1", "2-10", "11-25", "26-50", "51-100", ">100")))
     

mr_sub <-   ctry_mr %>% filter(is.na(mr_im_01_24_cat)==F)  

out1 <- mr_sub %>% filter(mr_im_01_24 >50) %>% select(ADM0_NAME, CENTER_LON, CENTER_LAT)
out2 <- mr_sub %>% 
          as_tibble() %>%
          arrange(WHO_REGION, desc(mr_im_01_24)) %>%
          group_by(WHO_REGION) %>% 
          summarise(top_ctry = first(ADM0_NAME),
                  top_long = first(CENTER_LON), 
                  top_lat = first(CENTER_LAT))
                  

write_xlsx(out2, path = "./output/mr_ctry_v2.xlsx")
mr_ctry_250305 <- read_excel("output/mr_ctry_250305.xlsx")

arrow1 <- mr_sub %>% 
  left_join(.,mr_ctry_250305, by = c("ADM0_NAME"="top_ctry")) %>% 
  filter(is.na(top_long)==F)

# Add in the arrows: 

# 1, 2-10, 11-25, 25-50, 50-100, >100

# domainCRS<- paste('PROJCS["ProjWiz_Custom_Lambert_Azimuthal"',
#                   'GEOGCS["GCS_WGS_1984"',
#                   'DATUM["D_WGS_1984"',
#                   'SPHEROID["WGS_1984",6378137.0,298.257223563]]',
#                   'PRIMEM["Greenwich",0.0]',
#                   'UNIT["Degree",0.0174532925199433]]',
#                   'PROJECTION["Lambert_Azimuthal_Equal_Area"]',
#                   'PARAMETER["False_Easting",0.0]',
#                   'PARAMETER["False_Northing",0.0]',
#                   'PARAMETER["Central_Meridian",-120]',
#                   'PARAMETER["Latitude_Of_Origin",12.55]',
#                   'UNIT["Meter",1.0]]',
#                   sep = ',')


# domainCRS<- paste('PROJCS["ProjWiz_Custom_Lambert_Azimuthal"',
#                   'GEOGCS["GCS_WGS_1984"',
#                   'DATUM["D_WGS_1984"',
#                   'SPHEROID["WGS_1984",6378137.0,298.257223563]]',
#                   'PRIMEM["Greenwich",0.0]',
#                   'UNIT["Degree",0.0174532925199433]]',
#                   'PROJECTION["Lambert_Azimuthal_Equal_Area"]',
#                   'PARAMETER["False_Easting",0.0]',
#                   'PARAMETER["False_Northing",0.0]',
#                   'PARAMETER["Central_Meridian",-120]',
#                   'PARAMETER["Latitude_Of_Origin",12.55]',
#                   'UNIT["Meter",1.0]]',
#                   sep = ',')

map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = ctry_mr,  fill = "#FFFFFF") +
  ggplot2::geom_sf(data = mr_sub , aes(fill = mr_im_01_24_cat), color = "grey20", lwd = 0.4) +
  scale_fill_brewer(name= "No. of Measles\nImportations\nto the USA:\n('01-'24)" , palette = "GnBu") +
  geom_arrow_curve(data=arrow1 , aes(x = top_long, y = top_lat, 
                                                            xend = US_CENTER_LON, yend = US_CENTER_LAT), 
                   mid_place = 1, col = "black", size = 1, curvature = .3,
                   lineend = "square") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "#E3EBFF"),
        legend.title = element_text(face="bold", size = 12),
        legend.text = element_text(size = 12),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_markdown(hjust = 0.5, face="bold", size = 12)) +
  guides(fill = guide_legend(nrow = 1)) +
  coord_sf(xlim = c(-127, 150), ylim = c(-20, 75), expand = FALSE)  

  
  

print(map)

ggsave(filename=temp_path, height = 4, width = 8, unit = "in", dpi = 300)

upload_to_sharepoint(
  file_to_upload = temp_path,  # Writes to temp directory 
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")



mp1 <- fortify(map(fill=TRUE, plot=FALSE))
mp2 <- mp1
mp2$long <- mp2$long + 360
mp2$group <- mp2$group + max(mp2$group) + 1
mp <- rbind(mp1, mp2)
ggplot(aes(x = long, y = lat, group = group), data = mp) + 
  geom_path() + 
  scale_x_continuous(limits = c(0, 360))


library(ggplot2)
library(dplyr)
library(maps)

# Load world map data
world <- map_data("world")

# Shift longitude values
world <- world %>%
  mutate(long = ifelse(long < 0, long + 360, long))

# # Plot the map centered on the International Date Line
# ggplot(world, aes(x = long, y = lat, group = group)) +
#   geom_polygon(fill = "lightblue", color = "black") +
#   coord_quickmap(xlim = c(0, 360), ylim = c(-90, 90)) +
#   scale_x_continuous(breaks = seq(0, 360, by = 60),
#                      labels = c("180°W", "120°W", "60°W", "0°", "60°E", "120°E", "180°E")) +
#   labs(title = "World Map Centered on the International Date Line",
#        x = "Longitude",
#        y = "Latitude") +
#   theme_minimal()


library(rnaturalearth)
library(tidyverse)


# domainCRS<- paste('PROJCS["ProjWiz_Custom_Lambert_Azimuthal"',
#                   'GEOGCS["GCS_WGS_1984"',
#                   'DATUM["D_WGS_1984"',
#                   'SPHEROID["WGS_1984",6378137.0,298.257223563]]',
#                   'PRIMEM["Greenwich",0.0]',
#                   'UNIT["Degree",0.0174532925199433]]',
#                   'PROJECTION["Lambert_Azimuthal_Equal_Area"]',
#                   'PARAMETER["False_Easting",0.0]',
#                   'PARAMETER["False_Northing",0.0]',
#                   'PARAMETER["Central_Meridian",-120]',
#                   'PARAMETER["Latitude_Of_Origin",12.55]',
#                   'UNIT["Meter",1.0]]',
#                   sep = ',')

# map<-ne_countries(returnclass = "sf", continent = "europe")
# ggplot() + 
#   geom_sf(data = map)+
#   coord_sf(crs = domainCRS)
