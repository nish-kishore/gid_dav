## CDC Ready - Country Specific Pages 
## 



source("./special_projects/obx_unit/obx_packages_01.R", local = T)
library(aweek)

# Get data 
raw.data <- get_all_polio_data()

# Set Inputs 

virus_type <- c("WILD 1", "cVDPV 1", "cVDPV 2", "cVDPV 3")
v2 <- c("WPV1", "cVDPV1", "cVDPV2", "cVDPV3")

end_date <- floor_date(raw.data$metadata$download_time, "week", week_start = 2) 
start_date <- floor_date(end_date %m-% months(13), "week", week_start = 7)
s_source <- c("AFP", "ENV")



# Get list of countries 
ctry_all <- raw.data$pos %>%
               filter(
                 dateonset >= start_date & 
                   dateonset <= end_date & 
                   source %in% s_source  &
                   measurement %in% virus_type) %>% 
            distinct(place.admin.0)




  