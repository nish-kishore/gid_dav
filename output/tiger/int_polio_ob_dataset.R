# Outbreak Table - Replication for GPEI and R4 

source("./reference/obx_packages_01.R", local = T)

# Load 2016 Data for outbreaks 

raw.data <- get_all_polio_data(size = "medium")


# Build outbreak dataset 
data <- raw.data$pos %>% 
          filter(measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3"))

# Three positives dont have one
table(is.na(data$emergencegroup))

miss_emg <- data %>% filter(is.na(emergencegroup)==T)
# 
table <- data %>% 
           group_by(data)
