library(sirfunctions)
source("./reference/obx_packages_01.R", local = T)

raw.data <- get_all_polio_data(size = "medium")  

# User for the Data Update
user <- "JPB (QDZ1)"
v_type = c("cVDPV 1", "cVDPV 2", "cVDPV 3")
# Create Database 
ob_data <- raw.data$pos %>%
  mutate(source_cat = case_when(
    source == "AFP" ~ "AFP",
    source %in% c("Contact", "Community", "Healthy", "Other") ~ "Other", 
    source == "ENV" ~ "ES"),
    emergence_group2 = case_when(
      is.na(emergencegroup)==F ~ emergencegroup, 
      is.na(emergencegroup)==T & 
        measurement == "VDPV 1" ~ "VDPV1", 
      is.na(emergencegroup)==T & 
        measurement == "VDPV 2" ~ "VDPV2", 
      is.na(emergencegroup)==T & 
        measurement == "VDPV 3" ~ "VDPV3")) %>%
  filter(measurement != "WILD 1" & 
           source_cat %in% c("AFP", "Other", "ES")) %>% 
  distinct(epid, measurement, .keep_all = T) 


ob_table <- ob_data %>%
  arrange(place.admin.0, emergence_group2, dateonset) %>%
  group_by(place.admin.0,  measurement, emergence_group2) %>% 
  summarise(total_afp = sum(source == 'AFP', na.rm = T),
            afp_first = first(dateonset[source == "AFP"]),
            afp_last = last(dateonset[source == "AFP"]), 
            total_other = sum(source_cat == 'Other', na.rm = T),
            other_first = first(dateonset[source_cat == "Other"]),
            other_last = last(dateonset[source_cat == "Other"]),
            total_es = sum(source_cat == 'ES', na.rm = T),
            es_first = first(dateonset[source_cat == "ES"]),
            es_last = last(dateonset[source_cat == "ES"])) %>%
  filter(is.na(emergence_group2)==F)

ob_table <- ob_data %>%
  arrange(place.admin.0, emergence_group2, dateonset) %>% 
  group_by(place.admin.0, emergence_group2) %>% 
  summarise(first_virus =first(dateonset),
            most_recent = last(dateonset)) %>% 
  left_join(ob_table, ., by = c("place.admin.0", "emergence_group2"))


# Load in Outbreak Dataset here
# updated as of 17 March 2025 -> pull from WHO deck or raw.data once processed by Nick. 


ob_dataset <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_raw/outbreaks_250317.rds")


ob_dataset <- ob_dataset %>%
  rename_all(~str_replace_all(., "\\s+", "")) %>% 
  #Remove summary data 
  slice(., 1:(n() - 3)) %>% 
  #Remove WPV1 OB 
  filter(SeroType != "WILD1") %>% 
  #Match to CORE 
  mutate(Country = ifelse(str_detect(Country, "IVOIRE"),"COTE D IVOIRE", Country)) %>% 
  select(Country, EmergenceGroup, OutbreakNoti.Date)
# OutbreakNoti.Date = if_else(is.na(OutbreakNoti.Date)==T, FirstVirus, OutbreakNoti.Date))


ob_all <- left_join(ob_table, ob_dataset, by = c("place.admin.0"="Country", "emergence_group2"="EmergenceGroup"))

meta_data <- data.frame(number_data_update = Sys.Date(), 
                        gpei_data_update = raw.data$metadata$processed_time, 
                        user = user)


ob_sum_table <- list(ob_data = ob_all, metadata = meta_data)

obs <- ob_sum_table$ob_data %>% 
           filter(measurement %in% v_type) %>%
          filter(!emergence_group2 == "CHN-SIC-1 ")

obs <- obs %>% filter(is.na(OutbreakNoti.Date)==T)

if(nrow(obs)==0){
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/pv_ob_table.rds", obj = ob_sum_table)
}else{
cli_alert("New Outbreak in Dataset - Need new pass of the OB Notification Date to replace line 53")
}

rm(obs, ob_dataset, meta_data, ob_all, ob_data, ob_sum_table, ob_table)