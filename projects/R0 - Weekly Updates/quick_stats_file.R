# Quick Check Scirpt 
library(here)
source(here("reference/obx_packages_01.R"))

ctry <- c("NIGERIA")
prov <- c("YOBE")
end_date <- c(Sys.Date()) 
start_date <- end_date %m-% months(13)

virus_types <- c("cVDPV 2")

# Load Data 
if (exists("raw.data") == TRUE){
  cli::cli_alert("Core data already loaded")
}else{
  raw.data <- get_all_polio_data(size = "small")
}

check <- raw.data$pos %>% 
             filter(place.admin.0 %in% ctry & 
                    dateonset >= start_date & 
                    dateonset <= end_date & 
                    measurement %in% virus_types) %>% 
         distinct(epid, measurement, .keep_all = T) %>% 
         select(epid, dateonset, place.admin.0, place.admin.1, place.admin.2, source, measurement, report_date)


# Quick counts 

virus_counts <- raw.data$pos %>% 
  filter(place.admin.0 %in% ctry & 
           place.admin.1 %in% prov &
           dateonset >= start_date & 
           dateonset <= end_date & 
           measurement %in% virus_types) %>%         
distinct(epid, measurement, .keep_all = T) %>% 
  select(epid, dateonset, place.admin.0, place.admin.1, place.admin.2, source, measurement, emergencegroup, report_date)



# SIA Check 
v_choice <- c("IPV, bOPV")

sia_check <- raw.data$sia %>%
               filter(place.admin.0 %in% ctry &
                      place.admin.1 == "YOBE" &
                      activity.start.date >= start_date &
                      activity.start.date <= end_date) %>% 
               distinct(sia.sub.activity.code, .keep_all = T)
  
# SIA Campaign Check 
sia_camp_check <- raw.data$sia %>% filter(sia.code == "ZMB-2024-001")


ctry_23 <- raw.data$pos %>% 
          filter(yronset == 2023 & 
                 measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3")) %>% 
          distinct(epid, measurement, .keep_all = T) %>% 
          group_by(place.admin.0) %>% 
          count()

ctry_24<- raw.data$pos %>% 
  filter(yronset == 2024 & 
           measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3")) %>% 
  distinct(epid, measurement, .keep_all = T) %>% 
  group_by(place.admin.0) %>% 
  count()

# New Country 

first_ob <- ob_d %>% 
              arrange(place.admin.0, OutbreakNoti.Date) %>%
              group_by(place.admin.0) %>%
              summarise(first_ob = first(OutbreakNoti.Date))


