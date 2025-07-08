# Project for all SERO Type Detections in a Given Region - with Site Name / Cases 
source("./reference/obx_packages_01.R", local = T)

# Project number 35 
# Last 12 months of all euro detecitons by sites 


# Inputs 
region <- c("EURO") 

# Months from onset date 
mo <- 12


# Load data 
if (exists("raw.data") == TRUE){
  cli::cli_alert("Core data already loaded")
}else{
  raw.data <- get_all_polio_data(size = "small")
}

datt_task_id <- "R35"
vno <- "3"
sub_folder <- "3. Figures"
fig_name <- paste("euro_pv12m_", today(), "-v", vno, ".jpeg",  sep = "")
out_put <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")


# Get Sharepoitn site 
get_sharepoint_site(
  site_url = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  tenant = Sys.getenv("CLIMICROSOFT365_TENANT", "common"),
  app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
  scopes = c("Group.ReadWrite.All", "Directory.Read.All", "Sites.ReadWrite.All",
             "Sites.Manage.All"),
  token = NULL)


# Dates to be updated 
date_end <- raw.data$metadata$download_time
date_start <- floor_date(date_end %m-% months(mo), unit = "months")
v_type <- c("cVDPV 1", "cVDPV 2", "cVDPV 3", "VDPV 1", "VDPV 2", "VDPV 3")


data_p <- raw.data$pos %>% 
  # mutate(
    # test = case_when(
    #   place.admin.0 == "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM" ~ "Palestine",
    #   place.admin.0 == "IRAN (ISLAMIC REPUBLIC OF)" ~ "Iran", 
    #   place.admin.0 == "DEMOCRATIC REPUBLIC OF THE CONGO" ~"DR Congo",
    #   place.admin.0 == "UNITED REPUBLIC OF TANZANIA" ~ "Tanzania",
    #   place.admin.0 == "SYRIAN ARAB REPUBLIC" ~ "Syria",
    #   TRUE ~ place.admin.0),
  
  filter(whoregion %in% c("EURO") & 
           dateonset >= date_start & dateonset <= date_end & 
           measurement %in% v_type)  %>% 
  arrange(measurement, place.admin.0, place.admin.1, site.name, dateonset) %>% 
  select(measurement, epid, place.admin.0, place.admin.1, place.admin.2, site.name, source,
         emergencegroup, dateonset, report_date, admin0whocode) %>% 
  mutate(p0 = str_to_sentence(place.admin.0), 
         p1 = str_to_sentence(place.admin.1), 
         p2 = str_to_sentence(place.admin.2), 
         sn = str_to_lower(site.name), 
         p2_clean = case_when(
           p2 == "South east (england)" ~ "Souteast",
           p2 == "Yorkshire and the humber" ~ "Yorkshire",
           TRUE ~ p2),
         sn_clean = case_when(
           sn == "jerusalem-kidron"  ~ "kidron", 
           sn == "germany/berlin" ~ "berlin", 
           sn == "germany/hamburg" ~ "hamburg",
           sn == "germany/sachsen" ~ "sachsen",
           sn == "rishon lezion " ~ "rishon-lezion",
           sn == "jerusalem og (east)" ~ "og-east",
           sn == "rzeszow city" ~ "rzeszow-city",
           sn == "leeds knostrop sewage treatment works" ~ "leeds-knostrop",
           sn == "london beckton site " ~ "beckton",
           TRUE ~ sn),
         adm0_clean = case_when(
           admin0whocode == "UNK" ~ "GBR",
           admin0whocode == "DEU" ~ "GER",
           admin0whocode == "SPA" ~ "SPN",
           TRUE ~ admin0whocode),
         
         site_clean = paste(adm0_clean, " / ", p2_clean, " / ", sn_clean, sep = ""), 
         site_clean = factor(
           site_clean, 
           levels=rev(sort(unique(site_clean))), 
           ordered=TRUE), 
         source_mod = case_when(
           source == "AFP" ~ "AFP",
           source == "ENV" ~ "ES",
           source %in% c("Community", "Contact",
                         "Healthy", "Other", "iVDPV") ~ "Other"), 
         source_mod = factor(source_mod, levels = c("AFP", "Other", "ES"), ordered = T), 
         emg_group = case_when(
           measurement %in% c("VDPV 1", "VDPV 2", "VDPV 3") ~ "VDPVs", 
           TRUE ~ emergencegroup), 
         emg_group = factor(
           emg_group, 
           levels = c("NIE-ZAS-1", "SUD-RED-1", "VDPVs"), 
           ordered = T))
         
           
           
  
           
  

g1 <- ggplot(data_p, aes(x=site_clean, y = dateonset)) +
  geom_point(data=data_p, aes(
    shape = measurement,
    color = emg_group),
    alpha = 0.6, 
    size = 3.5, show.legend = T) +
  coord_flip() +
  xlab("Country / District or City / ES Site Name") +
  ylab("Date of Sample Collection")  +
  scale_color_manual(name = "Emgergence Color:",
                     values = c(
                       "NIE-ZAS-1" = "#D16103",
                       "SUD-RED-1" = "#4E84C4",
                       "VDPVs" ="#000000"), drop = FALSE) +
  scale_shape_manual(name = "Virus Type:",
                     values = c(
                       "VDPV 1" =  2,
                       "VDPV 2" =  1,
                       "VDPV 3" =  0,
                       "cVDPV 2" = 16,
                       "cVDPV 1" = 17,
                       "cVDPV 3" = 18), 
                     labels = c( "VDPV 1" =  "VDPV1",
                                 "VDPV 2" =  "VDPV2",
                                 "VDPV 3" =  "VDPV3",
                                 "cVDPV 2" = "cVDPV2",
                                 "cVDPV 1" = "cVDPV1",
                                 "cVDPV 3" = "cVDPV3"), drop = FALSE) + 
  theme_bw() +
  scale_y_date(date_breaks = "2 months",
               date_labels = "%b '%y",
               limits = c((ymd(date_start) %m-% months(1)), ymd(date_end)))  +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text =  element_text(size = 10),
    legend.position = "bottom",
    legend.key = element_blank(),
    legend.direction = "horizontal",
    legend.spacing.x = unit(0.05, 'cm'),
    legend.text = element_text(size = 10),
    plot.title.position = "plot",
    legend.title = element_text(size = 12,
                              face = "bold"),
    plot.subtitle = element_text(size = 8)) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2, override.aes = list(size=3))) +
  labs(
  caption = paste0("ES: Environmental Surveillance, cVDPV: circulating Vaccine Derived Poliovirus\nProduced by CDC-GHC-GID-PEB \nData available through GPEI as of ", raw.data$metadata$download_time)) 
  

print(g1)

ggsave(filename = out_put, plot = g1, height = 4, width = 8, scale = 1.5, dpi = 300, bg = 'white')


upload_to_sharepoint(
  file_to_upload = out_put,  # Writes to temp directory
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")
