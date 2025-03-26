# Load in Packages 
source("./reference/obx_packages_01.R", local = T)
library(ggarrow)
# Load in Data 
map_ref<- sirfunctions::edav_io(io = "read", file_loc = "Data/orpg/mapref.table.rds")

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
datt_task_id <- "R19"
fig_name <- paste("zas1_traverlers_2b1_",e_w1,"_", format(today(), "%y%m%d"),".png", sep="")
temp_path <- file.path(tempdir(), fig_name)
sub_folder <- "3. Figures"
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")


# Nigeria
nig <- raw.data$pos %>% filter(place.admin.0 == "NIGERIA" & 
                               measurement == "cVDPV 2" & 
                               yronset >= 2021 & yronset <= 2024)

table(nig$yronset, nig$emergencegroup)
table(nig$yronset, nig$emergencegroup)


map1 <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = raw.data$global.prov %>% filter(ADM0_NAME == "NIGERIA"),  color = "grey20", lwd = 0.6, fill = "lightyellow") +
        ggplot2::geom_point(data = nig %>% filter(source=="ENV"), aes(x = as.numeric(longitude), 
                                                                      y = as.numeric(latitude), 
                                                                      color=emergencegroup),
                                                                      shape=15, show.legend = F) + 
        ggplot2::geom_point(data = nig %>% filter(source=="AFP" & emergencegroup == "NIE-ZAS-1"), aes(x = as.numeric(longitude), 
                                                                y = as.numeric(latitude), 
                                                                color=emergencegroup),
                      shape=16) +
        ggplot2::geom_point(data = nig %>% filter(source=="AFP" & emergencegroup != "NIE-ZAS-1"), aes(x = as.numeric(longitude), 
                                                                                                y = as.numeric(latitude), 
                                                                                                color=emergencegroup),
                      shape=16) + 
        scale_color_manual(name = "Virus Strains:", values = c("darkgreen", "lightblue", 
                                                               "limegreen", "orange", "blue", 
                                                               "purple", "black","red")) +
        theme_bw() +
        theme(legend.position = "bottom",
        legend.title = element_text(face="bold", size = 12),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_markdown(hjust = 0.5, face="bold", size = 12), 
        strip.placement = "outside",
        strip.background = element_blank()) + 
        facet_wrap(~yronset, nrow=1) 
        
print(map1)

ggsave(plot = map1, filename = "./output/zas1_2021-2024.jpeg", height = 4, width = 12, unit = "in", dpi = 300)

