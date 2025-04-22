# Lighting Bug Map 


# Notes 
# Need Warning if it crosses data threshold in small data set 
# Outbreak vs last vrius detection date 
# Serotype specific <- Co-Infected
# Enviromental <- Everthing or AFP -> ES onyl add later 
# Data labels 


# Need to work backwards to have 2019 


# Load Packages 
source("./reference/obx_packages_01.R", local = T)



# Initial Inputs 
date_end <- as_date("2025-01-01")
date_start  <- as_date("2018-01-01")
analysis_start <- floor_date(date_start %m-% months(6), "month")

# Load Data
if (year(analysis_start) >= 2019){
  raw.data <- get_all_polio_data(size = "small")
}else if(year(analysis_start) >= 2016 & year(analysis_start) < 2019){
  raw.data <- get_all_polio_data(size = "medium")
}else if (year(analysis_start) >= 2001 & year(analysis_start) < 2016){
  raw.data <- get_all_polio_data(size = "large")
}else{
  print("No Data Loaded")
}
  

# 
# if (exists("raw.data") == TRUE){
#   cli::cli_alert("Core data already loaded")
# }else{
#   raw.data <- get_all_polio_data(size = "small")
# }

# Load Inputs 
include_env <- T
sero_type <- c("cVDPV 1", "cVDPV 2", "cVDPV 3")
analysis_end = floor_date(raw.data$metadata$download_time, "week", week_start = 1) %m+% days(1)
preview_map <- T 
include_epi <- T 
color_epi_curve <- T 

s_w1 <- paste0("W",as.character(epiweek(date_start)), sub="")
w_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")

datt_task_id <- "R28"
sub_folder <- "3. Figures"
fig_name <- paste("v2_lightbug_", date_start, "_", date_end,".gif" , sep = "")
out_gif <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")


create_lightbug_gif <- function(
    include_env,
    sero_type,
    analysis_end, 
    date_end,
    date_start,  
    analysis_start,
    preview_map, 
    include_epi, 
    color_epi_curve,
    out_gif) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop('Package "magick" must be installed to use this function.',
         .call = FALSE
    )
  }
  

  
  cli::cli_process_start("Setting up data structures")

# Work on initial map 
  
# set up data structures
  virus_table <- raw.data$pos |>
    dplyr::filter(measurement %in% sero_type & 
                    dateonset >= analysis_start &  # Switch to full data set 
                    dateonset < analysis_end) |>
    dplyr::select(place.admin.0, dateonset, epid, source,  emergencegroup, measurement)
  

# Add in section here to select source types
  if (!include_env) {
    cli::cli_alert("Subsetting to only AFP and other detections")
    virus_table <- virus_table |>
      dplyr::filter(source %in% c("AFP", "Community", "Contact", "Healthy"))
  } else {
    cli::cli_alert("Subsetting to all positive detections")
    virus_table <- virus_table |>
      dplyr::filter(source %in% c("AFP", "ENV", "Community", "Contact", "Healthy"))
  }
  
# Group Months   min_date <- monthly_pos |>
  
date_seq <- seq(date_start, date_end, by = "month")

# Get Data Ready for Epi Curve 
# Create ataframe of the number of outbreaks for the date of analysis below to match 

epi_table <- tibble()

for (date_of_eval in date_seq){
  date_of_eval <- lubridate::as_date(date_of_eval)
  ob_months <- virus_table %>% 
    # dplyr::mutate(month_date = lubridate::floor_date(dateonset, unit = "month")) %>%
    dplyr::filter(dateonset >= date_of_eval %m-% months(6) & 
             dateonset < date_of_eval) %>% 
    dplyr::group_by(measurement, place.admin.0) %>% 
    dplyr::count() %>% 
    dplyr::group_by(measurement) %>% 
    dplyr::summarise(
      month_date = date_of_eval,
      n = n())

  epi_table <- bind_rows(epi_table, ob_months)
    
}  


epi_table <- epi_table %>% mutate(measurement = gsub(" ", "", measurement), 
                                  measurement = factor(measurement, levels = c("cVDPV3", "cVDPV2", "cVDPV1"))) 


cli::cli_process_done()
  
cli::cli_h1(paste0("Generating individual images - Start Date: ", date_start, " - End Date: ", date_end))
  for (date_of_eval in date_seq) {
    date_of_eval <- lubridate::as_date(date_of_eval)
    #Test Code 
    # date_of_eval <- as_date("2023-08-01")
    cli::cli_process_start(date_of_eval)
    
    # create temp directory
    out_file <- paste0(tempdir(), "/", date_of_eval, ".png")
    
# Might have to include some IF/AND Statements here: 
    
    # Filter Data 
    map_data <- virus_table %>% 
            filter(dateonset >= date_of_eval %m-% months(6) & 
                   dateonset < date_of_eval) %>% 
            group_by(place.admin.0, measurement) %>% 
            count() 
    
# Add in proxy data to stabilze output over time:
    df <- data.frame(
      place.admin.0 = c(NA_character_, NA_character_, NA_character_),
      measurement = c("cVDPV 1", "cVDPV 2", "cVDPV 3"),
      n = c(-99, -99, -99))
    
    
    map_data <- bind_rows(map_data, df)
    
    map_data <- map_data %>% 
            pivot_wider(names_from = measurement, values_from = n) %>% 
            mutate(
              ob_status = case_when(
                # is.na(`cVDPV 1`) == F  & is.na(`cVDPV 2`) == F & is.na(`cVDPV 3`) == F ~ "cVDPV1 & cVDPV2 & cVDPV3",
                is.na(`cVDPV 1`) == F  & is.na(`cVDPV 2`) == F~ "cVDPV1 & cVDPV2",
                is.na(`cVDPV 1`) == F  & is.na(`cVDPV 3`) == F~ "cVDPV1 & cVDPV3",
                is.na(`cVDPV 2`) == F  & is.na(`cVDPV 3`) == F~ "cVDPV2 & cVDPV3", 
                is.na(`cVDPV 2`) == F ~ "cVDPV2", 
                is.na(`cVDPV 1`) == F ~ "cVDPV1",
                is.na(`cVDPV 3`) == F ~ "cVDPV3"),
              ob_status = factor(ob_status, levels = c("cVDPV1", "cVDPV2", "cVDPV3", 
                                                        "cVDPV1 & cVDPV2", "cVDPV1 & cVDPV3", "cVDPV2 & cVDPV3"))) %>% 
              filter(is.na(place.admin.0) == F) # removes proxy data from data set 


    # Number of Countries for Figure 
    no_ctry <- nrow(map_data)
    
cli::cli_process_start("Parsing spatial information")
    
    ctry_data <- raw.data$global.ctry %>% 
                   filter(ENDDATE == "9999-12-31")
    
    ctry <- left_join(ctry_data, map_data, by = c("ADM0_NAME"="place.admin.0"))

cli::cli_process_done()
if(!include_epi){  
map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = ctry, fill = "#FFFFFF") +
  ggplot2::geom_sf(data = ctry, aes(fill = ob_status),lwd = 0.4,
                      show.legend = TRUE) +
  ggplot2::scale_fill_manual(name = "Outbreak\nType:", 
                              values = c("cVDPV1" = "#104E8B", 
                                         "cVDPV2" = "#EE2C2C", 
                                         "cVDPV3" = "#EEC900", 
                                         "cVDPV1 & cVDPV2" = "#68228B", 
                                         "cVDPV1 & cVDPV3" = "#556B2F",
                                         "cVDPV2 & cVDPV3" = "#FF7F00"),
                             na.translate = F,
                             drop = FALSE) +
  ggplot2::theme_bw() +
  guides(fill = guide_legend(ncol = 2)) +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = element_text(face="bold", size = 12),
                 plot.title = element_text(hjust = 0.5, face="bold", size = 12),
                 axis.title=element_blank(), 
                 axis.text=element_blank(), 
                 axis.ticks=element_blank(), 
                 panel.background = element_rect(fill = "#E3EBFF")) + 
  labs(title = paste0(" Polio Outbreaks (OBs)- ",
                      lubridate::year(date_of_eval),
                      " ",
                      lubridate::month(date_of_eval, label = T),
                      ": ",
                      no_ctry,
                      " countries"))
out_plot <- map 

}else{
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = ctry, fill = "#FFFFFF") +
    ggplot2::geom_sf(data = ctry, aes(fill = ob_status),lwd = 0.4,
                     show.legend = TRUE) +
    ggplot2::scale_fill_manual(name = "Outbreak\nType:", 
                               values = c("cVDPV1" = "#104E8B", 
                                          "cVDPV2" = "#EE2C2C", 
                                          "cVDPV3" = "#EEC900", 
                                          "cVDPV1 & cVDPV2" = "#68228B", 
                                          "cVDPV1 & cVDPV3" = "#556B2F",
                                          "cVDPV2 & cVDPV3" = "#FF7F00"),
                               na.translate = F,
                               drop = FALSE) +
    
    ggplot2::theme_bw() +
    guides(fill = guide_legend(ncol = 2)) +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = element_text(face="bold", size = 12),
                   plot.title = element_text(hjust = 0.5, face="bold", size = 12),
                   axis.title=element_blank(), 
                   axis.text=element_blank(), 
                   axis.ticks=element_blank(), 
                   panel.background = element_rect(fill = "#E3EBFF")) 
}

# Preview Map 
if (!preview_map) {
  cli::cli_alert("No map preview printed")
}else{
print(map)
}


# Create figure for both EPI Curve and Non-Epicure 

if(!include_epi){
  print("No epi curve included")
}else{
# Make epi curve 
if(!color_epi_curve){

epi_curve <-
  ggplot2::ggplot() +
  ggplot2::geom_bar(data = epi_table, ggplot2::aes(x = month_date, y = n), stat = "identity") +
  ggplot2::geom_vline(xintercept = date_of_eval, color = "red", linetype = 2) +
  ggplot2::labs(x = "Month", y = paste("No. of OBs")) +
  ggplot2::scale_x_date(breaks = "year", labels=date_format("%Y")) +
  ggplot2::scale_y_continuous(breaks = seq(0,35,10),limits = c(0,35)) + 
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = "none", 
                 axis.title = element_text(face = "bold"))


}else{

  epi_curve <-
    ggplot2::ggplot() +
    ggplot2::geom_bar(data = epi_table, ggplot2::aes(x = month_date, y = n, fill = measurement), stat = "identity") +
    ggplot2::geom_vline(xintercept = date_of_eval, color = "grey20", linetype = 2) +
    ggplot2::labs(x = "Month", y = paste("No. of OBs")) +
    ggplot2::scale_fill_manual(
                               values = c("cVDPV1" = "#104E8B", 
                                          "cVDPV2" = "#EE2C2C", 
                                          "cVDPV3" = "#EEC900")) +
    ggplot2::scale_y_continuous(breaks = seq(0,35,10),limits = c(0,35)) + 
    ggplot2::scale_x_date(breaks = "year", labels=date_format("%Y")) +
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = "none", 
                   axis.title = element_text(face = "bold"))
}
  out_plot <- ggpubr::ggarrange(epi_curve, map, ncol = 1, heights = c(1, 2)) |>
    ggpubr::annotate_figure(top = text_grob(paste0(" Polio Outbreaks (OBs) - ",
                                         lubridate::year(date_of_eval),
                                         " ",
                                         lubridate::month(date_of_eval, label = T),
                                         ": ",
                                         no_ctry,
                                         " countries"), face = "bold"))
                            
}

print(out_plot)

ggplot2::ggsave(out_file, plot = out_plot, dpi = 300, height = 8, width = 6, bg = "white")
cli::cli_process_done()
  }

cli::cli_h1("Generating Gif")

cli::cli_process_start("Reading images")
## list file names and read in
imgs <- list.files(tempdir(), pattern = "-01.png", full.names = T)
img_list <- lapply(imgs, magick::image_read)

cli::cli_process_done()

cli::cli_process_start("Joining images")
## join the images together
img_joined <- magick::image_join(img_list)
cli::cli_process_done()

cli::cli_process_start("Generating gif")
## animate at 2 frames per second
img_animated <- magick::image_animate(img_joined, fps = 4)

## save to disk
magick::image_write(
  image = img_animated,
  path = out_gif
)
cli::cli_process_done()
}

create_lightbug_gif( include_env,
                     sero_type,
                     analysis_end, 
                     date_end,
                     date_start,  
                     analysis_start,
                     preview_map, 
                     include_epi, 
                     color_epi_curve,
                     out_gif)


upload_to_sharepoint(
  file_to_upload = out_gif,  # Writes to temp directory
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")


