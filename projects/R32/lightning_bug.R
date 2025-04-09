# Lighting Bug Map 


# Notes 
# Need Warning if it crosses data threshold in small data set 
# Outbreak vs last vrius detection date 
# Serotype specific 
# Enviromental 
# Data labels 


# Need to work backwards to have 2019 


# Load Packages 
source("./reference/obx_packages_01.R", local = T)

# Load Data 
if (exists("raw.data") == TRUE){
  cli::cli_alert("Core data already loaded")
}else{
  raw.data <- get_all_polio_data(size = "small")
}

# Load Inputs 
include_env <- T
# cumulative <- F #
# cumulative_map <- T
sero_type <- c("cVDPV 1", "cVPDV 2", "cVDPV 2")

analysis_end = floor_date(raw.data$metadata$download_time, "week", week_start = 1) %m+% days(1)
# date_end <- ceiling_date(analysis_end %m-% months(1), "month") # Current Analysis 
date_end <- as_date("2024-06-01")
date_start  <- as_date("2024-01-01")
analysis_start <- floor_date(date_start %m-% months(13), "month")



s_w1 <- paste0("W",as.character(epiweek(date_start)), sub="")
w_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")

datt_task_id <- "R32"
sub_folder <- "3. Figures"
fig_name <- paste("lightbug_", date_start, "_", date_end,".gif" , sep = "")
out_gif <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")


create_emergence_group_gif <- function(
    # include_env,
    sero_type,
    analysis_end, 
    date_end,
    date_start,  
    analysis_start,
    # include_env = T,
    # cumulative_map = T,
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

  
# Group Months   min_date <- monthly_pos |>
  
date_seq <- seq(date_start, date_end, by = "month")

cli::cli_process_done()
  
cli::cli_h1(paste0("Generating individual images - Start Date: ", date_start, " - End Date: ", analysis_end))
  for (date_of_eval in date_seq) {
    date_of_eval <- lubridate::as_date(date_of_eval)
    
    cli::cli_process_start(date_of_eval)
    
    # create temp directory
    out_file <- paste0(tempdir(), "/", date_of_eval, ".png")
    
    
    # Filter Data 
    map_data <- virus_table %>% 
            filter(dateonset >= date_of_eval %m-% months(13) & 
                   dateonset < date_of_eval) %>% 
            group_by(place.admin.0) %>% 
            summarise(impact_ctry = "Y")

    # Number of Countries 
    no_ctry <- nrow(map_data)

cli::cli_process_start("Parsing spatial information")
    
    ctry_data <- raw.data$global.ctry %>% 
                   filter(ENDDATE == "9999-12-31")
    
    ctry <- left_join(ctry_data, map_data, by = c("ADM0_NAME"="place.admin.0"))

cli::cli_process_done()
    
map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = ctry, aes(fill = impact_ctry),lwd = 0.4,
                      show.legend = TRUE) +
  ggplot2::scale_fill_manual(name = "Ctry w/ PV Detection:", 
                              values = c("Y" = "#FF0000"),
                              na.value = "#FFFFFF",
                              drop = FALSE) +
  
  # ggplot2::geom_sf(data = plot_data, ggplot2::aes(fill = n_det_cat), show.legend = T) +
  # ggplot2::scale_fill_brewer(palette = "YlOrRd", drop = F, na.translate = F) +
 
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = element_text(face="bold", size = 12),
                 plot.title = element_text(hjust = 0.5, face="bold", size = 12),
                 axis.title=element_blank(), 
                 axis.text=element_blank(), 
                 axis.ticks=element_blank(), 
                 panel.background = element_rect(fill = "#E3EBFF")) + 
  labs(title = paste0("Spread of cVDPVs",
                      " as of ",
                      lubridate::year(date_of_eval),
                      " ",
                      lubridate::month(date_of_eval, label = T),
                      ": ",
                      no_ctry,
                      " countries"))

print(map)

ggplot2::ggsave(out_file, plot = map, dpi = 300, height = 8, width = 6, bg = "white")
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



