# Emergence Graph of the spread of Nigeria Emergences Across 
# Created: 15 Apr 25 
# Updated:

# Load Packages 
source("./reference/obx_packages_01.R", local = T)

# Load Data 
if (exists("raw.data") == TRUE){
  cli::cli_alert("Core data already loaded")
}else{
  raw.data <- get_all_polio_data(size = "medium")
}

# Load Inputs 
include_env <- T
cumulative_map <- F
include_epi <- T
color_epi_curve <- T
date_start = as_date("2018-01-01")
date_end = floor_date(raw.data$metadata$download_time, "week", week_start = 1) %m+% days(1)
emergence_group_root <- "NIE-"
s_w1 <- paste0("W",as.character(epiweek(date_start)), sub="")
w_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")

datt_task_id <- "R33"
sub_folder <- "3. Figures"
fig_name <- paste(emergence_group_root, "_", date_start, "_", date_end,".gif" , sep = "")
out_gif <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")

emg_colors <- c(
                "NIE-BOS-1" = "#BCD2EE",
                "NIE-JIS-1" = "#104E8B", 
                "NIE-KBS-1" = "#FFD39B", 
                "NIE-KGS-1" = "#00FFFF", 
                "NIE-KGS-2" = "#B03060", 
                "NIE-KTS-1" = "#68228b",  
                # "NIE-SOS-2" = "#556B2F", 
                # "cVDPV-a" =   "#FF7F00",
                "NIE-SOS-3" = "#6959CD",
                "NIE-SOS-4" = "#FF7F24", 
                "NIE-SOS-5" = "#FFC125",
                "NIE-SOS-6" = "#87CEFA",
                "NIE-SOS-7" = "#6B8E23",
                "NIE-SOS-8" = "#D15FEE",
                "NIE-ZAS-1" = "#F5191C",
                "NIE-YBS-1" = "#05998c",
                "NIE-YBS-2" = "#EE9572")

create_emergence_group_gif <- function(
    emergence_group_root,
    include_env = T,
    cumulative_map = T,
    date_start, 
    date_end,
    include_epi, 
    color_epi_curve,
    out_gif) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop('Package "magick" must be installed to use this function.',
         .call = FALSE
    )
  }
  
  cli::cli_process_start("Setting up data structures")
  
  # set up data structures
  emergence_group_pos <- raw.data$pos |>
    dplyr::filter(str_detect(emergencegroup, emergence_group_root) &
                    dateonset >= date_start &
                    dateonset < date_end) |>
    dplyr::select(place.admin.0, adm0guid, admin2guid, dateonset, epid, source, latitude, longitude, emergencegroup) %>% 
    mutate(eg2 = factor(emergencegroup, levels = c(
                                                   "NIE-BOS-1",
                                                   "NIE-JIS-1", 
                                                   "NIE-KBS-1", 
                                                   "NIE-KGS-1", 
                                                   "NIE-KGS-2", 
                                                   "NIE-KTS-1",  
                                                   "NIE-SOS-3",
                                                   "NIE-SOS-4", 
                                                   "NIE-SOS-5",
                                                   "NIE-SOS-6",
                                                   "NIE-SOS-7",
                                                   "NIE-SOS-8",
                                                   "NIE-ZAS-1",
                                                   "NIE-YBS-1",
                                                   "NIE-YBS-2" )))
  
  if (!include_env) {
    cli::cli_alert("Subsetting to only AFP Surveillance")
    emergence_group_pos <- emergence_group_pos |>
      dplyr::filter(source %in% c("AFP", "Community", "Contact", "Healthy"))
  } else {
    cli::cli_alert("Subsetting to only ALL detections")
    emergence_group_pos <- emergence_group_pos |>
      dplyr::filter(source %in% c("AFP", "Community", "Contact", "Healthy", "ENV"))
  }
  
  # group data by month and emergence group type 
  monthly_pos <- emergence_group_pos |>
    dplyr::filter(source == "AFP") |>
    dplyr::mutate(month_date = lubridate::floor_date(dateonset, unit = "month")) |>
    dplyr::group_by(month_date, emergencegroup) |>
    dplyr::summarise(n_det = dplyr::n(), .groups = "drop") %>% 
    mutate(eg2 = factor(emergencegroup, levels = c(
                                                   "NIE-BOS-1",
                                                   "NIE-KBS-1", 
                                                   "NIE-KGS-1", 
                                                   "NIE-KGS-2", 
                                                   "NIE-KTS-1",  
                                                   "NIE-SOS-3",
                                                   "NIE-SOS-4", 
                                                   "NIE-SOS-5",
                                                   "NIE-SOS-6",
                                                   "NIE-SOS-7",
                                                   "NIE-SOS-8",
                                                   "NIE-YBS-1",
                                                   "NIE-YBS-2",
                                                   "NIE-JIS-1",
                                                   "NIE-ZAS-1")))
  
  cli::cli_process_done()
  
  min_date <- monthly_pos |>
    dplyr::pull(month_date) |>
    min()
  
  max_date <- monthly_pos |>
    dplyr::pull(month_date) |>
    max()
  
  date_seq <- seq(min_date, max_date, by = "month")
  
  # if (cumulative) {
  #   cli::cli_alert_info("Producing cumulative results")
  #   monthly_pos <- tidyr::expand_grid(
  #     adm0guid = unique(monthly_pos$adm0guid),
  #     admin2guid = unique(monthly_pos$admin2guid),
  #     month_date = date_seq
  #   ) |>
  #     left_join(monthly_pos, by = c("adm0guid", "admin2guid", "month_date")) |>
  #     dplyr::group_by(adm0guid, admin2guid) |>
  #     dplyr::arrange(month_date) |>
  #     dplyr::mutate(n_det = sum(n_det, dplyr::lag(n_det, default = 0), na.rm = T))
  # }
  # 
  cli::cli_process_start("Parsing spatial information")
  countries_to_include <- raw.data$global.ctry %>% 
                                 filter(WHO_REGION %in% c("AFRO", "EMRO", "EURO", "SEARO", "WPRO") & 
                                          ENDDATE == as_date("9999-12-31"))
  
  country_bbox <- sf::st_bbox(countries_to_include) |>
    sirfunctions::f.expand.bbox(X = 1000, Y = 1000)
  
  cli::cli_h1(paste0("Generating individual images - Start Date: ", date_start, " - End Date: ", date_end))
  for (date_of_eval in date_seq) {
    date_of_eval <- lubridate::as_date(date_of_eval)
    cli::cli_process_start(date_of_eval)
    
    # create temp directory
    out_file <- paste0(tempdir(), "/", date_of_eval, ".png")

    
    if (cumulative_map) {
      cli::cli_alert_info("Producing cumulative map data points results")
      monthly_detections <- emergence_group_pos %>% 
        filter(dateonset >= date_start & 
                 dateonset < date_of_eval %m+% months(1))
    }else{
      monthly_detections <- emergence_group_pos %>% 
        filter(dateonset >= date_of_eval & 
                 dateonset < date_of_eval %m+% months(1))
    }

if(!include_epi){  
      
    
    out_plot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = countries_to_include, fill = c("#FFFFFF"), show.legend = FALSE) +
      # ggplot2::geom_sf(data = prov, color = "grey70", lwd = 0.5, fill = NA, show.legend = F) +
      # ggplot2::geom_sf(data = countries_to_include, color = "grey40", lwd = 0.6, fill = NA, show.legend = F) +
      ggplot2::geom_point(data = monthly_detections %>% filter(eg2 == "NIE-JIS-1") , 
                          aes(x = as.numeric(longitude),
                              y = as.numeric(latitude),
                              color= eg2), 
                          size = 1.6, 
                          show.legend = T) +
      ggplot2::geom_point(data = monthly_detections %>% filter(eg2 == "NIE-ZAS-1") , 
                          aes(x = as.numeric(longitude),
                              y = as.numeric(latitude),
                              color= eg2), 
                          size = 1.6, 
                          show.legend = T) +
      ggplot2::geom_point(data = monthly_detections %>% filter(!eg2 %in% c("NIE-ZAS-1", "NIE-JIS-1")) , 
                          aes(x = as.numeric(longitude),
                              y = as.numeric(latitude),
                              color=eg2), 
                          size = 1.6, 
                          show.legend = T) + 
      ggplot2::scale_color_manual(name = "Emergence\nGroup:", 
                                  values = emg_colors,
                                  drop = FALSE) +
      ggplot2::theme_bw() +
      ggplot2::theme( panel.background = element_rect(fill = "#E3EBFF"),
                     legend.position = "bottom",
                     legend.title = element_text(face="bold", size = 12),
                     plot.title = element_text(hjust = 0.5, face="bold", size = 12),
                     axis.title=element_blank(), 
                     axis.text=element_blank(), 
                     axis.ticks=element_blank(), 
                     legend.key=element_blank()) + 
      guides(color = guide_legend(ncol = 6)) + 
      coord_sf(xlim = c(-40, 110), ylim = c(-20, 75), expand = FALSE) + 
      labs(title = paste0(" Spread of cVDPV2 polio emergences from Nigeria - ",
                          lubridate::year(date_of_eval),
                          " ",
                          lubridate::month(date_of_eval, label = T)))
    
    # print(out_plot)
}else{
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = countries_to_include, fill = c("#FFFFFF"), show.legend = FALSE) +
    # ggplot2::geom_sf(data = prov, color = "grey70", lwd = 0.5, fill = NA, show.legend = F) +
    # ggplot2::geom_sf(data = countries_to_include, color = "grey40", lwd = 0.6, fill = NA, show.legend = F) +
    ggplot2::geom_point(data = monthly_detections %>% filter(eg2 == "NIE-JIS-1") , 
                        aes(x = as.numeric(longitude),
                            y = as.numeric(latitude),
                            color= eg2), 
                        size = 0.8, 
                        show.legend = T) +
    ggplot2::geom_point(data = monthly_detections %>% filter(eg2 == "NIE-ZAS-1") , 
                        aes(x = as.numeric(longitude),
                            y = as.numeric(latitude),
                            color= eg2), 
                        size = 0.8, 
                        show.legend = T) +
    ggplot2::geom_point(data = monthly_detections %>% filter(!eg2 %in% c("NIE-ZAS-1", "NIE-JIS-1")) , 
                        aes(x = as.numeric(longitude),
                            y = as.numeric(latitude),
                            color=eg2), 
                        size = 0.8, 
                        show.legend = T) + 
    ggplot2::scale_color_manual(name = "Emergence\nGroup:", 
                                values = emg_colors,
                                drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme( panel.background = element_rect(fill = "#E3EBFF"),
                    legend.position = "bottom",
                    legend.title = element_text(face="bold", size = 12),
                    plot.title = element_text(hjust = 0.5, face="bold", size = 12),
                    axis.title=element_blank(), 
                    axis.text=element_blank(), 
                    axis.ticks=element_blank(), 
                    legend.key=element_blank()) + 
    guides(color = guide_legend(ncol = 6)) + 
    coord_sf(xlim = c(-40, 110), ylim = c(-20, 75), expand = FALSE) 
  
  
  
  if(!color_epi_curve){  
  
    epi_curve <-
      ggplot2::ggplot() +
      ggplot2::geom_bar(data = monthly_pos, ggplot2::aes(x = month_date, y = n_det), stat = "identity") +
      ggplot2::geom_vline(xintercept = date_of_eval, color = "red", linetype = 2) +
      ggplot2::labs(x = "Month of Paralysis Onset", y = paste("No. of Polio Cases")) +
      ggplot2::scale_y_continuous(breaks = seq(0,100,20),limits = c(0,100)) +
      ggplot2::theme_bw() 
    
}else{
  
  epi_curve <-
    ggplot2::ggplot() +
    ggplot2::geom_bar(data = monthly_pos, ggplot2::aes(x = month_date, y = n_det, fill = eg2), stat = "identity") +
    ggplot2::geom_vline(xintercept = date_of_eval, color = "grey20", linetype = 2) +
    ggplot2::labs(x = "Month of Paralysis Onset", y = paste("No. of Polio Cases")) +
    ggplot2::scale_fill_manual(
      values = emg_colors) +
    ggplot2::scale_y_continuous(breaks = seq(0,100,20),limits = c(0,100)) +
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = "none")
}
     
  out_plot <- ggpubr::ggarrange(epi_curve, map, ncol = 1, heights = c(1, 2)) |>
    ggpubr::annotate_figure(top = paste0(" Spread of cVDPV2 polio emergences from Nigeria - ",
                                         lubridate::year(date_of_eval),
                                         " ",
                                         lubridate::month(date_of_eval, label = T)))
}
  
    ggplot2::ggsave(out_file, plot = out_plot, dpi = 300, height = 8, width = 8, bg = "white")
    
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

# Run Function 
create_emergence_group_gif( emergence_group_root,
                            include_env ,
                            cumulative_map,
                            date_start, 
                            date_end,
                            include_epi, 
                            color_epi_curve,
                            out_gif)


upload_to_sharepoint(
  file_to_upload = out_gif,  # Writes to temp directory
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")
