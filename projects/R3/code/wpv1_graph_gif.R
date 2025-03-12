## R9 - GIF of WPV1 detections in AFG / Pakistan 
## Updated - 13-02-2025 
## By: JPB 
  
# Load Packages 
source("./reference/obx_packages_01.R", local = T)
  
  # Load Data 
  if (exists("raw.data") == TRUE){
    cli::cli_alert("Core data already loaded")
  }else{
    raw.data <- get_all_polio_data(size = "small")
  }
  
# 
# # Get Sharepoint site 
# get_sharepoint_site(
#   site_url = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
#   tenant = Sys.getenv("CLIMICROSOFT365_TENANT", "common"),
#   app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
#   scopes = c("Group.ReadWrite.All", "Directory.Read.All", "Sites.ReadWrite.All",
#              "Sites.Manage.All"),
#   token = NULL)

# Load Data 
data <- raw.data
pos <- data$pos %>% 
   mutate(emergencegroup = if_else(measurement == "WILD 1", "WPV1", emergencegroup),
          wpv1_prox = case_when(
            emergencegroup == "WPV1" & source == "AFP" ~ "WPV1 Case",
            emergencegroup == "WPV1" & source == "ENV" ~ "WPV1 ES")) %>% 
  filter(place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN") & 
         emergencegroup == "WPV1" & 
         source %in% c("AFP", "ENV"))
                      

dist <- data$global.dist %>% filter(ADM0_NAME %in% c("AFGHANISTAN", "PAKISTAN"))
ctry <- data$global.ctry %>% filter(ADM0_NAME %in% c("AFGHANISTAN", "PAKISTAN"))
prov <- data$global.prov %>% filter(ADM0_NAME %in% c("AFGHANISTAN", "PAKISTAN"))


# Load Inputs 
include_env <- T
cumulative <- F #
cumulative_map <- T
date_start = as_date("2019-01-01")
date_end = as_date("2025-02-28")
emergence_group <- "WPV1"
s_w1 <- paste0("W",as.character(epiweek(date_start)), sub="")
w_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")

datt_task_id <- "R3"
sub_folder <- "3. Figures"
fig_name <- paste(emergence_group, "_", date_start, "_", date_end,".gif" , sep = "")
out_gif <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")


# # Output 
# datt_task_id <- "R3"
# sub_folder <- "3. Figures"
# fig_name <- paste("fig1_gif_wpv1_AFGPAK_",year(date_start),s_w1, "_",year(date_end), e_w1,"_", format(today(), "%y%m%d"),".gif", sep="")
# temp_path <- file.path(tempdir(), fig_name)
# sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")


create_emergence_group_gif <- function(
    emergence_group,
    pos,
    dist,
    ctry,
    include_env = T,
    cumulative = T,
    cumulative_map = T,
    out_gif) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop('Package "magick" must be installed to use this function.',
         .call = FALSE
    )
  }
  
  cli::cli_process_start("Setting up data structures")
  
  # set up data structures
  emergence_group_pos <- pos |>
    dplyr::filter(emergencegroup == emergence_group & 
                  dateonset >= date_start & 
                  dateonset < date_end) |>
    dplyr::select(place.admin.0, adm0guid, admin2guid, dateonset, epid, source, latitude, longitude, emergencegroup, wpv1_prox)
  
  if (!include_env) {
    cli::cli_alert("Subsetting to only AFP detections")
    emergence_group_pos <- emergence_group_pos |>
      dplyr::filter(source == "AFP")
  } else {
    cli::cli_alert("Subsetting to only AFP/ENV detections")
    emergence_group_pos <- emergence_group_pos |>
      dplyr::filter(source %in% c("AFP", "ENV"))
  }
  
  # group data by month country and district
  monthly_pos <- emergence_group_pos |>
    filter(place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN") &
           source == "AFP") |>
    dplyr::mutate(month_date = lubridate::floor_date(dateonset, unit = "month")) |>
    dplyr::group_by(adm0guid, admin2guid, month_date) |>
    dplyr::summarise(n_det = dplyr::n(), .groups = "drop")
  
  cli::cli_process_done()
  
  min_date <- monthly_pos |>
    dplyr::pull(month_date) |>
    min()
  
  max_date <- monthly_pos |>
    dplyr::pull(month_date) |>
    max()
  
  date_seq <- seq(min_date, max_date, by = "month")
  
  if (cumulative) {
    cli::cli_alert_info("Producing cumulative results")
      monthly_pos <- tidyr::expand_grid(
       adm0guid = unique(monthly_pos$adm0guid),
       admin2guid = unique(monthly_pos$admin2guid),
       month_date = date_seq
     ) |>
       left_join(monthly_pos, by = c("adm0guid", "admin2guid", "month_date")) |>
       dplyr::group_by(adm0guid, admin2guid) |>
       dplyr::arrange(month_date) |>
       dplyr::mutate(n_det = sum(n_det, dplyr::lag(n_det, default = 0), na.rm = T))
  }
  
  cli::cli_process_start("Parsing spatial information")
  countries_to_include <- ctry |>
    dplyr::filter(GUID %in% (monthly_pos |>
                               dplyr::pull(adm0guid) |>
                               unique()))
  
  country_bbox <- sf::st_bbox(countries_to_include) |>
    sirfunctions::f.expand.bbox(X = 1000, Y = 1000)
  
  # districts_to_include <- dist |>
  #   dplyr::filter(GUID %in% (monthly_pos |>
  #                              dplyr::pull(admin2guid) |>
  #                              unique()))
  
  cli::cli_process_done()
  
  # monthly_pos |>
  #   dplyr::pull(n_det) |>
  #   table() |>
  #   print()
  
  # cli::cli_alert_info("The distribution of detections (incidence or cumulative) is presented above. Please provide a numeric vector of values to bin the # of detections.")
  # 
  # x <- readline(prompt = "Vector: ")
  # 
  # x <- eval(parse(text = x))
  # 
  # monthly_pos <- monthly_pos |>
  #   dplyr::mutate(n_det_cat = cut(n_det, breaks = x, include.lowest = T))
  
  cli::cli_h1(paste0("Generating individual images - Start Date: ", min_date, " - End Date: ", max_date))
  for (date_of_eval in date_seq) {
    date_of_eval <- lubridate::as_date(date_of_eval)
    
    cli::cli_process_start(date_of_eval)
    
    # create temp directory
    out_file <- paste0(tempdir(), "/", date_of_eval, ".png")
    
    # plot_data <- dplyr::inner_join(
    #   dist,
    #   dplyr::filter(monthly_pos, month_date == date_of_eval),
    #   by = c("GUID" = "admin2guid")
    # )
    
    # if (nrow(plot_data) == 0) {
    #   plot_data <- dist |>
    #     dplyr::mutate(n_det = -1) |>
    #     dplyr::mutate(n_det_cat = cut(n_det, breaks = x, include.lowest = T)) |>
    #     dplyr::slice(1)
    # }
    # 
    
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
    
    map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = ctry, fill = c("#EFF7FF", "#E3EBFF")) +
      ggplot2::geom_sf(data = prov, color = "grey70", lwd = 0.5, fill = NA, show.legend = F) +
      ggplot2::geom_sf(data = ctry, color = "grey20", lwd = 0.8, fill = NA) +
      ggplot2::geom_point(data = monthly_detections %>% filter(source == "ENV" & emergence_group == "WPV1") , 
                          aes(x = as.numeric(longitude),
                              y = as.numeric(latitude),
                              color=wpv1_prox,
                              shape = wpv1_prox), 
                          size = 1.6, 
                          show.legend = TRUE) +
      ggplot2::geom_point(data = monthly_detections %>% filter(source == "AFP" & emergence_group == "WPV1") , 
                          aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=wpv1_prox,
                          shape = wpv1_prox), 
                          size = 1.6, 
                          show.legend = TRUE) + 
        ggplot2::scale_color_manual(name = "Detections:", 
                           values = c("WPV1 Case" = "#FF0000",
                                      "WPV1 ES" = "#EE9698"), 
                           breaks = c("WPV1 Case", "WPV1 ES"), 
                           drop = FALSE) +
      ggplot2::scale_shape_manual(name = "Detections:",
                           values = c("WPV1 Case" = 16,
                                      "WPV1 ES" = 15),
                           breaks = c("WPV1 Case", "WPV1 ES"),
                           drop = FALSE) +
      
    # ggplot2::geom_sf(data = plot_data, ggplot2::aes(fill = n_det_cat), show.legend = T) +
      # ggplot2::scale_fill_brewer(palette = "YlOrRd", drop = F, na.translate = F) +
      ggplot2::coord_sf(
        xlim = country_bbox[c("xmin", "xmax")],
        ylim = country_bbox[c("ymin", "ymax")]
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom",
            legend.title = element_text(face="bold", size = 12),
            axis.title=element_blank(), 
            axis.text=element_blank(), 
            axis.ticks=element_blank())  

    epi_curve <- monthly_pos |>
      dplyr::group_by(month_date) |>
      dplyr::summarise(n_det = sum(n_det), .groups = "drop") |>
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = month_date, y = n_det), stat = "identity") +
      ggplot2::geom_vline(xintercept = date_of_eval, color = "red", linetype = 2) +
      ggplot2::labs(x = "Date", y = paste("No. of cases per month")) +
      ggplot2::scale_y_continuous(breaks = seq(0,35,10),limits = c(0,35)) + 
      ggplot2::theme_bw() +
      labs(caption = paste("Produced by CDC-GHC-GID-PEB. Data available through GPEI as of 2025-01-29"))
    
    
    out_plot <- ggpubr::ggarrange(map, epi_curve, ncol = 1, heights = c(2, 1)) |>
      ggpubr::annotate_figure(top = paste0("Spread of ",
        emergence_group,
        " as of ",
        lubridate::year(date_of_eval),
        " ",
        lubridate::month(date_of_eval, label = T)
      ))
    
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
  img_animated <- magick::image_animate(img_joined, fps = 2)
  
  ## save to disk
  magick::image_write(
    image = img_animated,
    path = out_gif
  )
  cli::cli_process_done()
}


# Run Function 
create_emergence_group_gif(emergence_group = emergence_group,
                           pos = pos, 
                           dist = dist, 
                           ctry = ctry, 
                           include_env = include_env, 
                           cumulative_map = cumulative_map,
                           cumulative = cumulative, 
                           out_gif = out_gif)


upload_to_sharepoint(
  file_to_upload = out_gif,  # Writes to temp directory
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")

