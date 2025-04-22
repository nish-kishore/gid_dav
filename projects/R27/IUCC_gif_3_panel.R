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


# Keep AFP Case Static / 
# Do by epi week 
# Highlight the cases 

# Load Inputs 
include_env <- T
# cumulative <- F #
cumulative_map <- T
date_start = as_date("2022-05-19")
date_end = as_date("2023-02-18")
emergence_group <- "IUUC-2022"
s_w1 <- paste0("W",as.character(epiweek(date_start)), sub="")
w_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")
output <- "static"

datt_task_id <- "R27"
sub_folder <- "3. Figures"
fig_name_s <- paste(emergence_group, "_", date_start, "_", date_end,".jpeg" , sep = "")
out_static <- file.path(tempdir(), fig_name_s)
sp_path_s <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name_s, sep ="")

fig_name_g <- paste(emergence_group, "_", date_start, "_", date_end,".gif" , sep = "")
out_gif <- file.path(tempdir(), fig_name_g)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name_g, sep ="")



if (output == "static"){
  emergence_group_pos <- raw.data$pos |>
    dplyr::mutate(e_group2 = dplyr::case_when(
      measurement == "cVDPV 2" & source == "AFP" ~ "cVDPV2 Case",
      measurement == "cVDPV 2" & source == "ENV" ~ "cVDPV2 ES",
      TRUE ~ NA)) %>%
    dplyr::filter(emergencegroup == emergence_group & 
                    dateonset >= date_start & 
                    dateonset < date_end) |>
    dplyr::select(place.admin.0, adm0guid, adm1guid, admin2guid, dateonset, epid, source, latitude, longitude, emergencegroup, e_group2)
  
  countries_to_include <- raw.data$global.ctry |>
    dplyr::filter(WHO_REGION %in% c("AMRO", "EURO", "EMRO", "AFRO"))
  
  
  

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = raw.data$global.prov %>% filter(ADM0_NAME %in% c("UNITED STATES OF AMERICA", "CANADA")), show.legend = F) +
    ggplot2::geom_sf(data = raw.data$global.prov %>% filter(ADM1_NAME %in% c("NEW YORK", "QUEBEC")), fill = "lightyellow", show.legend = F) +
    ggplot2::geom_sf(data = raw.data$global.ctry %>% filter(ADM0_NAME %in% c("CANADA")), color = "grey20", lwd = 0.8, fill = NA, show.legend = F) +
    coord_sf(xlim = c(-80, -55), ylim = c(40, 50), expand = FALSE) + 
    ggplot2::geom_point(data = emergence_group_pos %>% filter(source == "ENV"), 
                        aes(x = as.numeric(longitude),
                            y = as.numeric(latitude),
                            color=e_group2,
                            shape = e_group2), 
                        size = 2, 
                        show.legend = T) + 
    ggplot2::geom_point(data = emergence_group_pos %>% filter(source == "AFP"), 
                        aes(x = as.numeric(longitude),
                            y = as.numeric(latitude),
                            color=e_group2,
                            shape = e_group2), 
                        size = 3, 
                        show.legend = NA) +
    ggplot2::scale_color_manual(name = "Detections:", 
                                values = c("cVDPV2 Case" = "#7131A1",
                                           "cVDPV2 ES" = "#61DB2C"), 
                                breaks = c("cVDPV2 Case", "cVDPV2 ES"), 
                                drop = FALSE) +
    ggplot2::scale_shape_manual(name = "Detections:",
                                values = c("cVDPV2 Case" = 16,
                                           "cVDPV2 ES" = 15),
                                breaks = c("cVDPV2 Case", "cVDPV2 ES"),
                                drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position  = "bottom", 
      panel.background = element_rect(fill = "#E3EBFF"),
      legend.title = element_text(face="bold", size = 16),
      legend.text = element_text(size = 14),
      axis.title=element_blank(), 
      axis.text=element_blank(), 
      axis.ticks=element_blank(),
      legend.key = element_rect(fill='transparent', colour='transparent')) +
    guides(colour = guide_legend(override.aes = list(size=4)))
  
  
  
  map1<- ggplot2::ggplot() +
    ggplot2::geom_sf(data = countries_to_include %>% filter(WHO_REGION == "EURO")) +
    ggplot2::geom_sf(data = countries_to_include %>% filter(ADM0_NAME=="THE UNITED KINGDOM"), fill ="lightyellow") +
    coord_sf(xlim = c(-15, 5), ylim = c(49, 60), expand = FALSE) +
    ggplot2::geom_point(data = emergence_group_pos %>% filter(source == "ENV"), 
                        aes(x = as.numeric(longitude),
                            y = as.numeric(latitude),
                            color=e_group2,
                            shape = e_group2), 
                        size = 2, 
                        show.legend = T) + 
    ggplot2::geom_point(data = emergence_group_pos %>% filter(source == "AFP"), 
                        aes(x = as.numeric(longitude),
                            y = as.numeric(latitude),
                            color=e_group2,
                            shape = e_group2), 
                        size = 3, 
                        show.legend = NA) +
    ggplot2::scale_color_manual(name = "Detections:", 
                                values = c("cVDPV2 Case" = "#7131A1",
                                           "cVDPV2 ES" = "#61DB2C"), 
                                breaks = c("cVDPV2 Case", "cVDPV2 ES"), 
                                drop = FALSE) +
    ggplot2::scale_shape_manual(name = "Detections:",
                                values = c("cVDPV2 Case" = 16,
                                           "cVDPV2 ES" = 15),
                                breaks = c("cVDPV2 Case", "cVDPV2 ES"),
                                drop = FALSE) +
    theme(panel.background = element_rect(fill = "#E3EBFF"),
          legend.position  = "none") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position  = "none", 
      panel.background = element_rect(fill = "#E3EBFF"),
      legend.title = element_text(face="bold", size = 12),
      axis.title=element_blank(), 
      axis.text=element_blank(), 
      axis.ticks=element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    labs(title = "United Kingdom")
  
  
  #Israel subset 
  
  map2 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = countries_to_include) +
    ggplot2::geom_sf(data = countries_to_include %>% filter(ADM0_NAME=="ISRAEL"), fill ="lightyellow") +
    coord_sf(xlim = c(32.5, 37.5), ylim = c(29.4, 33.4), expand = FALSE)+ 
    theme(panel.background = element_rect(fill = "#E3EBFF")) +
    ggplot2::geom_point(data = emergence_group_pos %>% filter(source == "ENV"), 
                        aes(x = as.numeric(longitude),
                            y = as.numeric(latitude),
                            color=e_group2,
                            shape = e_group2), 
                        size = 2, 
                        show.legend = T) + 
    ggplot2::geom_point(data = emergence_group_pos %>% filter(source == "AFP"), 
                        aes(x = as.numeric(longitude),
                            y = as.numeric(latitude),
                            color=e_group2,
                            shape = e_group2), 
                        size = 3, 
                        show.legend = NA) +
    ggplot2::scale_color_manual(name = "Detections:", 
                                values = c("cVDPV2 Case" = "#7131A1",
                                           "cVDPV2 ES" = "#61DB2C"), 
                                breaks = c("cVDPV2 Case", "cVDPV2 ES"), 
                                drop = FALSE) +
    ggplot2::scale_shape_manual(name = "Detections:",
                                values = c("cVDPV2 Case" = 16,
                                           "cVDPV2 ES" = 15),
                                breaks = c("cVDPV2 Case", "cVDPV2 ES"),
                                drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position  = "none", 
      panel.background = element_rect(fill = "#E3EBFF"),
      legend.title = element_text(face="bold", size = 16),
      axis.title=element_blank(), 
      axis.text=element_blank(), 
      axis.ticks=element_blank(),
      plot.title = element_text(hjust = 0.5))  + 
    labs(title = "Israel")
  
  
  
  # Overlay maps
full_map <- ggdraw(map) +
    draw_plot(
      {map2  +    
          theme(plot.background = element_rect(color = "black")) 
        
      },
      x = 0.65, 
      y = 0.15,
      width = 0.4, 
      height = 0.37) + 
    draw_plot(
      {map1  +   
          theme(plot.background = element_rect(color = "black")) 
        
      },
      x = 0.65, 
      y = 0.56,
      width = 0.4, 
      height = 0.37)  
  
print(full_map)  

ggsave(filename = out_static, height = 8.3, width = 12, units = "in", dpi = 300)
upload_to_sharepoint(
  file_to_upload = out_static,  # Writes to temp directory 
  sharepoint_file_loc = sp_path_s,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")

cli::cli_alert("Beep Beep - Image uploaded to Sharepoint")
  
}




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
  emergence_group_pos <- raw.data$pos |>
    dplyr::mutate(e_group2 = dplyr::case_when(
      measurement == "cVDPV 2" & source == "AFP" ~ "cVDPV2 Case",
      measurement == "cVDPV 2" & source == "ENV" ~ "cVDPV2 ES",
      TRUE ~ NA),
      e_group2 = factor(e_group2, levels = c("cVDPV2 Case", "cVDPV2 ES"))) %>%
    
    dplyr::filter(emergencegroup == emergence_group & 
                  dateonset >= date_start & 
                  dateonset < date_end) |>
    dplyr::select(place.admin.0, adm0guid, adm1guid, admin2guid, dateonset, epid, source, latitude, longitude, emergencegroup, e_group2)
  
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
  week_pos <- emergence_group_pos |>
    # filter(place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN") &
    #        source == "AFP") |>
    dplyr::mutate(week_date = lubridate::floor_date(dateonset, unit = "week")) |>
    dplyr::group_by(adm0guid, adm1guid, week_date) |>
    dplyr::summarise(n_det = dplyr::n(), .groups = "drop")

  cli::cli_process_done()
  
  min_date <- week_pos |>
    dplyr::pull(week_date) |>
    min()
  
  max_date <- week_pos |>
    dplyr::pull(week_date) |>
    max()
  
  date_seq <- seq(min_date, max_date, by = "week")
  
  # if (cumulative) {
  #   cli::cli_alert_info("Producing cumulative results")
  #     monthly_pos <- tidyr::expand_grid(
  #      adm0guid = unique(monthly_pos$adm0guid),
  #      admin2guid = unique(monthly_pos$admin2guid),
  #      month_date = date_seq
  #    ) |>
  #      left_join(monthly_pos, by = c("adm0guid", "admin2guid", "month_date")) |>
  #      dplyr::group_by(adm0guid, admin2guid) |>
  #      dplyr::arrange(month_date) |>
  #      dplyr::mutate(n_det = sum(n_det, dplyr::lag(n_det, default = 0), na.rm = T))
  # }
  
  cli::cli_process_start("Parsing spatial information")
  countries_to_include <- raw.data$global.ctry |>
    dplyr::filter(WHO_REGION %in% c("AMRO", "EURO", "EMRO", "AFRO"))
  
  # country_bbox <- sf::st_bbox(countries_to_include) |>
  #   sirfunctions::f.expand.bbox(X = 1000, Y = 1000)
  # 
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

    # date_of_eval <- as_date("2023-02-18")
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
      week_points <- emergence_group_pos %>% 
             filter(dateonset >= date_start & 
                    dateonset < date_of_eval %m+% weeks(1))
    }else{
      week_points <- emergence_group_pos %>% 
        filter(dateonset >= date_of_eval & 
                 dateonset < date_of_eval %m+% weeks(1))
    }


    
map_uk <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = countries_to_include %>% filter(WHO_REGION == "EURO")) +
      ggplot2::geom_sf(data = countries_to_include %>% filter(ADM0_NAME=="THE UNITED KINGDOM"), fill ="lightyellow") +
      coord_sf(xlim = c(-12, 5), ylim = c(49, 56), expand = FALSE) +
      ggplot2::geom_point(data = week_points %>% filter(source == "ENV"), 
                          aes(x = as.numeric(longitude),
                              y = as.numeric(latitude),
                              color=e_group2,
                              shape = e_group2), 
                          size = 4, 
                          show.legend = T) + 
      ggplot2::geom_point(data = week_points %>% filter(source == "AFP"), 
                          aes(x = as.numeric(longitude),
                              y = as.numeric(latitude),
                              color=e_group2,
                              shape = e_group2), 
                          size = 6, 
                          show.legend = NA) +
      ggplot2::scale_color_manual(name = "Detections:", 
                                  values = c("cVDPV2 Case" = "#8B0000",
                                             "cVDPV2 ES" = "#00008B"),  
                                  breaks = c("cVDPV2 Case", "cVDPV2 ES"), 
                                  drop = FALSE) +
      ggplot2::scale_shape_manual(name = "Detections:",
                                  values = c("cVDPV2 Case" = 16,
                                             "cVDPV2 ES" = 15),
                                  breaks = c("cVDPV2 Case", "cVDPV2 ES"),
                                  drop = FALSE) +
      theme(panel.background = element_rect(fill = "#E3EBFF"),
            legend.position  = "none") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position  = "none", 
        panel.background = element_rect(fill = "#E3EBFF"),
        legend.title = element_text(face="bold", size = 12),
        axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
      labs(title = "United Kingdom")



# Israel Map 
map_isr <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = countries_to_include) +
  ggplot2::geom_sf(data = countries_to_include %>% filter(ADM0_NAME=="ISRAEL"), fill ="lightyellow") +
  coord_sf(xlim = c(32.5, 37.5), ylim = c(29.4, 33.4), expand = FALSE)+ 
  theme(panel.background = element_rect(fill = "#E3EBFF")) +
  ggplot2::geom_point(data = week_points %>% filter(source == "ENV"), 
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=e_group2,
                          shape = e_group2), 
                      size = 4, 
                      show.legend = T) + 
  ggplot2::geom_point(data = week_points %>% filter(source == "AFP"), 
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=e_group2,
                          shape = e_group2), 
                      size = 6, 
                      show.legend = NA) +
  ggplot2::scale_color_manual(name = "Detections:", 
                              values = c("cVDPV2 Case" = "#8B0000",
                                         "cVDPV2 ES" = "#00008B"), 
                              breaks = c("cVDPV2 Case", "cVDPV2 ES"), 
                              drop = FALSE) +
  ggplot2::scale_shape_manual(name = "Detections:",
                              values = c("cVDPV2 Case" = 16,
                                         "cVDPV2 ES" = 15),
                              breaks = c("cVDPV2 Case", "cVDPV2 ES"),
                              drop = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position  = "none", 
    panel.background = element_rect(fill = "#E3EBFF"),
    legend.title = element_text(face="bold", size = 12),
    axis.title=element_blank(), 
    axis.text=element_blank(), 
    axis.ticks=element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"))  + 
  labs(title = "Israel")




# United States & Canada
    
map_usa_leg <-   ggplot2::ggplot() +
  ggplot2::geom_sf(data = raw.data$global.prov %>% filter(ADM0_NAME %in% c("UNITED STATES OF AMERICA", "CANADA")), show.legend = NA) +
  ggplot2::geom_sf(data = raw.data$global.prov %>% filter(ADM1_NAME %in% c("NEW YORK", "QUEBEC")), fill = "lightyellow", show.legend = NA) +
  ggplot2::geom_sf(data = raw.data$global.ctry %>% filter(ADM0_NAME %in% c("CANADA")), color = "grey20", lwd = 0.8, fill = NA, show.legend = NA) +
  coord_sf(xlim = c(-80, -55), ylim = c(40, 50), expand = FALSE) + 
  ggplot2::geom_point(data = emergence_group_pos %>% filter(source == "ENV"), 
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=e_group2,
                          shape = e_group2), 
                      size = 4, 
                      show.legend = TRUE) + 
  ggplot2::geom_point(data = emergence_group_pos %>% filter(source == "AFP"), 
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=e_group2,
                          shape = e_group2), 
                      size = 6, 
                      show.legend = TRUE) +
  ggplot2::scale_color_manual(name = "Detections:", 
                              values = c("cVDPV2 Case" = "#8B0000",
                                         "cVDPV2 ES" = "#00008B"),  
                              breaks = c("cVDPV2 Case", "cVDPV2 ES"), 
                              drop = FALSE) +
  ggplot2::scale_shape_manual(name = "Detections:",
                              values = c("cVDPV2 Case" = 16,
                                         "cVDPV2 ES" = 15),
                              breaks = c("cVDPV2 Case", "cVDPV2 ES"),
                              drop = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position  = "bottom", 
    panel.background = element_rect(fill = "#E3EBFF"),
    legend.title = element_text(face="bold", size = 16),
    legend.text = element_text(size = 14),    
    axis.title=element_blank(), 
    axis.text=element_blank(), 
    axis.ticks=element_blank(),
    legend.key = element_rect(fill='transparent', colour='transparent')) +
  guides(colour = guide_legend(override.aes = list(size=4)))

legend <- get_legend(map_usa_leg)

    
    
map_usa <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = raw.data$global.prov %>% filter(ADM0_NAME %in% c("UNITED STATES OF AMERICA", "CANADA")), show.legend = NA) +
      ggplot2::geom_sf(data = raw.data$global.prov %>% filter(ADM1_NAME %in% c("NEW YORK", "QUEBEC")), fill = "lightyellow", show.legend = NA) +
      ggplot2::geom_sf(data = raw.data$global.ctry %>% filter(ADM0_NAME %in% c("CANADA")), color = "grey20", lwd = 0.8, fill = NA, show.legend = NA) +
      coord_sf(xlim = c(-80, -70), ylim = c(40, 47), expand = FALSE) + 
  ggplot2::geom_point(data = week_points %>% filter(source == "ENV"), 
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=e_group2,
                          shape = e_group2), 
                      size = 4, 
                      show.legend = TRUE) + 
  ggplot2::geom_point(data = week_points %>% filter(source == "AFP"), 
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=e_group2,
                          shape = e_group2), 
                      size = 6, 
                      show.legend = TRUE) +
ggplot2::scale_color_manual(name = "Detections:", 
                            values = c("cVDPV2 Case" = "#8B0000",
                                       "cVDPV2 ES" = "#00008B"),  
                            breaks = c("cVDPV2 Case", "cVDPV2 ES"), 
                            drop = FALSE) +
  ggplot2::scale_shape_manual(name = "Detections:",
                              values = c("cVDPV2 Case" = 16,
                                         "cVDPV2 ES" = 15),
                              breaks = c("cVDPV2 Case", "cVDPV2 ES"),
                              drop = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position  = "none", 
    panel.background = element_rect(fill = "#E3EBFF"),
    legend.title = element_text(face="bold", size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title=element_blank(), 
    axis.text=element_blank(), 
    axis.ticks=element_blank(),
    legend.key = element_rect(fill='transparent', colour='transparent')) +
    guides(colour = guide_legend(override.aes = list(size=2))) + 
  labs(title = "United States & Canada")
  

# Panel Design 
test <- ggarrange(map_uk, map_isr, map_usa,  nrow = 1, align = "h", common.legend = T, legend = "bottom") +
plot_annotation(title = "GFG Multiplot") &  theme(plot.title = element_text(hjust = 0.5)) 
print(test)

out_map <- (map_uk | map_isr | map_usa) / legend 
print(out_map)

out_map <- plot_grid(map_uk , map_isr , map_usa, nrow =1, align = "h") 
print(out_map)

out_map_final <- plot_grid(out_map, legend, rel_heights = c(0.95, 0.05), ncol = 1)+ 
  plot_annotation(title = paste0("Spread of ",
                                 emergence_group,
                                 " Emergence as of ",
                                 lubridate::year(date_of_eval),
                                 "-",
                                 lubridate::month(date_of_eval, label = T),
                                 " (EpiWk ",
                                 lubridate::epiweek(date_of_eval),
                                 ")")) &  theme(plot.title = element_text(hjust = 0.5, size = 16)) 



ggplot2::ggsave(out_file, plot = out_map_final, dpi = 300, height = 6, width = 9, bg = "white")
    
    cli::cli_process_done()
  }
  
  cli::cli_h1("Generating Gif")
  
  cli::cli_process_start("Reading images")
  ## list file names and read in
  imgs <- list.files(tempdir(), pattern = ".png", full.names = T)
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
create_emergence_group_gif(emergence_group = emergence_group,
                           include_env = include_env, 
                           cumulative_map = cumulative_map,
                           cumulative = cumulative, 
                           out_gif = out_gif)


upload_to_sharepoint(
  file_to_upload = out_gif,  # Writes to temp directory
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")

