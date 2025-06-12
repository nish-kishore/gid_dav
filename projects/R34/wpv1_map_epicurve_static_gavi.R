
# Static Image Build
test <- pos %>% filter(emergencegroup == "WPV1")



d4 <- raw.data$metadata$download_time
d3 <- d4 %m-% months(6)
d2 <- d4 %m-% months(12)
d1 <- d4 %m-% months(18)



ctry <- data$global.ctry %>% filter(ADM0_NAME %in% c("AFGHANISTAN", "PAKISTAN"))

countries_to_include <- ctry |>
  dplyr::filter(GUID %in% (ctry |>
                             dplyr::pull(GUID) |>
                             unique()))
country_bbox <- sf::st_bbox(countries_to_include) |>
  sirfunctions::f.expand.bbox(X = 1000, Y = 1000)

pos1 <- pos %>%
  filter(dateonset >= d1 &
           dateonset <= d2)

map_a <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = ctry, fill = c("#EFF7FF", "#E3EBFF")) +
  ggplot2::geom_sf(data = prov, color = "grey70", lwd = 0.5, fill = NA, show.legend = FALSE) +
  ggplot2::geom_sf(data = ctry, color = "grey20", lwd = 0.8, fill = NA) +
  
  ggplot2::geom_point(data = pos1 %>% filter(source == "ENV" & emergencegroup == "WPV1") ,
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=wpv1_prox,
                          shape = wpv1_prox),
                      size = 1.5,
                      show.legend = FALSE) +
  ggplot2::geom_point(data = pos1 %>% filter(source == "AFP" & emergencegroup == "WPV1") ,
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=wpv1_prox,
                          shape = wpv1_prox),
                      size = 2,
                      show.legend = FALSE) +
  scale_color_manual(name = "Detections:",
                     values = c("WPV1 Case" = "#FF0000",
                                "WPV1 ES" = "#EE9698"),
                     breaks = c("WPV1 Case", "WPV1 ES"),
                     drop = FALSE) +
  scale_shape_manual(name = "Detections:",
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
  theme(legend.position = "none",
        legend.title = element_text(face="bold", size = 12),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_markdown(hjust = 0.5, face="bold", size = 12)) +
  ggtitle(paste0(d1, " to ", d2))
print(map_a)

pos1 <- pos %>%
  filter(dateonset > d2 &
           dateonset <= d3)


map_b <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = ctry, fill = c("#EFF7FF", "#E3EBFF")) +
  ggplot2::geom_sf(data = prov, color = "grey70", lwd = 0.5, fill = NA, show.legend = F) +
  ggplot2::geom_sf(data = ctry, color = "grey20", lwd = 0.8, fill = NA) +
  
  ggplot2::geom_point(data = pos1 %>% filter(source == "ENV" & emergencegroup == "WPV1") ,
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=wpv1_prox,
                          shape = wpv1_prox),
                      size = 1.5,
                      show.legend = FALSE) +
  ggplot2::geom_point(data = pos1 %>% filter(source == "AFP" & emergencegroup == "WPV1") ,
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=wpv1_prox,
                          shape = wpv1_prox),
                      size = 2,
                      show.legend = TRUE) +
  scale_color_manual(name = "Detections:",
                     values = c("WPV1 Case" = "#FF0000",
                                "WPV1 ES" = "#EE9698"),
                     breaks = c("WPV1 Case", "WPV1 ES"),
                     drop = FALSE) +
  scale_shape_manual(name = "Detections:",
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
  theme(legend.position = "bottom",
        legend.title = element_text(face="bold", size = 8),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_markdown(hjust = 0.5, face="bold", size = 12)) +
  ggtitle(paste0("2024-06-12", " to ", d3))
guides(
    color = guide_legend(override.aes = list(size = 2), nrow = 2))


pos1 <- pos %>%
  filter(dateonset > d3 &
           dateonset <= d4)


map_c <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = ctry, fill = c("#EFF7FF", "#E3EBFF")) +
  ggplot2::geom_sf(data = prov, color = "grey70", lwd = 0.5, fill = NA, show.legend = F) +
  ggplot2::geom_sf(data = ctry, color = "grey20", lwd = 0.8, fill = NA) +
  
  ggplot2::geom_point(data = pos1 %>% filter(source == "ENV" & emergencegroup == "WPV1") ,
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=wpv1_prox,
                          shape = wpv1_prox),
                      size = 1.5,
                      show.legend = TRUE) +
  ggplot2::geom_point(data = pos1 %>% filter(source == "AFP" & emergencegroup == "WPV1") ,
                      aes(x = as.numeric(longitude),
                          y = as.numeric(latitude),
                          color=wpv1_prox,
                          shape = wpv1_prox),
                      size = 2,
                      show.legend = FALSE) +
  scale_color_manual(name = "Detections:",
                     values = c("WPV1 Case" = "#FF0000",
                                "WPV1 ES" = "#EE9698"),
                     breaks = c("WPV1 Case", "WPV1 ES"),
                     drop = FALSE) +
  scale_shape_manual(name = "Detections:",
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
  theme(legend.position = "none",
        legend.title = element_text(face="bold", size = 12),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_markdown(hjust = 0.5, face="bold", size = 12)) +
  ggtitle(paste0("2024-12-12", " to ", d4)) 

# test <-
#   monthly_pos |>
#   dplyr::group_by(month_date) |>
#   dplyr::summarise(n_det = sum(n_det), .groups = "drop")
# 
# monthly_pos <- pos |>
#   filter(place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN") &
#            source == "AFP" & 
#            dateonset <= "2025-01-01") |>
#   dplyr::mutate(month_date = lubridate::floor_date(dateonset, unit = "month")) |>
#   dplyr::group_by(adm0guid, admin2guid, month_date) |>
#   dplyr::summarise(n_det = dplyr::n(), .groups = "drop")
# 
# # test <- monthly_pos |>
# #   dplyr::group_by(month_date) |>
# #   dplyr::summarise(n_det = sum(n_det), .groups = "drop")
# 
# # Monthly Epi Curve
# epi_curve <- monthly_pos |>
#   dplyr::group_by(month_date) |>
#   dplyr::summarise(n_det = sum(n_det), .groups = "drop") |>
#   ggplot2::ggplot() +
#   ggplot2::geom_bar(ggplot2::aes(x = month_date, y = n_det), stat = "identity") +
#   ggplot2::geom_vline(xintercept = (d2 %m+% months(1)) , color = "darkblue", linetype = 2) +
#   ggplot2::geom_vline(xintercept = (d3 %m-% months(1)) , color = "darkblue", linetype = 2) +
#   ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
#   ggplot2::labs(x = "Date", y = paste("No of cases\n  per month")) +
#   ggplot2::theme_bw() +
#   labs(caption = paste("Produced by CDC-GHC-GID-PEB. Data available through GPEI as of 2025-01-29"))
# 
# 
# 
# Option A
opt_a <- (map_a | map_b | map_c)
print(opt_a)
# 
# opt_b <- map_a / map_b / map_c /  epi_curve

# s_w1 <- paste0("W",as.character(epiweek(date_start)), sub="")
# w_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")

datt_task_id <- "R3"
sub_folder <- "3. Figures"
fig_name <- paste("wpv1_graph_3panel_", d4,"v3.png", sep = "")
out_file <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")
# 
# ggsave(filename = paste("./output/tiger/wpv1_graph_3panel_", d4,".png"), dpi = 300, height = 4, width = 9)
ggplot2::ggsave(out_file, plot = opt_a, dpi = 300, height = 4, width = 9, bg = "white")

upload_to_sharepoint(
  file_to_upload = out_file,  # Writes to temp directory
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")
