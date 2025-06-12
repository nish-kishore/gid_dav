# Epicurve 

pos <- data$pos %>% 
  mutate(emergencegroup = if_else(measurement == "WILD 1", "WPV1", emergencegroup),
         wpv1_prox = case_when(
           emergencegroup == "WPV1" & source == "AFP" ~ "WPV1 Case",
           emergencegroup == "WPV1" & source == "ENV" ~ "WPV1 ES")) %>% 
  filter( 
           emergencegroup == "WPV1" & 
           source %in% c("AFP", "ENV"))

epi_pos <- pos %>% 
             mutate(month = format(dateonset, "%Y-%m")) %>% 
             distinct(epid, .keep_all = T) %>%
             group_by(month, source) %>% 
             count() %>% 
             mutate(month2 = ymd(paste(month,"-01", sep = "")))

wpv1_ec <- ggplot() +
  geom_bar(data = epi_pos, mapping = aes(x = month2, y = n, fill = source), stat = "identity") +
  scale_fill_manual(name = "WPV1:",
                     values = c("AFP" = "#FF0000",
                                "ENV" = "#EE9698"),
                     labels = c("Cases", "ES")) + 
  scale_x_date(date_breaks = "6 months", limits = c(as_date("2018-12-01"), as_date("2025-06-01")), date_labels = "%y-%b") + 
  scale_y_continuous(breaks = seq(0,100,20)) +
  facet_grid(source ~.) + 
  theme_bw() + 
  labs(
    x = "Month",
    y = "No. of Detections"
  ) + 
  theme(
    legend.position = "bottom", 
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 8), 
    axis.text.x = element_text(angle = 45, hjust =1),
    legend.title  = element_text(size = 8, face = "bold"), 
    legend.key.size = unit(0.5,"cm"),
    legend.text = element_text(size = 8))
  
print(wpv1_ec)

datt_task_id <- "R3"
sub_folder <- "3. Figures"
fig_name <- paste("wpv1_ec__", raw.data$metadata$download_time,"v8.png", sep = "")
out_file <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")
# 
# ggsave(filename = paste("./output/tiger/wpv1_graph_3panel_", d4,".png"), dpi = 300, height = 4, width = 9)
ggplot2::ggsave(out_file, plot = wpv1_ec, dpi = 300, height = 3.8, width = 6, bg = "white")

upload_to_sharepoint(
  file_to_upload = out_file,  # Writes to temp directory
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")


# Epicurve for type 2 
pos <- data$pos  %>% 
  filter( 
    measurement == "cVDPV 2" & 
      source %in% c("AFP", "ENV"))

epi_pos <- pos %>% 
  mutate(month = format(dateonset, "%Y-%m")) %>% 
  distinct(epid, .keep_all = T) %>%
  group_by(month, source) %>% 
  count() %>% 
  mutate(month2 = ymd(paste(month,"-01", sep = "")))

cVDPV2_ec <- ggplot() +
  geom_bar(data = epi_pos, mapping = aes(x = month2, y = n, fill = source), stat = "identity") +
  scale_fill_manual(name = "cVDPV2:",
                    values = c("AFP" = "#38A800",
                               "ENV" = "#61DB2C"),
                    labels = c("Cases", "ES")) + 
  scale_x_date(date_breaks = "6 months", limits = c(as_date("2018-12-01"), as_date("2025-06-01")), date_labels = "%y-%b") + 
  scale_y_continuous(breaks = seq(0,150,25)) +
  facet_grid(source ~.) + 
  theme_bw() + 
  labs(
    x = "Month",
    y = "No. of Detections"
  ) + 
  theme(
    legend.position = "bottom", 
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 8), 
    axis.text.x = element_text(angle = 45, hjust =1),
    legend.title  = element_text(size = 8, face = "bold"), 
    legend.key.size = unit(0.5,"cm"),
    legend.text = element_text(size = 8))

print(cVDPV2_ec)

datt_task_id <- "R34"
sub_folder <- "3. Figures"
fig_name <- paste("cVDPV2_ec_", raw.data$metadata$download_time,"v1.png", sep = "")
out_file <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")
# 
# ggsave(filename = paste("./output/tiger/wpv1_graph_3panel_", d4,".png"), dpi = 300, height = 4, width = 9)
ggplot2::ggsave(out_file, plot = cVDPV2_ec, dpi = 300, height = 3.8, width = 6, bg = "white")

upload_to_sharepoint(
  file_to_upload = out_file,  # Writes to temp directory
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")

