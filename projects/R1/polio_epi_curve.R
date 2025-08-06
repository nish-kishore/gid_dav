################################################################################
########## Current Status of Polio Eradication Graphic #########################
################################################################################

# load packages
source("./reference/obx_packages_01.R", local = T)
library(ggbreak)
# Inputs 
# Read Data 
# Load Data

 raw.data <- sirfunctions::get_all_polio_data(size = "small")


# Inputs 
# Output 
datt_task_id <- "R1"
sub_folder <- "3. Figures"
date_end <- floor_date(raw.data$metadata$download_time, "week", week_start = 1) %m+% days(1)

# With table 
e_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")
fig_name <- paste("polio_epi_curve_simple",e_w1,"_", format(today(), "%y%m%d"),".png", sep="")
temp_path <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")


## Load Sharepoint Site & Writing Functions 
get_sharepoint_site(
  site_url = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  tenant = Sys.getenv("CLIMICROSOFT365_TENANT", "common"),
  app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
  scopes = c("Group.ReadWrite.All", "Directory.Read.All", "Sites.ReadWrite.All",
             "Sites.Manage.All"),
  token = NULL)

# set parameters
prior_year <- year(Sys.Date() %m-% years(0))

# load data, make sure you use the size = "large" in order to download data back to 2001

# Small graph (cVDPV and WPV cases 2014-2024)----------------------------------
# collapse data set into aggregate number of AFP cases per year by type
pos <- raw.data$pos %>%
  filter(source == "AFP" & measurement %in% c("WILD 1", "cVDPV 1", "cVDPV 2", "cVDPV 3") 
         & yronset >= 2014 & yronset <= prior_year) %>%
  mutate(measurement_1 = case_when(
    measurement == "WILD 1" ~ "WPV",
    measurement == "cVDPV 1" ~ "cVDPV",
    measurement == "cVDPV 2" ~ "cVDPV",
    measurement == "cVDPV 3" ~ "cVDPV",
  )) %>%
  distinct(epid, measurement, .keep_all = T) %>% 
  group_by(yronset, measurement_1) %>%
  summarize(cases=n())

# make the line graph
graph_1 <- ggplot() +
  geom_line(data = pos, mapping = aes(x = yronset, y = cases, group = measurement_1, 
                                      color = measurement_1), linewidth = 1) +
  geom_point(data = pos, mapping = aes(x = yronset, y = cases, group = measurement_1, 
                                       color = measurement_1), size = 3) +
  labs(title = paste0("Disaggregated cVDPV and WPV Cases\nby Year, 2014-", prior_year),
       x = "", 
       y = "", 
       color = " ") +
  theme_minimal() +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme(legend.position = "bottom",
        plot.title = element_text(color = "dodgerblue2", size = 10),
        axis.text = element_text(color = "dodgerblue2", face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.margin = margin(t = -30, r = 0, b = 0, l = 0)) +
  scale_x_continuous(
    breaks = seq(min(pos$yronset), max(pos$yronset), by = 1),  
    labels = as.character(seq(min(pos$yronset), max(pos$yronset), by = 1))
  ) +
  scale_y_continuous(
    limits = c(0, 1200),
    breaks = c(0, 200, 400, 600, 800, 1000, 1200)
  )

print(graph_1)

## Add in Conchi Data 
new_cases <- read_excel("C:/Users/qdz1/Desktop/polio_case_mod_CE.xlsx")
cases <- new_cases %>% filter(Year >= 1988 & Year <=2025) %>%
             select(Year, all_cases) %>% 
             mutate(Year = as.numeric(Year),
                    b1000 = all_cases / 1000)

# Larger graph (total of all AFP cases by year, 1986-2024)----------------------
# collapse data set into aggregate number of AFP cases per year by type
# pos_all <- raw.data$pos %>%
#   filter(source == "AFP" & measurement %in% c("WILD 1", "WILD 2", "WILD 3", 
#                                               "cVDPV 1", "cVDPV 2", "cVDPV 3") 
#                  & yronset >= 1986 & yronset <= prior_year) %>%
#   group_by(yronset) %>%
#   summarize(cases=n())

# # create data frame with the historical polio case data, copied from WUENIC
# wuenic <- data.frame(yronset = seq(1986, 2000),
#                      cases = c(32846, 39683, 34617, 26104, 23053, 13274, 15304, 
#                                10500, 8666, 7043, 4070, 5186, 6347, 7141, 2971))
# 
# # bind WUENIC data to POLIS which starts in 2001
# pos_all <- rbind(pos_all, wuenic)
cases <- cases

# make the graph
graph_2 <- ggplot() +
  geom_bar(data = cases, mapping = aes(x = Year, y = b1000), stat = "identity", fill = "purple4") +
  # geom_smooth(data = cases, mapping = aes(x = Year, y = b100), color = "darkgray", se = FALSE, linetype = 7) +
  scale_y_continuous(limits = c(0,400)) +
  scale_y_cut(breaks=c(9, 90), space = 0.3)+ 
  scale_x_continuous(limits = c(1987, prior_year+3), breaks = seq(1988, prior_year, by = 4)) + 
  annotate("text", x = 2019, y = 4, label = "Africa declared\nWPV-free", 
           color = "black", fontface = "bold", size = 3) + theme_classic() +
  annotate("segment", x = 2019, xend = 2019, y = 3.2, yend = 0.75, 
           arrow = arrow(length = unit(0.2, "cm")), color = "red3", size = 1.5) +
  # geom_ellipse(aes(x0 = 2022, y0 = 0, a = 3.7, b = 2.1, angle = 0), 
  #              color = "red3", size = 1.5, fill = NA) + 
  # annotate("text", x = 2022, y = 3.2, label = "Surge of\ncVDPV\noutbreaks", 
  #          color = "black", fontface = "bold", size = 3.5) + 
  annotate("text", x = 2016, y = 3, label = "tOPV/bOPV\nSwitch", 
           color = "black", fontface = "bold", size = 3) + 
  annotate("text", x = 2016, y = 25, label = "  \n   ", 
           color = "black", fontface = "bold", size = 4) +
  annotate("segment", x = 2016, xend = 2016, y = 2.2, yend = 0.2, 
           arrow = arrow(length = unit(0.2, "cm")), color = "red3", size = 1.5) +
  annotate("segment", x = 2001, xend = 1988, y = 200, yend = 200, 
           arrow = arrow(length = unit(0.2, "cm")), color = "red3", size = 1.5) + 
  annotate("text", x = 2004, y = 200, label = "The Global Polio\nEradication Initiative\nis created in 1988", 
           color = "black", fontface = "bold", size = 4) + 
annotate("text", x = 1999.5, y = 3, label = "The Americas declared\nWPV-Free",
         color = "black", fontface = "bold", size = 3, hjust = 0)+
annotate("segment", x = 2000, xend = 2000, y = 2, yend = 0.9,
         arrow = arrow(length = unit(0.2, "cm")), color = "red3", size = 1.5) +
  annotate("text", x = 2025, y = 3, label = "2025\nYear to Date", 
           color = "black", fontface = "bold", size = 3) + 
  annotate("segment", x = 2025, xend = 2025, y = 2.2, yend = 0.2, 
           arrow = arrow(length = unit(0.2, "cm")), color = "red3", size = 1.5) +
  annotate("text", x = 2025, y = 3, label = "2025\nYear to Date", 
           color = "black", fontface = "bold", size = 3) + 
  labs(title = "Current Status of Polio Eradication",
       x = "Year", 
       subtitle = "Cases of WPV and cVDPV Poliomyelitis by Year, 1988-2025",
       y = "Thousands of paralytic polio cases", 
       fill = "Number of reported cases (cVDPVs and WPVs)",
       caption = paste0("cVDPV: circulating Variant Poliovirus, WPV: Wild Poliovirus\nData available through GPEI as of July 29, 2025\nProduced by CDC-GHC-GID-PEB")) + 
  theme(
    axis.title = element_text(size = 28, face = "bold"), 
    axis.text = element_text(size = 24), 
    plot.title = element_text(size = 48, face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 32, face = "bold", color = "black", hjust = 0.5))

  print(graph_2)
  
  # scale_y_continuous(name = "Total Cases (x 100 Cases)", 
  #                    limits = c(-160, 4000), breaks = c(0, 500, 1000, 1500,
  #                                                                           2000, 2500, 3000, 3500, 4000)) +
                     

ggsave(filename = temp_path, plot = graph_2, height = 9, width = 16, dpi = 300,  unit = "in", bg = 'white')
  
  upload_to_sharepoint(
    file_to_upload = temp_path,  # Writes to temp directory 
    sharepoint_file_loc = sp_path,
    site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
    drive = "Documents")
  
  
  

# Option 2 - Simple graph 
graph_3 <- ggplot() +
    geom_bar(data = cases, mapping = aes(x = Year, y = b1000), stat = "identity", fill = "purple4") +
    # geom_smooth(data = cases, mapping = aes(x = Year, y = b100), color = "darkgray", se = FALSE, linetype = 7) +
    scale_y_continuous(limits = c(0,400), breaks = seq(0,400, 50)) +
    scale_x_continuous(limits = c(1987, prior_year+2), breaks = seq(1988, prior_year, by = 2)) + 
    labs(
      x = "Year", 
      y = "Thousands of paralytic polio cases", 
      fill = "Number of reported cases (cVDPVs and WPVs)",
      caption = paste0("Produced by CDC-GHC-GID-PEB \nData available through GPEI as of ", raw.data$metadata$download_time)) + 
    theme_minimal() +
    theme(
      axis.title = element_text(size = 20, face = "bold"), 
      axis.text = element_text(size = 18), 
      plot.title = element_text(size = 14, face = "bold")) 
  
  print(graph_3) 

  ggsave(filename = temp_path, plot = graph_3, height = 10, width = 12, scale = 1.2, dpi = 300, bg = 'white')
  
  upload_to_sharepoint(
    file_to_upload = temp_path,  # Writes to temp directory 
    sharepoint_file_loc = sp_path,
    site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
    drive = "Documents")
  
  
  
  
  # theme(legend.position = "bottom",
  #       plot.title = element_text(color = "blue2", margin = margin(t = 20, b = 10)),
  #       axis.text = element_text(color = "dodgerblue2", face = "bold"),
  #       axis.title.y = element_text(color = "dodgerblue2", face = "bold"),
  #       panel.grid.minor.x = element_blank(),
  #       panel.grid.major.x = element_blank(),
  #       axis.text.x = element_text(angle = 75, hjust = 0.5, vjust = 0.5)) +
  # # scale_y_continuous(limits = c(-1600, 40000), breaks = c(0, 5000, 10000, 15000,
  # #                                                         20000, 25000, 30000, 35000, 40000))

print(graph_2)

# combine these two together onto one plot--------------------------------------
eradication_status_graphic <- ggdraw() +
  draw_plot(graph_2, x=0, y=0, width=1, height=1) +
  draw_plot(graph_1, x=0.35, y = 0.5, width = 0.65, height = 0.4) +
  draw_label("Current Status of Polio Eradication", x=0.5, y=0.98, fontface = "bold",
             size = 16, hjust = 0.5, color = "navy") +
  # draw_line(x = c(0, 1), y = c(0.95, 0.95), color = "navy", size = 1) +
  draw_grob(rectGrob(x = 0.675, y = 0.708, width = 0.64, height = 0.38, 
                     gp = gpar(col = "navy", fill = NA, lwd = 1)))

print(eradication_status_graphic)

ggsave(filename = temp_path, plot = eradication_status_graphic, height = 4.1, width = 6, scale = 1.75, dpi = 300, bg = 'white')

upload_to_sharepoint(
  file_to_upload = temp_path,  # Writes to temp directory 
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")


