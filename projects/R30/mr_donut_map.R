# Measles Donut Map Code 

# Load packages: 
source("./reference/obx_packages_01.R", local = T)
library(ggarrow)

mr_data <- read_excel("reference/mr_sub.xlsx")

# Load Data 
if (exists("raw.data") == TRUE){
  cli::cli_alert("Core data already loaded")
}else{
  raw.data <- get_all_polio_data(size = "small")
}


datt_task_id <- "R30"
sub_folder <- "3. Figures"
fig_name <- paste("donutemap_", today(),".jpeg" , sep = "")
out_pic <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")



# Code for Donut Plot Calculate the percentage based on TotalAFPcases
mr_data <- mr_data %>%
  mutate(Percent = N / sum(N),
         pct_round = round(Percent*100,0))

# Compute the cumulative percentages (top of each rectangle)
mr_data$ymax <- cumsum(mr_data$Percent)

# Compute the bottom of each rectangle
mr_data$ymin <- c(0, head(mr_data$ymax, n = -1))



# Make the plot
paralytic_donut_a <- ggplot(mr_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Loc)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() + 
  labs(fill = "Imported cases:") +
  theme(legend.position = "bottom",
        text = element_text(face = "bold")) +
  geom_text(aes(x = ifelse(Loc == "US Residents", 3.35, 3.5), y = (ymin + ymax) / 2, label = paste0(pct_round,"%\n (", N,")")), color = "white", size = 4, fontface = "bold") +
  scale_fill_manual(values = c("#82A7CC", "#0033CC"),
                                        na.value = "grey80")

print(paralytic_donut_a)



ggsave(filename = out_pic, height = 4, width = 8, dpi = 300)

upload_to_sharepoint(
  file_to_upload = out_pic,  # Writes to temp directory
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")

#This saves the donut plot; copy onto left side of Slide 8
