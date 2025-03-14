# MR Immunity Map 
datt_task_id <- "R31"
sub_folder <- "3. Figures"
fig_name <- paste("donutemap_", today(),".jpeg" , sep = "")
out_pic <- file.path(tempdir(), fig_name)
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")

get_sharepoint_site(
  site_url = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  tenant = Sys.getenv("CLIMICROSOFT365_TENANT", "common"),
  app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
  scopes = c("Group.ReadWrite.All", "Directory.Read.All", "Sites.ReadWrite.All",
             "Sites.Manage.All"),
  token = NULL)

# Get country shape files: 
ctry <- raw.data$global.ctry 

mr_data <- read_excel("reference/mr_ratio_240314.xlsx") %>%
  mutate(across(where(is.character), toupper)) %>%
  mutate(Country = ifelse(Country == "CÔTE D'IVOIRE", "COTE D IVOIRE", Country),
         Country = ifelse(Country == "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND", "THE UNITED KINGDOM" ,Country))

data <- left_join(ctry, mr_data, by = c("ADM0_NAME"="Country")) 


data <- data %>% 
  mutate(
    ratio_cat = case_when( 
      Ratio <= 0.75 ~ "Low Risk", 
      Ratio > 0.75 & Ratio <= 1.00 ~ "Watch",
      Ratio > 1.00 & Ratio <= 2.00 ~ "Warning (Cat 1)",
      Ratio > 2.00 & Ratio <= 3.00 ~ "Warning (Cat 2)",
      Ratio > 3.00 & Ratio <= 4.00 ~ "Warning (Cat 3)",
      Ratio > 4.00 & Ratio <= 5.00 ~ "Warning (Cat 4)",
      Ratio > 5.00  ~ "Warning (Cat 5)",
      TRUE ~ "No Data"), 
     ratio_cat = factor(ratio_cat, levels = c("No Data", "Low Risk", "Watch", "Warning (Cat 1)", "Warning (Cat 2)", "Warning (Cat 3)",
                                              "Warning (Cat 4)", "Warning (Cat 5)")))


map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = data,  fill = "#FFFFFF") +
  ggplot2::geom_sf(data = data , aes(fill = ratio_cat), color = "grey20", lwd = 0.4, show.legend = T) +
  scale_fill_manual(name= "Risk of \n National Measles Outbreak",
                    values = c("white","#EED5B7", "#B4EEB4", "#CAE1FF",
                                "#00B2EE", "#1C86EE", "#104E8B", 
                                "black"),
                    drop = F) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "#E3EBFF"),
        legend.title = element_text(face="bold", size = 12),
        legend.text = element_text(size = 12),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_markdown(hjust = 0.5, face="bold", size = 12)) +
  guides(fill = guide_legend(nrow = 2)) 

print(map)
