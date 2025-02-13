# Uses SIR Functions Package
# Saves output to temp directory and then uploads to DATT folder 



# Output 
datt_task_id <- "R2"
fig_name <- paste("map_pvdetect_12m_",e_w1,"_", format(today(), "%y%m%d"),".png", sep="")
filename = paste0(datt_task_id,"/outputs/",fig_name,".png")

## Load Sharepoint Site & Writing Functions 

get_sharepoint_site(
  site_url = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  tenant = Sys.getenv("CLIMICROSOFT365_TENANT", "common"),
  app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
  scopes = c("Group.ReadWrite.All", "Directory.Read.All", "Sites.ReadWrite.All",
             "Sites.Manage.All"),
  token = NULL)

# Create Task Folder 
datt_task_id <- "R2"
sub_folder <- "3. Figures"
#Label Figure Name (Include FigNo if Multiple)
fig_name <- paste("map_pvdetect_12m_",e_w1,"_", format(today(), "%y%m%d"),".png", sep="")
# Use in ggsave to save to temp file directory 
temp_path <- file.path(tempdir(), fig_name)
# Pathway to write for sharepoint. Needs to have same figure name, will create folders to make pathway 
sp_path <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name, sep ="")

upload_to_sharepoint(
  file_to_upload = temp_path,  # Writes to temp directory 
  sharepoint_file_loc = sp_path,
  site = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  drive = "Documents")
