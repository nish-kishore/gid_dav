# CDC Ready Mapping - Data frame pull

# Questions:
  # Include other surveillance?

library(here)
# AFRO AFP And ES Detections Last 90 Days
source(here("reference/obx_packages_01.R"))
library(aweek)

# Get data
raw.data <- sirfunctions::get_all_polio_data()

# Set Inputs

virus_type <- c("WILD 1", "cVDPV 1", "cVDPV 2", "cVDPV 3")
v2 <- c("WPV1", "cVDPV1", "cVDPV2", "cVDPV3")

end_date <- floor_date(raw.data$metadata$download_time, "week", week_start = 2)
start_date <- floor_date(end_date %m-% months(13), "week", week_start = 7)
s_source <- c("AFP", "ENV")

# Report Date Window for New Viruses
doa <-  floor_date(raw.data$metadata$download_time, "week", week_start = 1)
date_3a <- doa - days(5)   # closing window for cVDPV's
date_3a_a <- doa - days(6) # extend an additional day for WPV's
date_3b <- doa + days(1) # report window closes



# Set up date
p1 <- raw.data$pos %>% filter(
                          dateonset >= start_date &
                          dateonset <= end_date &
                          source %in% s_source &
                          measurement %in% virus_type)  %>%
                       distinct(epid, measurement, .keep_all = T) %>%
                       mutate(
                         m_sub = case_when(
                           measurement == "WILD 1" ~ "WPV1",
                           measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3") ~ "cVDPV"),
                         m_sub2  = case_when(
                           measurement == "WILD 1" ~ "WPV1",
                           measurement == "cVDPV 1" ~ "cVDPV1",
                           measurement == "cVDPV 2" ~ "cVDPV2",
                           measurement == "cVDPV 3" ~ "cVDPV3"))


# Add Overall Summary 

t0 <- p1 %>%
  group_by(place.admin.0, m_sub2) %>%
  count() %>%
  group_by(place.admin.0) %>%
  count() %>%
  ungroup() %>%
  summarise("Affected Countries" = length(place.admin.0)) %>%
  mutate("m_sub2" = "Overall")

t0 <- p1 %>%
  filter(source == "AFP") %>%
  summarise("Total Cases" = n()) %>%
  cbind(t0)

t0 <- p1 %>%
  filter(source == "ENV") %>%
  summarise("Total ES Detections" = n()) %>%
  cbind(t0)

c0 <- p1 %>%
  filter(
    source == "AFP" &
      report_date <= date_3b &
      (report_date >= date_3a |
         (report_date >= date_3a_a &
            place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN"))))
if(nrow(c0) == 0){
  t0 <- t0 %>%
    mutate("New Cases" = 0)
}else{
  t0 <-  c0 %>%
    summarise("New Cases" = n()) %>%
    cbind(t0)
}

c0 <- p1 %>%
  filter(
    source == "ENV" &
      report_date <= date_3b &
      (report_date >= date_3a |
         (report_date >= date_3a_a &
            place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN"))))
if(nrow(c0) == 0){
  t0 <- t0 %>%
    mutate("New ES Detections" = 0)
}else{
  t0 <-  c0 %>%
    summarise("New ES Detections" = n()) %>%
    cbind(t0)
}

# Table 1:
t1 <- p1 %>%
       group_by(place.admin.0, m_sub2) %>%
       count() %>%
       group_by(m_sub2) %>%
       count() %>%
       rename("Affected Countries" = n)

t1 <- p1 %>%
          filter(source == "AFP") %>%
          group_by(m_sub2) %>%
          count()%>%
          rename("Total Cases" = n) %>%
          left_join(t1, ., by = "m_sub2")

t1 <- p1 %>%
  filter(source == "ENV") %>%
  group_by(m_sub2) %>%
  count()%>%
  rename("Total ES Detections" = n)%>%
  left_join(t1, ., by = "m_sub2")


c0 <- p1 %>%
  filter(
           source == "AFP" &
           report_date <= date_3b &
           (report_date >= date_3a |
              (report_date >= date_3a_a &
                 place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN"))))
if(nrow(c0) == 0){
  t1 <- t1 %>%
    mutate("New Cases" = 0)
}else{
  t1 <-  c0 %>%
    group_by(m_sub2) %>%
    count() %>%
    rename("New Cases" = n) %>%
    left_join(t1, ., by = "m_sub2")
}

c0 <- p1 %>%
  filter(
    source == "ENV" &
      report_date <= date_3b &
      (report_date >= date_3a |
         (report_date >= date_3a_a &
            place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN"))))
if(nrow(c0) == 0){
  t1 <- t1 %>%
    mutate("New ES Detections" = 0)
}else{
  t1 <-  c0 %>%
    group_by(m_sub2) %>%
    count() %>%
    rename("New ES Detections" = n) %>%
    left_join(t1, ., by = "m_sub2")
    
}

t1 <- t1 %>% replace(is.na(.), 0)
t1 <- bind_rows(t1, t0)
assign(paste0("t1","_","overall_sum", sep = ""), t1 )
rm(t1, t0)

# Table 2 Code
for (x in v2){

# Base for table
t2 <- p1 %>%
        filter(m_sub2 == x) %>%
        group_by(place.admin.0) %>%
        count() %>%
        select(place.admin.0)

# New AFP Cases
c1 <- p1 %>%
  filter(m_sub2 == x &
         source == "AFP" &
           report_date <= date_3b &
           (report_date >= date_3a |
              (report_date >= date_3a_a &
                 place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN"))))

if(nrow(c1) == 0){
  t2 <- t2 %>%
          mutate("New Cases" = 0)
}else{
 t2 <-  p1 %>%
    filter(m_sub2 == x &
           source == "AFP" &
           report_date <= date_3b &
           (report_date >= date_3a |
             (report_date >= date_3a_a &
              place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN")))) %>%
    group_by(place.admin.0) %>%
    count() %>%
    rename("New Cases" = n) %>%
    left_join(t2, ., by = "place.admin.0")
}

# New ENV Cases
c1 <- p1 %>%
  filter(m_sub2 == x &
           source == "ENV" &
           report_date <= date_3b &
           (report_date >= date_3a |
              (report_date >= date_3a_a &
                 place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN"))))

if(nrow(c1) == 0){
  t2 <- t2 %>%
    mutate("New ES Detections" = 0)
}else{
  t2 <-  p1 %>%
    filter(m_sub2 == x &
             source == "ENV" &
             report_date <= date_3b &
             (report_date >= date_3a |
                (report_date >= date_3a_a &
                   place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN")))) %>%
    group_by(place.admin.0) %>%
    count() %>%
    rename("New ES Detections" = n) %>%
    left_join(t2, ., by = "place.admin.0")
}

# Add in total counts
# New ENV Cases
c1 <- p1 %>%
  filter(m_sub2 == x &
           source == "AFP")


if(nrow(c1) == 0){
  t2 <- t2 %>%
    mutate("Total Cases" = 0)
}else{
  t2 <-  p1 %>%
    filter(m_sub2 == x &
             source == "AFP") %>%
    group_by(place.admin.0) %>%
    count() %>%
    rename("Total Cases" = n) %>%
    left_join(t2, ., by = "place.admin.0")
}

c1 <- p1 %>%
  filter(m_sub2 == x &
           source == "ENV")


if(nrow(c1) == 0){
  t2 <- t2 %>%
    mutate("Total ES Detections" = 0)
}else{
  t2 <-  p1 %>%
    filter(m_sub2 == x &
             source == "ENV") %>%
    group_by(place.admin.0) %>%
    count() %>%
    rename("Total ES Detections" = n) %>%
    left_join(t2, ., by = "place.admin.0")
}

t2 <- t2 %>% replace(is.na(.), 0)

t2 <- p1 %>%
          filter(m_sub2 == x) %>%
          arrange(place.admin.0, dateonset) %>%
          group_by(place.admin.0) %>%
          summarise(most_recent = last(dateonset)) %>%
          rename("Most Recent" = most_recent) %>%
          left_join(t2, ., by = "place.admin.0") %>%
          arrange(desc(`Most Recent`)) %>%
          select(place.admin.0, `New Cases`, `Total Cases`, `New ES Detections`, `Total ES Detections`, `Most Recent`) %>%
          mutate(
            place.admin.0 = str_to_title(place.admin.0),
            place.admin.0 = case_when(
              place.admin.0 == "Cote D Ivoire" ~ "Côte d'Ivoire",
              place.admin.0 =="Democratic Republic Of The Congo" ~ "Democratic Republic of the Congo",
              place.admin.0 =="Occupied Palestinian Territory, Including East Jerusalem" ~ "Palestinian Territories",
              place.admin.0 =="Congo" ~ "Republic of the Congo",
              TRUE ~ place.admin.0))

assign(paste0("t2","_",x, sep = ""), t2 )
remove(t2)
}

# T2 Overall Table for Display 
# Base for table
t2_overall <- p1 %>%
  group_by(place.admin.0, m_sub2) %>%
  count() %>%
  select(place.admin.0, m_sub2)

# New AFP Cases
c1 <- p1 %>%
  filter(
           source == "AFP" &
           report_date <= date_3b &
           (report_date >= date_3a |
              (report_date >= date_3a_a &
                 place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN"))))

if(nrow(c1) == 0){
  t2_overall <- t2_overall %>%
    mutate("New Cases" = 0)
}else{
  t2_overall <-  p1 %>%
    filter(
             source == "AFP" &
             report_date <= date_3b &
             (report_date >= date_3a |
                (report_date >= date_3a_a &
                   place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN")))) %>%
    group_by(place.admin.0, m_sub2) %>%
    count() %>%
    rename("New Cases" = n) %>%
    left_join(t2_overall, ., by = c("place.admin.0", "m_sub2"))
}

# New ENV Cases
c1 <- p1 %>%
  filter(
           source == "ENV" &
           report_date <= date_3b &
           (report_date >= date_3a |
              (report_date >= date_3a_a &
                 place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN"))))

if(nrow(c1) == 0){
  t2_overall <- t2_overall %>%
    mutate("New ES Detections" = 0)
}else{
  t2_overall <-  p1 %>%
    filter(
             source == "ENV" &
             report_date <= date_3b &
             (report_date >= date_3a |
                (report_date >= date_3a_a &
                   place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN")))) %>%
    group_by(place.admin.0, m_sub2) %>%
    count() %>%
    rename("New ES Detections" = n) %>%
    left_join(t2_overall, ., by = c("place.admin.0", "m_sub2"))
}

# Add in total counts
# New AFP Cases
c1 <- p1 %>%
  filter(
           source == "AFP")


if(nrow(c1) == 0){
  t2_overall <- t2_overall %>%
    mutate("Total Cases" = 0)
}else{
  t2_overall <-  p1 %>%
    filter(
             source == "AFP") %>%
    group_by(place.admin.0, m_sub2) %>%
    count() %>%
    rename("Total Cases" = n) %>%
    left_join(t2_overall, ., by = c("place.admin.0", "m_sub2"))
}


# ENV Counts 
c1 <- p1 %>%
  filter(
           source == "ENV")


if(nrow(c1) == 0){
  t2_overall <- t2_overall %>%
    mutate("Total ES Detections" = 0)
}else{
  t2_overall <-  p1 %>%
    filter(
             source == "ENV") %>%
    group_by(place.admin.0, m_sub2) %>%
    count() %>%
    rename("Total ES Detections" = n) %>%
    left_join(t2_overall, ., by = c("place.admin.0", "m_sub2"))
}

t2_overall <- t2_overall %>% replace(is.na(.), 0)

t2_overall <- p1 %>%
  arrange(place.admin.0, m_sub2, dateonset) %>%
  group_by(place.admin.0, m_sub2) %>%
  summarise(most_recent = last(dateonset)) %>%
  rename("Most Recent" = most_recent) %>%
  left_join(t2_overall, ., by = c("place.admin.0", "m_sub2")) %>%
  arrange(desc(`Most Recent`)) %>%
  rename("Virus Type" = m_sub2) %>%
  select(place.admin.0, `New Cases`, `Total Cases`, `New ES Detections`, `Total ES Detections`, `Most Recent`) %>%
  mutate(
    place.admin.0 = str_to_title(place.admin.0),
    place.admin.0 = case_when(
      place.admin.0 == "Cote D Ivoire" ~ "Côte d'Ivoire",
      place.admin.0 =="Democratic Republic Of The Congo" ~ "Democratic Republic of the Congo",
      place.admin.0 =="Occupied Palestinian Territory, Including East Jerusalem" ~ "Palestinian Territories",
      place.admin.0 =="Congo" ~ "Republic of the Congo",
      TRUE ~ place.admin.0))



# Map <- all AFP and ES Detections
t3_map <-
     p1 %>%
      select(epid, m_sub2, dateonset, place.admin.0, place.admin.1, place.admin.2, source, longitude, latitude)


set_week_start(7)

# Epicurve Data By - all by Serotype
t4_epi_all <- p1 %>%
         mutate(e_date = floor_date(dateonset, "week", week_start = 7)) %>%
         group_by(e_date, m_sub2) %>%
         count() %>%
         mutate(wk_date = date2week(e_date),
                m_sub2 = factor(m_sub2, levels = c("cVDPV3", "cVDPV1", "cVDPV2", "WPV1")))


t4_epi_afp <- p1 %>%
  filter(source == "AFP") %>%
  mutate(e_date = floor_date(dateonset, "week", week_start = 7)) %>%
  group_by(e_date, m_sub2) %>%
  count() %>%
  mutate(wk_date = date2week(e_date),
         m_sub2 = factor(m_sub2, levels = c("cVDPV3", "cVDPV1", "cVDPV2", "WPV1")))


t4_epi_es <- p1 %>%
  filter(source == "ENV") %>%
  mutate(e_date = floor_date(dateonset, "week", week_start = 7)) %>%
  group_by(e_date, m_sub2) %>%
  count() %>%
  mutate(wk_date = date2week(e_date),
         m_sub2 = factor(m_sub2, levels = c("cVDPV3", "cVDPV1", "cVDPV2", "WPV1")))


# Export
dfs <- list("T1_PVOverall" = t1_overall_sum,
            "T2_overall" = t2_overall,
            "T2_cVDPV1" = t2_cVDPV1,
            "T2_cVDPV2" = t2_cVDPV2,
            "T2_cVDPV3" = t2_cVDPV3,
            "T2_WPV1" = t2_WPV1,
            "T3_map" = t3_map,
            "T4_epi_overall" = t4_epi_all,
            "T4_epi_afp" = t4_epi_afp,
            "T4_epi_es" = t4_epi_es)

if(!dir.exists(here("output"))){
  dir.create(here("output"))
}

write_xlsx(dfs, path = here(paste0("output/cdcready_polio_main_page_",format(end_date, "%y%m%d"),".xlsx", sep = "")))

# g1a <- ggplot(t4_epi_all, aes(e_date, n, fill = m_sub2)) +
#      geom_bar(stat = "identity") +
#      scale_x_date(date_breaks="2 week", date_labels = "%Y-%W") +
#      guides(fill=guide_legend(title="Virus Serotype:")) + 
#      theme_bw() +
#      theme(axis.text.x  = element_text(angle = 45, hjust = 1),
#           legend.position = "bottom") + 
#      labs(y = "No. of detections (AFP / ES)", x = "Date of Paralysis Onset / ES Collection (Epi Week)") + 
#      scale_y_continuous(breaks = seq(0,50,10))
#   
# ggsave(g1a, filename = "test1.png", width = 8, heigh = 4, scale = 1.5, dpi = 300)
