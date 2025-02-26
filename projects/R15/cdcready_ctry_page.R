## CDC Ready - Country Specific Pages
##
# Create List for Data Set Compression
#initiate a list



##############
library(here)
source(here("reference/obx_packages_01.R"))
library(aweek)

# Get data
raw.data <- get_all_polio_data()

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


# Set week start date for epi curve
set_week_start(7)


# Get list of countries
ctry_all <- raw.data$pos %>%
               filter(
                 dateonset >= start_date &
                   dateonset <= end_date &
                   source %in% s_source  &
                   measurement %in% virus_type) %>%
            distinct(place.admin.0) %>%
            arrange(place.admin.0) %>%
            as_vector()

# Set up Data:
p0 <- raw.data$pos %>% filter(
  dateonset >= start_date &
    dateonset <= end_date &
    source %in% s_source &
    measurement %in% virus_type)  %>%
  mutate(
    m_sub = case_when(
      measurement == "WILD 1" ~ "WPV1",
      measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3") ~ "cVDPV"),
    m_sub2  = case_when(
      measurement == "WILD 1" ~ "WPV1",
      measurement == "cVDPV 1" ~ "cVDPV1",
      measurement == "cVDPV 2" ~ "cVDPV2",
      measurement == "cVDPV 3" ~ "cVDPV3"))


for (i in ctry_all) {
  list_namet <- paste0("pv_", str_replace_all(i, " ", "_"))

  p1 <- p0 %>%
    filter(place.admin.0 == i)



# Table One & Table 2 Counts by SubRegion <- New Cases / Total Cases
for (x in v2){

  # Base for table
  t1 <- p1 %>%
    filter(m_sub2 == x) %>%
    group_by(place.admin.0) %>%
    count() %>%
    select(place.admin.0)


  t2 <- raw.data$global.prov %>%
         filter(ADM0_NAME == i &
                ENDDATE == "9999-12-31") %>%
         as_tibble() %>%
         select(ADM1_NAME)



  # New AFP Cases
  c1 <- p1 %>%
    filter(m_sub2 == x &
             source == "AFP" &
             report_date <= date_3b &
             (report_date >= date_3a |
                (report_date >= date_3a_a &
                   place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN"))))


  if(nrow(c1) == 0){
    t1 <- t1 %>%
      mutate("New Cases" = 0)
    t2 <- t2 %>%
      mutate("New Cases" = 0)
  }else{
    t1 <-  p1 %>%
      filter(m_sub2 == x &
               source == "AFP" &
               report_date <= date_3b &
               (report_date >= date_3a |
                  (report_date >= date_3a_a &
                     place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN")))) %>%
      group_by(place.admin.0) %>%
      count() %>%
      rename("New Cases" = n)

    t2 <-  p1 %>%
      filter(m_sub2 == x &
               source == "AFP" &
               report_date <= date_3b &
               (report_date >= date_3a |
                  (report_date >= date_3a_a &
                     place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN")))) %>%
      group_by(place.admin.1) %>%
      count() %>%
      left_join(t2,., by = c("ADM1_NAME"="place.admin.1")) %>%
      rename("New Cases" = n)


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
    t1 <- t1 %>%
      mutate("New ES Detections" = 0)

    t2 <- t2 %>%
      mutate("New ES Detections" = 0)
  }else{
    t1 <-  p1 %>%
      filter(m_sub2 == x &
               source == "ENV" &
               report_date <= date_3b &
               (report_date >= date_3a |
                  (report_date >= date_3a_a &
                     place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN")))) %>%
      group_by(place.admin.0) %>%
      count() %>%
      rename("New ES Detections" = n) %>%
      left_join(t1, ., by = "place.admin.0")

    t2 <-  p1 %>%
      filter(m_sub2 == x &
               source == "ENV" &
               report_date <= date_3b &
               (report_date >= date_3a |
                  (report_date >= date_3a_a &
                     place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN")))) %>%
      group_by(place.admin.1) %>%
      count() %>%
      rename("New ES Detections" = n) %>%
      left_join(t2, ., by = c("ADM1_NAME"="place.admin.1"))
  }

  # Add in total counts
  c1 <- p1 %>%
    filter(m_sub2 == x &
             source == "AFP")


  if(nrow(c1) == 0){
    t1 <- t1 %>%
      mutate("Total Cases" = 0)
    t2 <- t2 %>%
      mutate("Total Cases" = 0)
  }else{
    t1 <-  p1 %>%
      filter(m_sub2 == x &
               source == "AFP") %>%
      group_by(place.admin.0) %>%
      count() %>%
      rename("Total Cases" = n) %>%
      left_join(t1, ., by = "place.admin.0")

    t2 <-  p1 %>%
      filter(m_sub2 == x &
               source == "AFP") %>%
      group_by(place.admin.1) %>%
      count() %>%
      rename("Total Cases" = n) %>%
      left_join(t2, ., by = c("ADM1_NAME"="place.admin.1"))
  }

  c1 <- p1 %>%
    filter(m_sub2 == x &
             source == "ENV")


  if(nrow(c1) == 0){
    t1 <- t1 %>%
      mutate("Total ES Detections" = 0)
    t2 <- t2 %>%
      mutate("Total ES Detections" = 0)
  }else{
    t1 <-  p1 %>%
      filter(m_sub2 == x &
               source == "ENV") %>%
      group_by(place.admin.0) %>%
      count() %>%
      rename("Total ES Detections" = n) %>%
      left_join(t1, ., by = "place.admin.0")

    t2 <-  p1 %>%
      filter(m_sub2 == x &
               source == "ENV") %>%
      group_by(place.admin.1) %>%
      count() %>%
      rename("Total ES Detections" = n) %>%
      left_join(t2, ., by = c("ADM1_NAME"="place.admin.1"))
  }

  t1 <- t1 %>% replace(is.na(.), 0)
  t2 <- t2 %>% replace(is.na(.), 0)

  t1 <- p1 %>%
    filter(m_sub2 == x) %>%
    arrange(place.admin.0, dateonset) %>%
    group_by(place.admin.0) %>%
    summarise(most_recent = last(dateonset)) %>%
    rename("Most Recent" = most_recent) %>%
    left_join(t1, ., by = "place.admin.0") %>%
    ungroup() %>%
    select(`New Cases`, `Total Cases`, `New ES Detections`, `Total ES Detections`, `Most Recent`)

  t2 <- p1 %>%
    filter(m_sub2 == x) %>%
    arrange(place.admin.1, dateonset) %>%
    group_by(place.admin.1) %>%
    summarise(most_recent = last(dateonset)) %>%
    left_join(t2, ., by = c("ADM1_NAME"="place.admin.1")) %>%
    rename("Most Recent" = most_recent,
           "Province" = ADM1_NAME) %>%
    ungroup() %>%
    select( `Province`, `New Cases`, `Total Cases`, `New ES Detections`, `Total ES Detections`, `Most Recent`)


    assign(paste0(x,"_", "t1", sep = ""), t1 )
    assign(paste0(x,"_", "t2", sep = ""), t2 )

# Add in table 1b code here
t1b <-
      raw.data$global.prov %>% filter(
        ADM0_NAME == i  &
          ENDDATE == "9999-12-31") %>%
      as_tibble() %>%
      group_by(ADM0_NAME) %>%
      summarise(prov_count = n())

# Detections within 13 months
t1b <- p1 %>%
      filter(m_sub2 == x &
            dateonset >= start_date) %>%
      group_by(place.admin.0, place.admin.1) %>%
      count() %>%
      group_by(place.admin.0) %>%
      summarise(
        impacted_prov13m = n()) %>%
    left_join(t1b, . , by = c("ADM0_NAME"="place.admin.0"))

# Detections between 6 and 13 months
t1b <- p1 %>%
      filter(m_sub2 == x &
               dateonset >= start_date &
               dateonset <= (end_date %m-% months(6))) %>%
      group_by(place.admin.0, place.admin.1) %>%
      count() %>%
      group_by(place.admin.0) %>%
      summarise(
        impacted_prov613m = n()) %>%
     left_join(t1b, . , by = c("ADM0_NAME"="place.admin.0"))

# Detections in the last six months
t1b <- p1 %>%
  filter(m_sub2 == x &
           dateonset > (end_date %m-% months(6))) %>%
  group_by(place.admin.0, place.admin.1) %>%
  count() %>%
  group_by(place.admin.0) %>%
  summarise(
    impacted_prov6 = n()) %>%
  left_join(t1b, . , by = c("ADM0_NAME"="place.admin.0")) %>%
  replace(is.na(.), 0)


t1b <- t1b %>%
  mutate(
         prov_pct13 = round(impacted_prov13m / prov_count * 100, 0),
         prov_pct613 = round(impacted_prov613m / prov_count * 100, 0),
         prov_pct6 = round(impacted_prov6 / prov_count * 100, 0),
         "No of Prov w/ pv < 13ms (%)" = paste(impacted_prov13m,  " (", prov_pct13, ")", sep = ""),
         "No of Prov w/ pv btw 6-13ms (%)" = paste(impacted_prov613m,  " (", prov_pct613, ")", sep = ""),
         "No of Prov w/ pv <6ms (%)" = paste(impacted_prov6, " (", prov_pct6, ")", sep = ""),
         newob_6m = impacted_prov13m -impacted_prov613m,
         newob_6mpct = round(newob_6m / prov_count * 100, 0),
         "No of New Provs w/ pv <6ms (%)" = paste(newob_6m, " (", newob_6mpct, ")", sep = "")) %>%
  rename("No of Provs" = prov_count) %>%
  select("No of Provs",  "No of Prov w/ pv < 13ms (%)", "No of Prov w/ pv btw 6-13ms (%)",
                    "No of Prov w/ pv <6ms (%)",  "No of New Provs w/ pv <6ms (%)")


assign(paste0(x,"_", "t1b", sep = ""), t1b )


}

# Table 3 - Epicurve
c2 <- p1 %>%
  filter(source == "AFP")

if(nrow(c2) == 0){
  t3 <- p1 %>%
    filter(m_sub2 == x) %>%
    group_by(place.admin.0) %>%
    count() %>%
    select(place.admin.0)
  t3 <- t3  %>%
    mutate("AFP Cases" = "No AFP cases in <13ms")
}else{
  t3 <- c2 %>%
    mutate(e_date = floor_date(dateonset, "week", week_start = 7)) %>%
    group_by(e_date, m_sub2) %>%
    count() %>%
    mutate(wk_date = date2week(e_date),
           m_sub2 = factor(m_sub2, levels = c("cVDPV3", "cVDPV1", "cVDPV2", "WPV1")))
}

  t3 <- ungroup(t3)
  assign("epicurve_t3", t3 )



  assign(list_namet, list(WPV1_t1 = WPV1_t1,
                          WPV1_t1b = WPV1_t1b,
                          WPV1_t2 = WPV1_t2,
                          cVDPV1_t1 = cVDPV1_t1,
                          cVDPV1_t1b = cVDPV1_t1b,
                          cVDPV1_t2 = cVDPV1_t2,
                          cVDPV2_t1 = cVDPV2_t1,
                          cVDPV2_t1b = cVDPV2_t1b,
                          cVDPV2_t2 = cVDPV2_t2,
                          cVDPV3_t1 = cVDPV3_t1,
                          cVDPV3_t1b = cVDPV3_t1b,
                          cVDPV3_t2 = cVDPV3_t2,
                          epicurve_t3  = epicurve_t3))

  remove(c1, c2, p1, t1, t1b, t2, t3,
                     cVDPV1_t1, cVDPV2_t1, cVDPV3_t1, WPV1_t1,
                     cVDPV1_t1b, cVDPV2_t1b, cVDPV3_t1b, WPV1_t1b,
                     cVDPV1_t2, cVDPV2_t2, cVDPV3_t2, WPV1_t2,
                     epicurve_t3)

}

remove(p0)





