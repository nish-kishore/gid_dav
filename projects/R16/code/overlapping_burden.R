#dependencies
library(tidyverse)
library(here)
library(sf)
library(legendry)
library(geosphere)

#read in and process data

vpd.data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/country_vpd_year_casedata.rds")
ctry.sp <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/ctry_shapes.rds")

data <- vpd.data |>
  select(iso3_code, country_name, year, vpd, variable, value, gid_critical_countries) |>
  filter(vpd %in% c("Mpox", "Measles", "Diptheria", "Rubella", "Poliomyelitis", "Yellow fever"),
         variable == "cases",
         year >= 2019, year <= 2024) |>
  group_by(iso3_code, country_name, vpd, variable, gid_critical_countries) |>
  summarise(value = sum(value)) |>
  ungroup() |>
  mutate(country_name = case_when(
    country_name == "Iran (Islamic Republic of)" ~ "Iran",
    country_name == "Bolivia (Plurinational State of)" ~ "Bolivia",
    country_name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country_name == "Congo" ~ "Republic of the Congo",
    country_name == "Timor-Leste" ~ "East Timor",
    country_name == "Viet Nam" ~ "Vietnam",
    T ~ country_name
  ))

priority.countries <- vpd.data |>
  filter(gid_critical_countries == "Yes") |>
  pull(country_abbrev) |> unique() |> sort()

#helper functions
sf_point_parsing <- function(ctry.info, jitter.scale){

  type <- unique(ctry.info$type)
  n.vpd <- nrow(ctry.info)
  h <- unique(ctry.info$height)
  w <- unique(ctry.info$width)
  centroid <- st_coordinates(ctry.info) |> unique()
  x <- centroid[1]
  y <- centroid[2]



  if(type == "long"){

    t.mat <- switch(as.character(n.vpd),
                    "2" = {matrix(c(0.5,-0.5),nrow = 2)},
                    "3" = {matrix(c(1,0,-1),nrow = 3)},
                    "4" = {matrix(c(1.5,0.5,-0.5,-1.5), nrow = 4)},
                    "5" = {matrix(c(1.5,0.5,-0.5,-1.5,-2.5), nrow = 5)}
    )

    return(
      ctry.info |>
        as_tibble() |>
        select(-geometry) |>
        bind_cols("lat" = as.numeric(y + (h/jitter.scale / 6370000) * (180 / pi) * t.mat)) |>
        mutate("lon" = x) |>
        st_as_sf(coords = c("x" = "lon", "y" = "lat"), crs = st_crs(ctry.info)))

  }

  if(type == "wide"){

    t.mat <- switch(as.character(n.vpd),
                    "2" = {matrix(c(-0.5,0.5),nrow = 2)},
                    "3" = {matrix(c(-1,0,1),nrow = 3)},
                    "4" = {matrix(c(-1.5,-0.5,0.5,1.5), nrow = 4)},
                    "5" = {matrix(c(-1.5,-0.5,0.5,1.5, 2.5), nrow = 5)}
    )

    return(
      ctry.info |>
        as_tibble() |>
        select(-geometry) |>
        bind_cols("lon" = as.numeric(x + (h/jitter.scale / 6370000) * (180 / pi) * t.mat / cos(x * pi/180))) |>
        mutate("lat" = y) |>
        st_as_sf(coords = c("x" = "lon", "y" = "lat"), crs = st_crs(ctry.info)))


  }

  if(type == "square"){

    t.mat.x <- switch(as.character(n.vpd),
                      "2" = {matrix(c(-0.5,0.5),nrow = 2)},
                      "3" = {matrix(c(-0.5,0.5,-0.5),nrow = 3)},
                      "4" = {matrix(c(-0.5,0.5,-0.5,0.5), nrow = 4)},
                      "5" = {matrix(c(-0.5,0.5,-0.5,0.5,-0.5), nrow = 5)}

    )

    t.mat.y <- switch(as.character(n.vpd),
                      "2" = {matrix(c(0,0),nrow = 2)},
                      "3" = {matrix(c(0.5,0.5,-0.5),nrow = 3)},
                      "4" = {matrix(c(0.5,0.5,-0.5,-0.5), nrow = 4)},
                      "5" = {matrix(c(0.5,0.5,-0.5,-0.5,-1.5), nrow = 5)}

    )

    return(
      ctry.info |>
        as_tibble() |>
        select(-geometry) |>
        bind_cols("lon" = as.numeric(x + (h/jitter.scale / 6370000) * (180 / pi) * t.mat.x/ cos(x * pi/180))) |>
        mutate("lat" = as.numeric(y + (h/jitter.scale / 6370000) * (180 / pi) * t.mat.y)) |>
        st_as_sf(coords = c("x" = "lon", "y" = "lat"), crs = st_crs(ctry.info)))


  }

}

create_vpd_burden_map <- function(coi, ctry.sp, data){

  #subsetting spatial file to country of interest
  ctry <- ctry.sp |> filter(iso_a3 == coi)

  #identify all neighboring countries
  neighboring.ctry <- ctry.sp[
    ctry.sp |>
      filter(iso_a3 == coi) |>
      st_intersects(ctry.sp) |>
      as_vector(),] |>
    filter(iso_a3 != coi,
           region_un == ctry$region_un)

  #fixing country specific neighbors
  if(coi == "PHL"){
    neighboring.ctry <- ctry.sp |>
      filter(admin %in% c("Taiwan", "Vietnam", "Indonesia", "Malaysia", "China", "Japan"))
  }

  if(coi == "IDN"){
    neighboring.ctry <- ctry.sp |>
      filter(admin %in% c("Papua New Guinea", "Malaysia", "East Timor"))
  }

  if(coi == "PAK"){
    neighboring.ctry <- neighboring.ctry |> filter(admin != "Siachen Glacier")
  }

  #calculate spatial centers of each country
  ctry.centers <- bind_rows(ctry, neighboring.ctry) |>
    st_centroid()

  #create zoomed bounding box
  plot.bbox <- bind_rows(ctry, neighboring.ctry) |> st_bbox() |> sirfunctions::f.expand.bbox(X = 100000, Y = 100000)

  #create joint dataset for visualization
  plot.data <- data |>
    filter(iso3_code %in% c(coi, neighboring.ctry$iso_a3)) |>
    left_join(ctry.centers |> select(iso_a3), by = c("iso3_code" = "iso_a3")) |>
    mutate(
      vix_val = case_when(
        nchar(value) > 0 & nchar(value) <= 3 ~ as.character(value),
        nchar(value) > 3 & nchar(value) <= 6 ~ paste0(round(value/1000,0),"K"),
        nchar(value) > 6 & nchar(value) <= 9 ~ paste0(round(value/1000000,0),"M")
      ),
      vix_lvl = case_when(
        str_detect(vix_val, "K") ~ 1.75,
        str_detect(vix_val, "M") ~ 2,
        T ~ 1.5
      )
    ) |>
    filter(value > 0) |>
    st_as_sf()

  #determine if each country is long, wide or square
  ctry.shape.types <- bind_rows(ctry, neighboring.ctry) |>
    rowwise() |>
    group_split() |>
    lapply(function(x){
      st_bbox(x) %>%
        {
          tibble(
            "ctry" = x$admin,
            "height" = as.integer(distm(.[c("xmin", "ymin")], .[c("xmin", "ymax")])),
            "width" = as.integer(distm(.[c("xmin", "ymin")], .[c("xmax", "ymin")]))
          ) |>
            mutate(ratio = height/width)
        }
    }) |>
    bind_rows() |>
    mutate(type = case_when(
      ratio <= 0.5 ~ "wide",
      ratio > 0.5 & ratio < 1.5 ~ "square",
      ratio >= 1.5 ~ "long"
    ))

  #use helper function to calculate optimal point placement
  plot.data.ordered <- left_join(plot.data, ctry.shape.types, by = c("country_name" = "ctry")) |>
    group_by(country_name) |>
    arrange(vpd) |>
    group_split() |>
    lapply(function(x) sf_point_parsing(ctry.info = x, jitter.scale = 6)) |>
    bind_rows()

  ggplot() +
    geom_sf(data = ctry.sp, color = "grey40") +
    geom_sf(data = ctry, fill = "lightgoldenrod3", color = "black", linewidth = 1.05) +
    geom_sf(data = neighboring.ctry, fill = "lightgoldenrod1", color = "black") +
    geom_sf(data = plot.data.ordered, aes(fill = vpd, size = value), color = "grey20", shape = 21) +
    geom_sf_text(data = plot.data.ordered, aes(label = vix_val), size = 2, fontface = "bold") +
    scale_size_continuous(
      range = c(5,15), labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      breaks = c(100, 1000, 10000, 100000, 1000000)) +
    # scale_size_continuous(
    # range = c(5,8),
    # breaks = c(1.5, 1.75)) +
    coord_sf(xlim = plot.bbox[c("xmin", "xmax")], ylim = plot.bbox[c("ymin", "ymax")]) +
    theme_bw() +
    labs(
      fill = "VPD",
      size = "# cases",
      title = paste0("Burden of select vaccine preventable diseases (VPDs) in ", filter(ctry.sp, iso_a3 == coi) |> pull(admin), " and neighboring countries"),
      subtitle = paste0("Cumulative cases between 2019-2024\nNeighboring countries include: ", neighboring.ctry$admin |> sort() |> paste0(collapse = ", ")),
      caption = "Bubbles are placed in sequence on the map and do not reflect actual locations of outbreaks"
    ) +
    theme(panel.background = element_rect(fill = "lightblue"),
          panel.ontop = FALSE,
          legend.key = element_rect(fill = NA),
          axis.title = element_blank()) +
    guides(fill = guide_legend(override.aes = list(size = 8)))


}

#run function
create_vpd_burden_map(coi = "NGA", ctry.sp = ctry.sp, data = data)
create_vpd_burden_map(coi = "AFG", ctry.sp = ctry.sp, data = data)
create_vpd_burden_map(coi = "BRA", ctry.sp = ctry.sp, data = data)
create_vpd_burden_map(coi = "COD", ctry.sp = ctry.sp, data = data)
create_vpd_burden_map(coi = "ETH", ctry.sp = ctry.sp, data = data)
create_vpd_burden_map(coi = "IDN", ctry.sp = ctry.sp, data = data)
create_vpd_burden_map(coi = "PAK", ctry.sp = ctry.sp, data = data)
create_vpd_burden_map(coi = "PHL", ctry.sp = ctry.sp, data = data)

