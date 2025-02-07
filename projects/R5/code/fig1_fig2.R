#dependencies
library(tidyverse)
library(readxl)
library(rnaturalearth)
library(here)

#read in and process data

priority.countries <- c("ETH", "NGA", "COD", "IDN", "BRA", "PHL", "AFG", "PAK")
#
# data <- read_excel("C:/Users/ynm2/Downloads/Annual-Arrivals-2000-to-Present–Country-of-Residence.xlsx", sheet = "Annual") |>
#   slice_tail(n = 235) |>
#   select(-starts_with("...")) |>
#   select(-starts_with("Notes")) |>
#   set_names(c("ctry", "region", 2000:2023)) |>
#   mutate(ctry = case_when(
#     ctry == "Zaire ( formerly Congo, Democratic Republic of)" ~ "Democratic Republic of the Congo",
#     ctry == "Congo" ~ "Republic of the Congo",
#     ctry == "Tanzania" ~ "United Republic of Tanzania",
#     ctry == "Swaziland" ~ "eSwatini",
#     ctry == "South Sudan, Republic of" ~ "South Sudan",
#     ctry == "Côte d'Ivoire" ~ "Ivory Coast",
#     ctry == "Bahamas, the" ~ "The Bahamas",
#     ctry == "Burma (Myanmar)" ~ "Myanmar",
#     ctry == "Cape Verde" ~ "Cabo Verde",
#     ctry == "Curacao" ~ "Curaçao",
#     ctry == "Czech Republic" ~ "Czechia",
#     ctry == "Micronesia" ~ "Federated States of Micronesia",
#     ctry == "Holy See/Vatican" ~ "Vatican",
#     ctry == "Indian Ocean Territory" ~ "Indian Ocean Territories",
#     ctry == "Hong Kong" ~ "Hong Kong S.A.R.",
#     ctry == "Macao SAR" ~ "Macao S.A.R",
#     ctry == "Macedonia" ~ "North Macedonia",
#     T ~ ctry
#   )) |>
#   pivot_longer("2000":"2023") |>
#   rename("year" = "name", "count" = "value") |>
#   mutate(year = as.integer(year))
#
# ctry.sp <- ne_countries(type = "countries",
#                         scale = "medium", returnclass = "sf") |>
#   mutate(gid_priority = ifelse(iso_a3 %in% priority.countries, "Priority", NA)) |>
#   select(admin, gid_priority) |>
#   filter(admin != "Antarctica") |>
#   mutate(admin = case_when(
#     admin == "Sint Maarten" ~ "Saint Maarten",
#     T ~ admin
#   ))
#
# sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/traveler_outbound.rds", obj = data)
# sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/ctry_shapes.rds", obj = ctry.sp)

data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/traveler_outbound.rds")
ctry.sp <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/ctry_shapes.rds")

analytic.data <- left_join(data, ctry.sp, by = c("ctry" = "admin")) |> select(ctry, year, count, gid_priority) |>
  mutate(gid_priority = ifelse(is.na(gid_priority), "Not-priority", "Priority"))

trend <- lm(count ~ year + gid_priority + ctry, data = analytic.data) |> summary()

trend.coeff <- trend$coefficients

names <- row.names(trend.coeff)

trend.coeff <- as_tibble(trend.coeff) |> add_column("value" = names)

increase.in.travel <- trend.coeff |> filter(str_starts(value, "ctry")) |> mutate(value = str_replace_all(value, "ctry", "")) |>
  filter(`Pr(>|t|)` <= 0.05,
         !value %in% c("Mexico", "Canada")) |>
  mutate(trend = "Increasing")

#total number of travelers from priority countries
total_num <- left_join(ctry.sp, data |> filter(year == 2023), by = c("admin" = "ctry")) |> as_tibble() |> filter(!is.na(gid_priority)) |> summarize(total = sum(count)) |> pull(total)

#volume by country
left_join(ctry.sp, data |> filter(year == 2023), by = c("admin" = "ctry")) |>
  ggplot() +
  geom_sf(aes(fill = count)) +
  geom_sf(data = ctry.sp |> filter(!is.na(gid_priority)), color = "black", fill = NA, size = 1.1) +
  scale_fill_viridis_c(trans = scales::log10_trans(), labels = scales::comma, option = "cividis") +
  theme_bw() +
  labs(fill = "# travelers", title = "Number of international travelers to the USA in 2023 by country of origin",
       subtitle = paste0("GID Priority Countries (AFG, BRA, COD, ETH, IDN, NGA, PHL, PAK) outlined\n",scales::comma(total_num)," international travelers from priority countries in 2023"),
       caption = "Data from the National Travel and Tourism Office of the International Trade Administration\n(https://www.trade.gov/feature-article/ntto-releases-international-travel-statistics-2023)")

#volume by country not including Mexico and Canada
left_join(ctry.sp, data |> filter(year == 2023, !ctry %in% c("Mexico", "Canada")), by = c("admin" = "ctry")) |>
  ggplot() +
  geom_sf(aes(fill = count)) +
  geom_sf(data = ctry.sp |> filter(!is.na(gid_priority)), color = "red", fill = NA, size = 1.1) +
  scale_fill_viridis_c(trans = scales::log10_trans(), labels = scales::comma, option = "cividis") +
  theme_bw() +
  labs(fill = "# travelers", title = "Number of non-US citizens entering the USA in 2023 by country of origin not including Mexico and Canada",
       subtitle = paste0("GID Critical Countries (AFG, BRA, COD, ETH, IDN, NGA, PHL, PAK) outlined\n",scales::comma(total_num)," international travelers from priority countries in 2023"),
       caption = "Data from the National Travel and Tourism Office of the International Trade Administration\n(https://www.trade.gov/feature-article/ntto-releases-international-travel-statistics-2023)\nProduced by CDC-GHC-GID")

ggsave(here("projects/R5/outputs/fig1.png"), width = 10, height = 6, dpi = 300)

#countries with a significant and steady increase in # of travelers
left_join(ctry.sp, increase.in.travel, by = c("admin" = "value")) |>
  mutate(trend = ifelse(is.na(trend), "Static", "Increasing")) |>
  ggplot() +
  geom_sf(aes(fill = trend)) +
  geom_sf(data = ctry.sp |> filter(!is.na(gid_priority)), color = "red", fill = NA, size = 1.1) +
  scale_fill_manual(values = c(scales::muted("green"), "gray")) +
  theme_bw() +
  labs(fill = "Trend", title = "Trend of non-US citizen travel to the USA not including Mexico and Canada",
       subtitle = paste0("GID Priority Countries (AFG, BRA, COD, ETH, IDN, NGA, PHL, PAK) outlined\n",scales::comma(total_num)," international travelers from priority countries in 2023"),
       caption = "Data from the National Travel and Tourism Office of the International Trade Administration\n(https://www.trade.gov/feature-article/ntto-releases-international-travel-statistics-2023)")

ggsave(here("projects/R5/outputs/fig2.png"), width = 10, height = 6, dpi = 300)
