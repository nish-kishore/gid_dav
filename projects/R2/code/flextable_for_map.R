library(flextable)
library(sirfunctions)
library(dplyr)
library(tidyr)

raw.data <- get_all_polio_data()
pos22.24 <- raw.data$pos |>
  filter(yronset %in% c("2022", "2023", "2024"))

summary.22.24 <- pos22.24 |>
  filter(measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3", "WILD 1") & source %in% c("AFP", "ENV")) |>
  distinct(epid, measurement, .keep_all =T) |>
  group_by(measurement, yronset, source) |>
  mutate(serotype_count = n()) |>
  ungroup() |>
  select(measurement, source, yronset, serotype_count) |>
  unique() |>
  pivot_wider(id_cols = measurement, names_from = c("source", "yronset"), values_from = serotype_count) |>
  arrange(measurement) |>
  mutate(AFP_2023 = ifelse(is.na(AFP_2023), 0, AFP_2023),
         ENV_2023 = ifelse(is.na(ENV_2023), 0, ENV_2023), 
         ENV_2024 = ifelse(is.na(ENV_2024), 0, ENV_2024), 
         measurement = case_when(measurement == "WILD 1" ~ "WPV1", 
                                 measurement == "cVDPV 1" ~ "cVDPV1", 
                                 measurement == "cVDPV 2" ~ "cVDPV2", 
                                 measurement == "cVDPV 3" ~ "cVDPV3")) |>
  select(measurement, AFP_2022, ENV_2022, AFP_2023, ENV_2023, AFP_2024, ENV_2024) 

table <- flextable(summary.22.24) 
table <- set_header_labels(table, measurement = NA, AFP_2022 = "Cases", ENV_2022 = "ES", AFP_2023 = "Cases", ENV_2023 = "ES", AFP_2024 = "Cases", ENV_2024 = "ES")
table <- add_header_row(table, values = c("", "2022", "2023", "2024"), colwidths = c(1, 2, 2, 2))
table <- align(table, part = "all", align = "center")
print(table)
