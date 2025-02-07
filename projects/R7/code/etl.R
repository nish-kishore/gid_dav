#dependencies
library(tidyverse)
library(sirfunctions)
library(waffle)
library(ggpubr)
library(here)

#data
# data <- read_csv("C:/Users/ynm2/Downloads/wuenic2023rev_data_2025-02-05.csv") |>
#   select(ctry = Name, year = Year, vaccine = Vaccine, coverage = Coverage, n_vacc = Vaccinated,
#          n_target = Target, n_miss = Num_missed) |>
#   mutate(
#     coverage = coverage/100,
#     n_vacc = str_replace_all(n_vacc, ",", "") |> as.integer(),
#     n_target = str_replace_all(n_target, ",", "") |> as.integer(),
#     n_miss = str_replace_all(n_miss, ",", "") |> as.integer()
#   )
#
# sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/wuenic2023.rds", obj = data)

data <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/wuenic2023.rds")

priority.countries <- c("Brazil", "Democratic Republic of the Congo", "Ethiopia",
                        "Indonesia", "Nigeria", "Philippines")

text.prop <- tibble(
  xpos = c(-Inf,-Inf,Inf,Inf),
  ypos =  c(-Inf, Inf,-Inf,Inf),
  annotateText = c("Bottom Left (h0,v0)","Top Left (h0,v1)"
                   ,"Bottom Right h1,v0","Top Right h1,v1"),
  hjustvar = c(0,0,1,1) ,
  vjustvar = c(0,1,0,1)) |>
  filter(str_starts(annotateText, "Bottom Right")) |>
  expand_grid(
    ctry = priority.countries,
    name = c("DTP1 Coverage", "Proportion Zero Dose")
  ) |>
  mutate(ctry = ifelse(ctry == "Democratic Republic of the Congo", "DRC", ctry)) |>
  left_join(
    data |>
      filter(ctry %in% priority.countries, vaccine == "DTP1") |>
      mutate(ctry = ifelse(ctry == "Democratic Republic of the Congo", "DRC", ctry)) |>
      mutate(p_miss = n_miss/n_target) |>
      select(ctry, year, "DTP1 Coverage" = coverage, "Proportion Zero Dose" = p_miss) |>
      pivot_longer(`DTP1 Coverage`:`Proportion Zero Dose`) |>
      filter(year == 2023) |>
      select(-year) |>
      mutate(value = value*100),
    by = c("name", "ctry")
  ) |>
  mutate(value = paste0(as.integer(value),"%"))

text.tot <- tibble(
  xpos = c(-Inf,-Inf,Inf,Inf),
  ypos =  c(-Inf, Inf,-Inf,Inf),
  annotateText = c("Bottom Left (h0,v0)","Top Left (h0,v1)"
                   ,"Bottom Right h1,v0","Top Right h1,v1"),
  hjustvar = c(0,0,1,1) ,
  vjustvar = c(0,1,0,1)) |>
  filter(str_starts(annotateText, "Bottom Right")) |>
  expand_grid(
    ctry = priority.countries,
    name = c("DTP1 Covered", "Number Zero Dose")
  ) |>
  mutate(ctry = ifelse(ctry == "Democratic Republic of the Congo", "DRC", ctry)) |>
  left_join(
    data |>
      filter(ctry %in% priority.countries, vaccine == "DTP1") |>
      mutate(ctry = ifelse(ctry == "Democratic Republic of the Congo", "DRC", ctry)) |>
      mutate(n_cov = round(coverage*n_target,0)) |>
      select(ctry, year, "DTP1 Covered" = n_cov, "Number Zero Dose" = n_miss) |>
      pivot_longer(`DTP1 Covered`:`Number Zero Dose`) |>
      filter(year == 2023) |>
      select(-year) |>
      mutate(value = round(value/1000,0)),
    by = c("name", "ctry")
  ) |>
  rowwise() |>
  mutate(value = case_when(
    nchar(value) == 3 ~ paste0(value, "K"),
    nchar(value) == 4 ~ paste0(round(value/1000,2), "M")
  )) |>
  filter(name == "Number Zero Dose")

#waffle figure as proportions
data |>
  filter(ctry %in% priority.countries, vaccine == "DTP1") |>
  mutate(ctry = ifelse(ctry == "Democratic Republic of the Congo", "DRC", ctry)) |>
  mutate(p_miss = n_miss/n_target) |>
  select(ctry, year, "DTP1 Coverage" = coverage, "Proportion Zero Dose" = p_miss) |>
  pivot_longer(`DTP1 Coverage`:`Proportion Zero Dose`) |>
  filter(year == 2023) |>
  select(-year) |>
  mutate(value = value*100) |>
  # group_by(ctry, name) |>
  # group_split() |>
  # lapply(function(x){x |> add_row(tibble("ctry" = x$ctry, name = NA, value = 100-x$value))}) |>
  # bind_rows() |>
  # filter(!is.na(name)) |>
  ggplot() +
  geom_waffle(aes(values = value, fill = name), color = "white", n_rows = 10) +
  geom_label(data=text.prop,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=value)) +
  facet_grid(name ~ ctry, drop = T) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(fill = "Type", title = "Proportion of children covered by DTP1 and proportion missed in GID critical countries in 2023",
       caption = "Data from 2023 WUENIC estimates\nProduced by CDC-GHC-GID")

ggsave(here("projects/R7/outputs/wuenic_fig1.png"), width = 10, height = 4, dpi = 300)

#waffle figure as totals
data |>
  filter(ctry %in% priority.countries, vaccine == "DTP1") |>
  mutate(ctry = ifelse(ctry == "Democratic Republic of the Congo", "DRC", ctry)) |>
  mutate(n_cov = round(coverage*n_target,0)) |>
  select(ctry, year, "DTP1 Covered" = n_cov, "Number Zero Dose" = n_miss) |>
  pivot_longer(`DTP1 Covered`:`Number Zero Dose`) |>
  filter(year == 2023) |>
  select(-year) |>
  mutate(value = value/10000) |>
  filter(name == "Number Zero Dose") |>
  # group_by(ctry, name) |>
  # group_split() |>
  # lapply(function(x){x |> add_row(tibble("ctry" = x$ctry, name = NA, value = 100-x$value))}) |>
  # bind_rows() |>
  # filter(!is.na(name)) |>
  ggplot() +
  geom_waffle(aes(values = value, fill = name), color = "white") +
  geom_label(data=text.tot,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=value)) +
  facet_grid(name ~ ctry, drop = T) +
  scale_fill_manual(values = "dodgerblue4") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(fill = "Type", title = "Number of zero dose children in GID critical countries in 2023",
       caption = "Data from 2023 WUENIC estimates\nProduced by CDC-GHC-GID")

ggsave(here("projects/R7/outputs/wuenic_fig2.png"), width = 10, height = 2, dpi = 300)

