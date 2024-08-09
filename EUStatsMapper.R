# Project: REUStatsMapper
# Version: 1.0.0
# License: GPL-3
# Copyright (c) 2022 netvapor
# URL: github.com/netvapor/REUStatsMapper
# Formatting: UTF-8

library(dplyr)
library(ggplot2)
library(eurostat)
library(jsonlite)
library(yaml)

config = yaml.load_file(file.path(getwd(), "example_config_1.yml"))
input_file_path = config$input_file_path
chart_title = config$chart_title
group_type = config$group_type

data_of_year = config$data_of_year
if(is.null(data_of_year)){
  data_of_year = "auto-select"
}

chart_size = config$chart_size
if(is.null(chart_size)){
  chart_size = c(1080, 1080)
}

text_size = config$text_size
if(is.null(text_size)){
  text_size = 20
}

output_file_path = config$output_file_path
if(is.null(output_file_path)){
  output_file_path = file.path(getwd(), "chart")
}

input_json = read_json(input_file_path)
nuts_groups_df <- data.frame(matrix(ncol = 3, nrow = 0))
i = 1
for (group in 1:length(input_json$groups)) {
  group_raw = unlist(input_json$groups[group])
  for (j in 2:length(group_raw))
    nuts_groups_df = rbind(
      nuts_groups_df,
      data.frame(
        NUTS_group = group_raw[1],
        geo = group_raw[j],
        color = substr(names(group_raw)[1], 1, 7)
      )
    )
}

if (nchar(group_raw[2]) == 5) {
  NUTS_level = "3"
} else if (nchar(group_raw[2]) == 4) {
  NUTS_level = "2"
} else{
  stop("NUTS level could not be detected. The map possibly contains non-EU areas.")
}

population = get_eurostat(id = "demo_r_pjangrp3")
gdp = get_eurostat(id = "nama_10r_3gdp")
eurogeo = get_eurostat_geospatial(nuts_level = as.numeric(NUTS_level),
                                  resolution = "01")

population = population %>%
  filter(sex == "T", age == "TOTAL") %>%
  filter(nchar(geo) == as.numeric(NUTS_level) + 2) %>%
  dplyr::select(geo, values, TIME_PERIOD) %>% rename(Population = values)
gdp = gdp %>%
  filter(nchar(geo) == as.numeric(NUTS_level) + 2) %>%
  filter(unit == "MIO_EUR") %>%
  dplyr::select(geo, values, TIME_PERIOD) %>% rename(GDP = values)

merged_stats = merge(population, gdp, by = c("geo", "TIME_PERIOD"))
merged_stats_labelled = merge(merged_stats, nuts_groups_df, by = "geo")

if (data_of_year == "auto-select") {
  total = merged_stats_labelled %>% group_by(TIME_PERIOD) %>% count(TIME_PERIOD, sort = TRUE)
  max_count = max(total$n)
  total = total %>% filter(n == max_count) %>% arrange(desc(TIME_PERIOD))
  data_of_year = total$TIME_PERIOD[1]
}

merged_stats_labelled = merged_stats_labelled %>% filter(TIME_PERIOD == data_of_year)
mapdata = eurogeo %>% right_join(merged_stats_labelled)
mapdata$NUTS_group <-
  factor(mapdata$NUTS_group, levels = unique(nuts_groups_df$NUTS_group))

summary = mapdata %>%
  group_by(NUTS_group) %>%
  summarize(Population = sum(Population),
            GDP_mil = sum(GDP)) %>%
  arrange(desc(Population))
summary$GDP_pc = round(summary$GDP_mil * 1000000 / summary$Population)

for (x in levels(mapdata$NUTS_group)) {
  summary_row = summary %>% filter(NUTS_group == x)
  levels(mapdata$NUTS_group)[levels(mapdata$NUTS_group) == x] = paste0(
    "\n",
    as.character(x),
    "\n\nPopulation:  ",
    prettyNum(
      summary_row$Population,
      big.mark = " ",
      scientific = FALSE
    ),
    "\nGDP (in millions):  ",
    prettyNum(
      round(summary_row$GDP_mil),
      big.mark = " ",
      scientific = FALSE
    ),
    " €\nGDP (per capita):  ",
    prettyNum(
      summary_row$GDP_pc,
      big.mark = " ",
      scientific = FALSE
    ),
    " €\n"
  )
}

chart = ggplot(mapdata, aes(fill = NUTS_group)) +
  scale_fill_manual(values = unique(nuts_groups_df$color)) +
  geom_sf(color = alpha("white", 2 / 3), alpha = 1) +
  theme_minimal() +
  labs(
    title = chart_title,
    fill = group_type,
    caption = paste0("Data: Eurostat - Date: ", data_of_year, 
                     "\nFind the source code @ github.com/netvapor/REUStatsMapper")) +
  theme(
    legend.key.height = unit(4, "line"),
    panel.grid.major = element_line(colour = "transparent"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(size = text_size))

filename <- paste0(output_file_path, '.png')
png(filename, width = chart_size[1], height = chart_size[2])
chart
dev.off()
