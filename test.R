library(tidyverse)
library(f1dataR)
library(ggalluvial)
library(showtext)
library(ggthemes)
library(patchwork)
library(glue)
library(ggtext)

font_add_google(name = "Tomorrow", family = "Tomorrow")
font_add_google(name = "Roboto", family = "Roboto")
font_add_google(name = "Roboto Mono", family = "Roboto Mono")
font_add_google(name = "Ubuntu", family = "Ubuntu")
font_add_google(name = "Ubuntu Mono", family = "Ubuntu Mono")
font_add_google(name = "Fira Sans", family = "Fira")
font_add_google(name = "Fira Mono", family = "Fira Mono")

showtext_auto()


ROUND <- 10

dri
ver_metadata <- read_csv("./utils/driver_metadata.csv")
driver_colors <- driver_metadata |>
  pull(constructor_color, driver_id)

standings_raw <- bind_rows(
  f1dataR::load_standings(season = 2024, round = ROUND) |>
    mutate(season = 2024),
  f1dataR::load_standings(season = 2023, round = ROUND) |>
    mutate(season = 2023)
)

standings <- standings_raw |>
  filter(!driver_id %in% c("de_vries")) |>
  expand(driver_id, season) |>
  left_join(standings_raw, by = c("driver_id", "season")) |>
  left_join(driver_metadata |> select(driver_id, family_name), by = "driver_id") |>
  mutate(
    position = as.numeric(position),
    points = as.numeric(points),
    points = replace_na(points, 0),
    points_for_plot = points + 60 - position
  ) |>
  group_by(season) |>
  arrange(-position) |>
  mutate(text_y_pos = cumsum(points_for_plot) - (points_for_plot / 2)) |>
  group_by(driver_id) |>
  
  arrange(season) |>
  mutate(
    point_lag = points - lag(points),
    text_label = case_when(
      season == 2023 ~ paste0(family_name, ": ", points),
      season == 2024 &
        point_lag >= 0 ~ paste0(family_name, ": ", points, "(+", abs(point_lag), ")"),
      season == 2024 &
        point_lag < 0 ~ paste0(family_name, ": ", points, "(-", abs(point_lag), ")"),
    )
  ) |>
  ungroup()

redbull_clr = driver_metadata$constructor_color[driver_metadata$driver_id ==
                                              "max_verstappen"]
mclaren_clr = driver_metadata$constructor_color[driver_metadata$driver_id ==
                                                "norris"]
alonso_clr = driver_metadata$constructor_color[driver_metadata$driver_id ==
                                                 "alonso"]
leclerc_clr = driver_metadata$constructor_color[driver_metadata$driver_id ==
                                                  "leclerc"]

russell_clr = driver_metadata$constructor_color[driver_metadata$driver_id ==
                                                  "russell"]

title <- glue(
  "<span style = 'font-size:24pt;'>**Comparing The F1 Driver Standings After 10 Rounds to Last Season**</span><br>
  <span style = 'font-size:12pt;'>While <span style = 'color:{redbull_clr};'>**Max Verstappen**</span> still doimnates at the top of the 
  table as he did at this stage of the previous season, there are some noteable changes in the pecking order.<br><br>
  Among the noteable gainers are <span style = 'color:{mclaren_clr};'>**Lando Norris**</span> up at 2nd from 9th with a massive **+108** point gain, 
  his teammate <span style = 'color:{mclaren_clr};'>**Oscar Piastri**</span> at 6th with a **+70** gain, and <span style = 'color:{leclerc_clr};'>**Charles Leclerc**</span> at 3rd with a **+74** gain.<br><br>
  Among the noteable losers are <span style = 'color:{alonso_clr};'>**Fernando Alonso**</span> with a huge **-96** point drop-off down to 9th from 3rd, 
  <span style = 'color:{russell_clr};'>**Lewis Hamilton**</span> with **-51** point loss, 
  and <span style = 'color:{redbull_clr};'>**Sergio Pérez**</span> with a **-45** point loss following a sequence of poor race finishes.
  
  </span>"
)

standings |>
  ggplot() +
  aes(
    x = season,
    y = points_for_plot,
    alluvium = driver_id,
    group = season
  ) +
  geom_alluvium(
    aes(fill = driver_id),
    color = "grey10",
    decreasing = FALSE,
    width = 0.6,
    alpha = 0.9,
    size = 0.7
  ) +
  geom_text(
    data = standings |>  filter(season == 2023),
    aes(label = text_label, y = text_y_pos),
    hjust = 0,
    nudge_x = -0.25,
    color = "grey10",
    size = 4,
    family = "Fira Mono"
  ) +
  geom_text(
    data = standings |>  filter(season == 2024),
    aes(label = text_label, y = text_y_pos, color = driver_id),
    hjust = 1,
    nudge_x = 0.25,
    color = "grey10",
    size = 4,
    family = "Fira Mono"
  ) +
  scale_x_discrete() +
  scale_fill_manual(values = driver_colors) +
  annotate(
    geom = "text",
    x = c(2022.6, 2024.4),
    y = 1000,
    angle = 90,
    label = c("2023 Season", "2024 Season"),
    size = 8,
    family = "Fira",
    color = "grey10"
  ) +
  theme_void() +
  labs(title = title) +
  theme(
    plot.background = element_rect(fill = "grey95"),
    legend.position = "none",
    plot.title = element_textbox_simple(
      size = 14,
      lineheight = 1,
      padding = margin(10, 120, 0, 120)
    )
    #plot.margin = unit(c(0,0,0,-160), "pt")
  )








point_diff_NULLpoint_diff_plot <- standings |>
  filter(season == 2024) |>
  mutate(family_name = fct_reorder(family_name, -point_lag)) |>
  ggplot(aes(
    x = family_name,
    y = point_lag,
    fill = driver_id,
    label = family_name
  )) +
  geom_col(width = 0.9,
           color = "grey10",
           alpha = 0.9) +
  geom_text(
    aes(
      label = family_name,
      y = ifelse(point_lag > 0, point_lag + 2, point_lag - 2),
      hjust = ifelse(point_lag > 0, 0, 1)
    ),
    angle = 90,
    color = "grey10",
    size = 4,
    family = "Fira Mono",
  ) +
  scale_fill_manual(values = driver_colors) +
  scale_y_continuous(limits = c(-125, 125), breaks = seq(-125, 125, 25)) +
  labs(y = "Points Difference") +
  theme_clean(base_family = "Fira", base_size = 20) +
  theme(
    plot.background = element_rect(fill = "grey95"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(
      family = "Fira",
      hjust = 0.5,
      size = 22
    ),
    plot.margin = unit(c(2, 1, 2, 1), "cm")
  )
