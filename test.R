library(tidyverse)
library(f1dataR)
library(ggalluvial)
library(showtext)

font_add_google(name = "Tomorrow", family = "Tomorrow")
font_add_google(name = "Roboto", family = "Roboto")
font_add_google(name = "Roboto Mono", family = "Roboto Mono")
font_add_google(name = "Ubuntu", family = "Ubuntu")
font_add_google(name = "Ubuntu Mono", family = "Ubuntu Mono")

showtext_auto()


ROUND <- 10

driver_metadata <- read_csv("./utils/driver_metadata.csv")
driver_colors <- driver_metadata |>
  pull(constructor_color, driver_id)

standings_raw <- bind_rows(
  f1dataR::load_standings(season = 2024, round = ROUND) |>
    mutate(season = 2024),
  f1dataR::load_standings(season = 2023, round = ROUND) |>
    mutate(season = 2023)
)

standings <- standings_raw |>
  filter(!driver_id %in% c("bearman", "de_vries")) |>
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
        point_lag >= 0 ~ paste0(family_name, ": ", points, " (+", abs(point_lag), ")"),
      season == 2024 &
        point_lag < 0 ~ paste0(family_name, ": ", points, " (-", abs(point_lag), ")"),
    )
  )

main_plt <- standings |>
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
    alpha = 0.75,
    size = 0.5
  ) +
  geom_text(
    data = standings |>  filter(season == 2023),
    aes(label = text_label, y = text_y_pos),
    hjust = 0,
    nudge_x = -0.2,
    color = "grey10",
    size = 4,
    family = "Roboto"
  ) +
  geom_text(
    data = standings |>  filter(season == 2024),
    aes(label = text_label, y = text_y_pos),
    hjust = 1,
    nudge_x = 0.2,
    color = "grey10",
    size = 4,
    family = "Roboto"
  ) +
  scale_x_discrete() +
  scale_fill_manual(values = driver_colors)

main_plt +
  annotate(
    geom = "text",
    x = c(2022.6, 2024.4),
    y = 1000,
    angle = 90,
    label = c("2023 Season", "2024 Season"),
    size = 10,
    family = "Roboto",color = "grey40"
  ) +
  labs(title = "How does the Driver championship standings look after 10 rounds compared to last season?", 
       subtitle = "While Verstappen still dominates at the top of the table") +
  theme_void(base_family = "Roboto", base_size = 18) +
  theme(plot.background = element_rect(fill = "#f3f5e4"),
        legend.position = "none", plot.margin = unit(c(1, 1, 1, 1), "cm"))

