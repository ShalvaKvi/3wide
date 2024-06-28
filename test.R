
library(tidyverse)
library(f1dataR)
library(ggalluvial)
library(showtext)
library(glue)
library(ggtext)

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
