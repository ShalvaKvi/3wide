# Load Libraries ----
library(tidyverse)
library(f1dataR)
library(ggalluvial)
library(showtext)
library(glue)
library(ggtext)

# Data Analysis ----

## Get Data ----

driver_metadata <- read_csv("./utils/driver_metadata.csv") # useful metadata
ROUND <- 10 # define the F1 season round

# use the {f1dataR} package to retrieve the 2023 and 2024 season standings
standings_raw <- bind_rows(
  f1dataR::load_standings(season = 2024, round = ROUND) |>
    mutate(season = 2024),
  
  f1dataR::load_standings(season = 2023, round = ROUND) |>
    mutate(season = 2023)
)

## Data Wrangling ----

standings <- standings_raw |>
  filter(driver_id != "de_vries") |> # drop de-vries, not scored any points in 2023 
  expand(driver_id, season) |>
  left_join(standings_raw, by = c("driver_id", "season")) |>
  left_join(driver_metadata |> select(driver_id, family_name), by = "driver_id") |>
  mutate(
    position = as.numeric(position),
    points = as.numeric(points),
    points = replace_na(points, 0),
    # create a column to help with plotting, keep the drivers in correct order
    points_for_plot = points + 60 - position 
  ) |>
  # create a column to help with positioning the driver names text for the plot
  group_by(season) |>
  arrange(-position) |>
  mutate(text_y_pos = cumsum(points_for_plot) - (points_for_plot / 2)) |>
  # create a column to hold the labels for the plot
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


# Plotting ----

## Load Fonts ----
font_add_google(name = "Tomorrow", family = "Tomorrow")
font_add_google(name = "Fira Sans", family = "Fira")
font_add(family = "fa6-brands", regular = "C:\\Users\\Shalva\\AppData\\Local\\Microsoft\\Windows\\Fonts\\Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

## Set Colors ----

# named vector used for plotting
driver_colors <- driver_metadata |>
  pull(constructor_color, driver_id)

# save relevant colors used in the text as vars
redbull_clr  <-  driver_metadata$constructor_color[driver_metadata$driver_id == "max_verstappen"]
mclaren_clr <- driver_metadata$constructor_color[driver_metadata$driver_id == "norris"]
alonso_clr <- driver_metadata$constructor_color[driver_metadata$driver_id == "alonso"]
ferrari_clr <- driver_metadata$constructor_color[driver_metadata$driver_id == "leclerc"]
russell_clr <- driver_metadata$constructor_color[driver_metadata$driver_id == "russell"]

## Set Text ----

# social icons
x_icon <- glue(
  "<span style='font-family:fa6-brands;'><span style='font-size:14pt;'>&#xe61a;</span></span>"
)
github_icon <- glue(
  "<span style='font-family:fa6-brands;'><span style='font-size:14pt;'>&#xf09b;</span></span>"
)

# main body
header <- glue(
  "
  <span style='font-size:28pt;'>**The F1 Driver Standings After 10 Rounds, How Does it Compare to Last Season?**<br><br>
  "
)

first_par <- glue(
  "
  <span style='font-size:12pt;'>While <span style = 'color:{redbull_clr};'>**Max Verstappen**</span> still doimnates at the
  top of thetable as he did at this stage of the previous season, there are some noteable changes in the pecking order.<br><br>
  "
)

second_par <- glue(
  "
  Among the gainers are the <span style='color:{mclaren_clr};'>**McLaren**</span> duo of <span style='color:{mclaren_clr};'>**Lando Norris**</span>,
  with a massive **+108** point gain, and his teammate <span style='color:{mclaren_clr};'>**Oscar Piastri**</span> with a **+70** point gain,
  who have made the most out of their improved car. With them are the <span style='color:{ferrari_clr};'>**Ferrari**</span> teammates
  of <span style='color:{ferrari_clr};'>**Charles Leclerc**</span> with a **+74** point gain,and <span style='color:{ferrari_clr};'>**Carlos Sainz**</span>
  with a slighly less impressive **+33** point gain.<br><br>
  "
)

third_par <- glue(
  "
  At the top of the losers stands <span style='color:{alonso_clr};'>**Fernando Alonso**</span> with a whopping **-96** point loss
  compared to last season, dropping him to 9th in the standings. With him we can find <span style='color:{russell_clr};'>**Lewis Hamilton**</span>
  with **-51** point loss casusing him to lag behind his teammate,and <span style='color:{redbull_clr};'>**Sergio Pérez**</span> with a **-45** point loss who
  had a very dissapointing start to the season.</span>
  "
)

title <- paste0(header, first_par, second_par, third_par)

# additional text
subtitle <-  glue("<span style='font-size:16pt;'>**F1 Driver Standings After 10 Rounds:**")
caption <- glue(
  "<span style='font-size:10pt;'>Data analysis and visualization by **3wide**. Socials: {x_icon} 3wide_analytics {github_icon} ShalvaKvi/3wide"
)


## Figure ----

main_plt <- standings |>
  ggplot(aes(
    x = season,
    y = points_for_plot,
    alluvium = driver_id,
    group = season
  )) +
  geom_alluvium(
    aes(fill = driver_id),
    color = "grey5",
    decreasing = FALSE,
    width = 0.6,
    alpha = 0.85,
    size = 0.8
  ) +
  # left side labels
  geom_text(
    data = standings |>  filter(season == 2023),
    aes(label = text_label, y = text_y_pos, size = points_for_plot),
    hjust = 0,
    nudge_x = -0.25,
    color = "grey5",
    family = "Fira"
  ) +
  # right side labels
  geom_text(
    data = standings |>  filter(season == 2024),
    aes(
      label = text_label,
      y = text_y_pos,
      color = driver_id,
      size = points_for_plot
    ),
    hjust = 1,
    nudge_x = 0.25,
    color = "grey5",
    family = "Fira"
  ) +
  annotate(
    #
    geom = "text",
    x = c(2022.6, 2024.4),
    y = 1000,
    angle = 90,
    label = c("2023 Season", "2024 Season"),
    size = 6,
    fontface = "bold",
    family = "Fira",
    color = "grey20"
  ) +
  # scale and color adjustments
  scale_x_discrete() +
  scale_size_continuous(range = (c(3.5, 5))) +
  scale_fill_manual(values = driver_colors) +
  # adding labs
  labs(title = title,
       subtitle = subtitle,
       caption = caption) +
  # theme adjustments
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F1F0EF"),
    legend.position = "none",
    plot.title = element_textbox_simple(
      color = "grey20",
      family = "Fira",
      lineheight = 1,
      halign = 0,
      padding = margin(0, 100, 0, 100)
    ),
    plot.subtitle = element_textbox_simple(
      color = "grey20",
      family = "Fira",
      lineheight = 1,
      halign = 0.6,
      padding = margin(0, 140, -20, 100)
    ),
    plot.caption = element_textbox_simple(
      color = "grey30",
      family = "Tomorrow",
      lineheight = 1,
      halign = 1,
      padding = margin(0, 70, 0, 70)
    ),
    plot.margin = unit(c(5, -60, 5, -60), "pt")
  )

ggsave(filename = "./posts/driver_standings_comaprison_r10_2023_2024/driver_standings_comaprison_r10_2023_2024.png", plot = main_plt, device = "png", width = 900, height = 1400, units = "px", dpi = 100)
