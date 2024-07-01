# Load Libraries ----
library(tidyverse)
library(f1dataR)
library(ggalluvial)
library(showtext)
library(glue)
library(ggtext)
library(ggthemes)


# Data Analysis ----

## Get Data ----

driver_metadata <- read_csv("./utils/driver_metadata.csv") # useful metadata for plotting
schedule_df <- f1dataR::load_schedule() # season schedule, used for plotting

ROUNDS <- 1:11 # define rounds
SEASON <- 2024 # define season

# Run a loop to retreive session laps from relevant rounds
for (round in ROUNDS) {
  data_raw <- f1dataR::load_session_laps(season = SEASON,
                                         round = round,
                                         session = "Q") |>
    mutate(season = SEASON, round = round)
  
  if (round == 1) {
    full_data <- data_raw
  } else {
    full_data <- full_data |> bind_rows(data_raw)
  }
  
}

#full_data <- read_csv("./temp_data.csv")
#write_csv(full_data, "./temp_data.csv")

## Data Wrangling ----

lap_time_summary <- full_data |>
  # find fastest sector times for each driver and round
  group_by(season, round, driver) |>
  summarise(across(c(sector1time:sector3time, lap_time), ~ min(.x, na.rm = TRUE)),
            driver_optimal_lap = sector1time + sector2time + sector3time) |>
  # calculate optimal lap, and gap from optimal for each driver
  group_by(season, round) |>
  mutate(
    session_optimal_lap = min(sector1time) + min(sector2time) + min(sector3time),
    gap_driver_best_from_session_optimal = lap_time - session_optimal_lap,
    across(where(is.numeric), ~ round(.x, 3))
  )

# relevant drivers for the analysis
highlight_drivers <- c("VER", "PER")

# finalize the data frame for the plotting procedure
plot_df <- lap_time_summary |>
  filter(driver %in% highlight_drivers) |>
  group_by(season, round) |>
  arrange(driver) |>
  # find the gap of per from verstappen
  mutate(driver_gap = gap_driver_best_from_session_optimal - lead(gap_driver_best_from_session_optimal)) |>
  # join the driver family name for plotting
  left_join(driver_metadata |> select(family_name, code), by = c("driver" = "code"))

# Plotting ----

## Load Fonts ----
font_add_google(name = "Tomorrow", family = "Tomorrow")
font_add_google(name = "Fira Sans", family = "Fira")
font_add(family = "fa6-brands", regular = "C:\\Users\\Shalva\\AppData\\Local\\Microsoft\\Windows\\Fonts\\Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

## Set Colors ----
per_color <- driver_metadata$constructor_color[driver_metadata$code == "PER"]
ver_color <- driver_metadata$constructor_color[driver_metadata$code == "NOR"]
red_highlight_color <- driver_metadata$constructor_color[driver_metadata$code == "LEC"]

## Calc avg values for plot ----
avg_1_6 <- sprintf("+%0.3fs", mean(plot_df$driver_gap[plot_df$round <= 6], na.rm = TRUE))
avg_7_11 <- sprintf("+%0.3fs", mean(plot_df$driver_gap[plot_df$round > 6], na.rm = TRUE))

## Set Text ----

# title
header <- glue(
  "
  <span style='font-size:36pt;'>**<span style='color:{per_color};'>Sergio Pérez's</span> Poor Qualifying Performance Continues**<br><br>
  "
)

first_par <- glue(
  "
  <span style='font-size:16pt;'>With the Austrian Grand Prix coming to an end,
  <span style='color:{per_color};'>**Sergio Pérez**</span> has continued his streak of poor qualifying performances.<br>
  An analysis of personal best lap times in qualifying sessions reveals that
  <span style='color:{per_color};'>**Sergio Pérez**</span> has been struggling to extract pace from his car while his teammate
  <span style='color:{ver_color};'>**Max Verstappen**</span> still maintains a competitive pace.
  The average qualifying gap to his teammate increased from a somewhat reasonable **{avg_1_6}** in the first 6 rounds of the season,
  to a whopping <span style='color:{red_highlight_color};'>**{avg_7_11}**</span> in the last 5. This run of poor form raises further
  questions on Red Bull's decision to extend the driver's contract until the end of 2026.</span>
  "
)

title <- paste0(header, first_par)

# subtitle
subtitle <-  glue(
  "
  <span style='font-size:16pt;'>**Qualifying Personal Best Lap Times Gap from Session Optimal:**</span><br>
  <span style='font-size:12pt;'>Each dot represents the gap from the qualifying
  personal best lap time to the session's theoretical optimal lap time.<br>
  The dotted grey line represesnts the session's theoretical optimal lap time</span>
  "
)

# caption
x_icon <- glue(
  "<span style='font-family:fa6-brands;'><span style='font-size:14pt;'>&#xe61a;</span></span>"
)
github_icon <- glue(
  "<span style='font-family:fa6-brands;'><span style='font-size:14pt;'>&#xf09b;</span></span>"
)

caption <- glue(
  "<span style='font-size:10pt;'>Data analysis and visualization by **3wide**. Socials: {x_icon} 3wide_analytics {github_icon} ShalvaKvi/3wide"
)

## Figure ----

###  Base plots
main_plt <- plot_df |>
  ggplot(aes(x = gap_driver_best_from_session_optimal, y = round)) +
  # optimal lap time line + text
  annotate(
    geom = "text",
    x = c(-0.05, 0.05),
    y = c(0.25, 12),
    label = "Optimal lap time",
    angle = c(90, 270),
    size = 5,
    color = "grey50",
    family = "Fira"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    color = "grey50",
    size = 1,
  ) +
  # geoms
  geom_line(aes(group = round), size = 4, color = "grey85") +
  geom_point(aes(color = driver), size = 5, alpha = 0.9) +
  # driver name indication
  geom_text(
    aes(
      y = round - 0.4,
      label = ifelse(round == 1, family_name, ""),
      color = driver
    ),
    nudge_x = ifelse(plot_df$driver == "VER", -0.07, 0.07),
    size = 6,
    family = "Fira",
    fontface = "bold"
  ) +
  # gap to optimal text
  geom_text(
    aes(
      label = sprintf("+%0.3f", gap_driver_best_from_session_optimal),
      color = driver,
    ),
    nudge_x = ifelse(plot_df$driver == "VER", -0.12, 0.12),
    size = 4
  ) +
  ###  Scales
  scale_y_reverse(
    limits = c(12.2, 0),
    breaks = ROUNDS,
    labels = paste0("Round ", ROUNDS, ": ", schedule_df$race_name[1:11])
  ) +
  scale_x_continuous(limits = c(-0.12, 2.75)) +
  scale_color_manual(values = c("VER" = ver_color, "PER" = per_color)) +
  ### Annotations
  # Round 1-6 annotation
  annotate(
    geom = "segment",
    x = 1,
    xend = 1,
    y = 0.75,
    yend = 6.25,
    linewidth = 1,
    linetype = "solid",
    color = "grey50"
  ) +
  annotate(
    geom = "text",
    x = 1.32,
    y = 3.5,
    label = paste0("Average gap to \n Verstappen: ", avg_1_6),
    size = 6,
    #fontface = "bold",
    color = "grey50",
    family = "Fira"
  ) +
  # Round 7-11 annotation
  annotate(
    geom = "segment",
    x = 2.25,
    xend = 2.25,
    y = 6.75,
    yend = 11.25,
    linewidth = 1,
    color = red_highlight_color
  ) +
  annotate(
    geom = "text",
    x = 2.57,
    y = 9,
    label = paste0("Average gap to \n Verstappen: ", avg_7_11),
    size = 6,
    #fontface = "bold",
    color = red_highlight_color,
    family = "Fira"
  ) +
  ###  Theme
  labs(
    y = "",
    x = "",
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  theme_tufte(base_family = "Fira", base_size = 16) +
  theme(
    plot.background = element_rect(fill = "#F1F0EF"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_markdown(
      color = "grey30",
      hjust = 0,
      size = 14
    ),
    plot.title = element_textbox_simple(
      color = "grey10",
      family = "Fira",
      lineheight = 1,
      halign = 0,
      padding = margin(50, 100, 0, -200)
    ),
    plot.subtitle = element_textbox_simple(
      color = "grey30",
      family = "Fira",
      lineheight = 1,
      halign = 0,
      padding = margin(100, 150, -30, 100)
    ),
    plot.caption = element_textbox_simple(
      color = "grey30",
      family = "Tomorrow",
      lineheight = 1,
      halign = 1,
      padding = margin(0, 70, 0, 70)
    ),
    plot.margin = unit(c(20, 20, 20, 20), "pt")
  )

ggsave(filename = "./posts/01_07_2024_sergio_perez_pace_analysis/sergio_perez_pace_analysis.png", plot = main_plt, device = "png", width = 1500, height = 1800, units = "px", dpi = 100)

