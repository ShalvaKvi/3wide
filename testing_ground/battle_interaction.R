library(tidyverse)
library(f1dataR)



data_raw <- f1dataR::load_session_laps(season = 2024, round = 11, session = "R")


# pivot the df to long format - one row per sector
long_df <- data_raw |>
  select(!sector1session_time:sector3session_time,) |> 
  pivot_longer(
    cols = sector1time:sector3time,
    names_to = "sector_number",
    names_pattern = "(\\d+)",
    values_to = "sector_time",
  ) |> 
  left_join(
    data_raw |>
      select(!sector1time:sector3time) |> 
      pivot_longer(
        cols = sector1session_time:sector3session_time,
        names_to = "sector_number",
        names_pattern = "(\\d+)",
        values_to = "sector_session_time",
      ) |> 
      select(driver, lap_number, sector_number, sector_session_time),
    by = c("driver", "lap_number", "sector_number")
  )


rank_df <- long_df |> 
  group_by(lap_number, sector_number) |> 
  arrange(sector_session_time) |> 
  mutate(sector_position = row_number()) |> 
  arrange(lap_number, sector_number, position) |> 
  select(driver, lap_number,lap_time, sector_number,  sector_number, position, sector_position, sector_session_time, lap_start_time)
  #group_by(driver) |> 
  #mutate(runing_time = ifelse)


test <- data_raw |> group_by(driver) |> filter(row_number() == 1) |> mutate(a = sector2time+sector3time)


tele_data <- f1dataR::load_driver_telemetry(season = 2024, round = 11, driver = "NOR", session = "R", laps = "all")
