library(tidyverse)
library(f1dataR)



data_raw <- f1dataR::load_session_laps(season = 2024, round = 11, session = "R")



long_df <- data_raw |>
  select(!sector1session_time:sector3session_time,) |> 
  pivot_longer(
    cols = sector1time:sector3time,
    names_to = "sector",
    names_pattern = "(\\d+)",
    values_to = "sector_time",
  ) |> 
  left_join(
    data_raw |>
      select(!sector1time:sector3time) |> 
      pivot_longer(
        cols = sector1session_time:sector3session_time,
        names_to = "sector",
        names_pattern = "(\\d+)",
        values_to = "sector_session_time",
      ) |> 
      select(driver, lap_number, sector, sector_session_time),
    by = c("driver", "lap_number", "sector")
  ) |> 
  group_by(driver) |> 
  mutate(runing_time = ifelse)


test <- data_raw |> group_by(driver) |> filter(row_number() == 1) |> mutate(a = sector2time+sector3time)


tele_data <- f1dataR::load_driver_telemetry(season = 2024, round = 11, driver = "NOR", session = "R", laps = "all")
