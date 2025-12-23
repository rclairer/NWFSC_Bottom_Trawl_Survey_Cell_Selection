library(dplyr)
library(openxlsx) 

main_cells <- openxlsx::read.xlsx("Primary.Alt.Cells.Grid.Cent.ID.Dep.Corners_testing.xlsx")

primary_cells <- main_cells %>%
  dplyr::filter(Distance.nm == 0)

primary_cells <- primary_cells %>%
  group_by(Pass) %>%
  mutate(
    Leg = case_when(
      Lat >= 45.0 ~ 1,
      Lat < 45.0 & Lat >= 42.0 ~ 2,
      Lat < 42.0 & Lat >= 36.6 ~ 3,
      Lat < 36.6 & Lat >= 34.5 ~ 4,
      Lat < 34.5 ~ 5
    )
  )%>%
  ungroup()

all_cells <- main_cells %>%
  left_join(
    primary_cells %>%
      select(Pass, Primary, Leg), by = c("Pass", "Primary")
  )

all_cells <- all_cells %>%
  arrange(Pass, Leg, desc(Lat))

#check number of stations per leg
stations_per_pass_leg <- all_cells%>%
  filter(Distance.nm == 0) %>%
  group_by(Pass, Leg) %>%
  summarise(
    n_stations = n(),
    max_lat = max(Lat),
    min_lat = min(Lat),
    .groups = "drop"
  )

stations_per_pass_leg

stations_depth_pass <- all_cells %>%
  filter(Distance.nm == 0) %>%
  count(Pass, Depth.Range, name = "n_per_depth")

stations_depth_pass

stations_depth_pass_leg <- all_cells %>%
  filter(Distance.nm == 0) %>%
  count(Pass, Leg, Depth.Range, name = "n_per_depth")

stations_depth_pass_leg
  
