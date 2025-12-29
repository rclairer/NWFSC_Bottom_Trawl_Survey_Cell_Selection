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

#Pass   Leg n_stations max_lat min_lat
#  1     1     1         83    48.4    45  
#2     1     2         81    44.9    42.1
#3     1     3         99    42.0    36.6
#4     1     4         38    36.3    34.5
#5     1     5         75    34.4    32.0
#6     2     1        109    48.4    45  
#7     2     2         61    45.0    42  
#8     2     3         88    41.9    36.7
#9     2     4         43    36.5    34.5
#10     2     5         75    34.4    32.1

stations_depth_pass <- all_cells %>%
  filter(Distance.nm == 0) %>%
  count(Pass, Depth.Range, name = "n_per_depth")

stations_depth_pass

stations_depth_pass_leg <- all_cells %>%
  filter(Distance.nm == 0) %>%
  count(Pass, Leg, Depth.Range, name = "n_per_depth")

stations_depth_pass_leg

#############################  
#it would make the most sense to look at maps before this stage and then the team gets back to me with these numbers and then I run the code
#by number instead of lat
number_station_per_leg <- list(
  '1' = c(88,80,68,70,70),
  '2' = c(80,88,68,70,70))

primary_cells <- main_cells %>%
  dplyr::filter(Distance.nm == 0)

primary_cells <- primary_cells %>%
  arrange(pass, desc(latitude)) %>%
  group_by(pass) %>%
  mutate(
    
  )

