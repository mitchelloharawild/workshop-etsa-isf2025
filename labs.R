library(fpp3)
tourism |>
  filter(Quarter == yearquarter("1998 Q1"))

library(fpp3)
accommodation <- read.csv(
  "https://workshop.nectric.com.au/user2024/data/aus_accommodation.csv"
)

as_tibble(accommodation) |>
  mutate(Date = yearquarter(Date)) |>
  as_tsibble(
    index = Date,
    key = c(State)
  )

.leap.seconds


tourism |>
  # Only tourists to Victoria
  filter(State == "Victoria") |>
  # Calculate total Trips
  summarise(Total_Trips = sum(Trips), avg_trips = mean(Trips))


tourism |>
  # Calculate annual totals
  mutate(Year = year(Quarter)) |>
  index_by(Year) |>
  # Calculate total Trips
  summarise(Total_Trips = sum(Trips), avg_trips = mean(Trips))

tourism |>
  features(Trips, feat_stl) |>
  slice_max(trend_strength, n = 3) |>
  inner_join(x = tourism, y = _, by = join_by(Region, State, Purpose)) |>
  ggplot(aes(x = Quarter, y = Trips)) + geom_line() +
  facet_grid(vars(Region, State, Purpose), scales = "free_y")

?features_by_tag

pbs_scripts <- PBS |>
  summarise(Scripts = sum(Scripts))
pbs_scripts |>
  ggplot(aes(x = month(Month, label = TRUE), y = Scripts, group = year(Month))) +
  geom_line()

pbs_scripts |>
  gg_season(Scripts, period = "1 year")

library(fpp3)
aus_production |>
  autoplot(Gas)

# gg_season()

aus_production |>
  gg_season(Gas, period = "1 year") +
  scale_y_log10()

aus_production |>
  model(STL(log(Gas))) |>
  components() |>
  autoplot()


aus_production |>
  model(STL(log(Gas))) |>
  components() |>
  gg_season(season_year)

aus_production |>
  model(STL(log(Gas))) |>
  components() |>
  gg_subseries(season_year)

pbs_scripts <- PBS |>
  summarise(Scripts = sum(Scripts))
pbs_scripts |>
  autoplot(Scripts)
# View the seasonality
pbs_scripts |>
  gg_season(Scripts)
# View the changes in seasonality
pbs_scripts |>
  gg_subseries(Scripts)
pbs_scripts |>
  model(STL(Scripts)) |>
  components() |>
  gg_season(season_year)
pbs_scripts |>
  model(STL(Scripts)) |>
  components() |>
  gg_subseries(season_year)

vic_elec |>
  ggplot(aes(y = Demand, x = Temperature,
             colour = wday(Time) %in% 2:6)) +
  geom_point(alpha = 0.1) +
  theme_minimal()

pedestrian |>
  filter(Sensor == "Southern Cross Station") |>
  index_by(Date) |>
  summarise(Count = sum(Count)) |>
  gg_season(Count, period = "1 week")

pedestrian |>
  filter(Sensor == "Southern Cross Station") |>
  # index_by(Date) |>
  # summarise(Count = sum(Count)) |>
  fill_gaps() |>
  gg_season(Count, period = "1 week")

ped_dcmp <- pedestrian |>
  filter(Sensor == "Southern Cross Station") |>
  fill_gaps(Count = 0) |>
  model(STL(Count)) |>
  components()
ped_dcmp |>
  autoplot()
ped_dcmp |>
  gg_season(season_year, period = "year")
ped_dcmp |>
  gg_season(season_week, period = "week")
ped_dcmp |>
  gg_season(season_day, period = "day")
ped_dcmp |>
  gg_season(season_day + season_week, period = "week")


ped_dcmp <- pedestrian |>
  filter(Sensor == "Southern Cross Station") |>
  fill_gaps(Count = 0) |>
  model(STL(Count ~ season(period = "year") + season(period = "week"))) |>
  components()

ped_dcmp |>
  mutate(weekday = wday(Date_Time) %in% 2:6) |>
  gg_season(season_week, period = "day") +
  facet_wrap(vars(weekday))


library(sugrrants)
vic_elec |>
  filter(Time < as.Date("2012-05-01")) |>
  ggplot(aes(x = hms::as_hms(Time), y = Demand)) +
  geom_line() +
  facet_calendar(~ Date, nrow = 2)


pedestrian |>
  filter(Sensor == "Southern Cross Station") |>
  filter(Date_Time < as.Date("2015-05-01")) |>
  ggplot(aes(x = hms::as_hms(Date_Time), y = Count)) +
  geom_line() +
  facet_calendar(~ as.Date(Date_Time))
