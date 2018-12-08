# Load some packages

library(tidyverse)

############# French mortality ##############
library(demography)

# Set up data set in tsibble format
frmort <- set.upperage(fr.mort, 100)
frmort <- tibble(
  year = rep(frmort$year, rep(length(frmort$age), length(frmort$year))),
  age = rep(frmort$age, length(frmort$year)),
  female = c(frmort$rate$female),
  male = c(frmort$rate$male),
) %>%
  gather(male, female, key = "sex", value = "mortrate")

# Plot functions by sex
frmort %>%
  ggplot(aes(x = age, y = mortrate, group = year)) +
  geom_line() +
  facet_grid(~sex) +
  scale_y_log10()

# Mapping time to colour
frmort %>%
  ggplot(aes(x = age, y = mortrate, group = year, col = year)) +
  geom_line() +
  facet_grid(~sex) +
  scale_y_log10()

frmort %>%
  ggplot(aes(x = age, y = mortrate, group = year, col = year)) +
  geom_line() +
  facet_grid(~sex) +
  scale_y_log10() +
  scale_color_gradientn(colours = rainbow(10))

# Putting time on horizontal axis and mapping x to colour.

frmort %>%
  ggplot(aes(x = year, y = mortrate, group = age, col = age)) +
  geom_line() +
  facet_grid(~sex) +
  scale_y_log10() +
  scale_color_gradientn(colours = rainbow(10))

# 3-d plots

frmort %>%
  ggplot(aes(x = year, y = age, fill = log(mortrate))) +
  geom_raster() +
  facet_grid(~sex) +
  scale_fill_viridis_c(option = "A", direction = -1)

## ACF plots

myacf <- function(x) {
  tibble(
    lag = 0:25,
    acf = as.numeric(acf(x, plot = FALSE, lag.max = 25)$acf)
  )
}

fracf <- frmort %>%
  nest(year, mortrate) %>%
  mutate(
    acf = map(data, ~ myacf(x = .$mortrate))
  ) %>%
  select(-data) %>%
  unnest() %>%
  filter(lag > 0)

fracf %>%
  ggplot(aes(x = age, y = acf, group = lag, col = lag)) +
  geom_line() +
  facet_grid(~sex) +
  scale_color_gradientn(colours = rainbow(10))

fracf %>%
  ggplot(aes(x = lag, y = acf, group = age, col = age)) +
  geom_line() +
  facet_grid(~sex) +
  scale_color_gradientn(colours = rainbow(10))

fracf %>%
  ggplot(aes(x = lag, y = age, fill = acf)) +
  geom_raster() +
  facet_grid(~sex) +
  scale_fill_viridis_c(option = "A", direction = -1)

########### Pedestrian data ############

library(sugrrants)

pedestrian <- pedestrian %>%
  filter(
    Sensor_Name == "Flinders Street Station Underpass",
    Date <= as.Date("2016-12-31"),
  ) %>%
  select(-Sensor_ID, -Sensor_Name) %>%
  rename_all(tolower) %>%
  rename(
    hour = "time",
    time = "date_time",
    number = "hourly_counts"
  ) %>%
  left_join(tsibble::holiday_aus(2016, state = "VIC")) %>%
  mutate(
    daytype = ifelse(
      day %in% c("Saturday", "Sunday") | !is.na(holiday),
      "Holiday", "Workday"
    ),
    week = lubridate::week(date)
  ) %>%
  select(date, hour, day, daytype, number, week)

# Time on horizontal axes, functional variable to colour
pedestrian %>%
  ggplot(aes(x = date, y = number, group = hour, col = hour)) +
  geom_line() +
  scale_color_gradientn(colours = rainbow(10))

pedestrian %>%
  ggplot(aes(x = date, y = number, group = hour, col = hour)) +
  geom_line() +
  facet_grid(~daytype) +
  scale_color_gradientn(colours = rainbow(10))

pedestrian %>%
  ggplot(aes(x = date, y = number, group = hour, col = hour)) +
  geom_line() +
  facet_grid(~day) +
  scale_color_gradientn(colours = rainbow(10))

# hour on horizontal axes, time to colour

pedestrian %>%
  mutate(
    ndate = as.numeric(date - as.Date("2016-01-01"))
  ) %>%
  ggplot(aes(x = hour, y = number, group = date, col = ndate)) +
  geom_line() +
  scale_color_gradientn(colours = rainbow(10))

pedestrian %>%
  mutate(
    ndate = as.numeric(date - as.Date("2016-01-01"))
  ) %>%
  ggplot(aes(x = hour, y = number, group = date, col = ndate)) +
  geom_line() +
  facet_grid(~daytype) +
  scale_color_gradientn(colours = rainbow(10))

## Calendar

p <- pedestrian %>%
  frame_calendar(x = hour, y = number, date = date) %>%
  ggplot(aes(x = .hour, y = .number, group = date, colour = daytype)) +
  geom_line() +
  theme(legend.position = "bottom")
prettify(p)

# 3-d

pedestrian %>%
  ggplot(aes(x = date, y = hour, fill = number)) +
  geom_raster() +
  facet_grid(~daytype) +
  scale_fill_viridis_c(option = "A", direction = -1)

# ACF

pedacf <- pedestrian %>%
  nest(-hour) %>%
  mutate(
    acf = map(data, ~ myacf(x = .$number))
  ) %>%
  select(-data) %>%
  unnest() %>%
  filter(lag > 0)

pedacf %>%
  ggplot(aes(x = hour, y = acf, group = lag, col = lag)) +
  geom_line() +
  scale_color_gradientn(colours = rainbow(10))

pedacf %>%
  ggplot(aes(x = lag, y = acf, group = hour, col = hour)) +
  geom_line() +
  scale_color_gradientn(colours = rainbow(10))

pedacf %>%
  ggplot(aes(x = lag, y = hour, fill = acf)) +
  geom_raster() +
  scale_fill_viridis_c(option = "A", direction = -1)

# Spectral density

myspectrum <- function(x) {
  spec <- spec.ar(x, plot=FALSE)
  tibble(
    frequency = spec$freq,
    logspectrum = log(c(spec$spec))
  )
}
  
pedspectrum <- pedestrian %>%
  nest(-hour) %>%
  mutate(
    spec = map(data, ~ myspectrum(x = .$number))
  ) %>%
  select(-data) %>%
  unnest()

pedspectrum %>%
  ggplot(aes(x = frequency, y = hour, fill = logspectrum)) +
  geom_raster() +
  scale_fill_viridis_c(option = "A", direction = -1)


