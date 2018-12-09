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
# Functional ACF requires cross-correlations for ages

facf <- function(df, xvar, yvar, time, lag.max=20) {
  key <- enquo(xvar)
  value <- enquo(yvar)
  timeindex <- enquo(time)
  x <- df %>%
    select(!!key, !!value, !!timeindex) %>%
    spread(value=!!value, key=!!key) %>%
    select(-!!timeindex) %>%
    as.ts() %>%
    acf(plot=FALSE, lag.max=lag.max, na.action=na.pass)
  nx <- dim(x$acf)[2]
  output <- NULL
  for(i in seq(lag.max+1)) {
    output <- bind_rows(output,
      tibble(
        lag = i-1,
        x1 = rep(rep(0:(nx-1), nx)),
        x2 = rep(0:(nx-1), rep(nx,nx)),
        acf = c(x$acf[i,,])
      ))
  }
  colnames(output)[2:3] <- paste0(as.character(key)[[2]],1:2)
  output
}

fracf <- frmort %>%
  nest(-sex) %>%
  mutate(
    acf = map(data, ~ facf(df=., xvar=age, yvar=mortrate, time=year))
  ) %>%
  select(-data) %>%
  unnest()

fracf %>%
  filter(lag < 4) %>%
  ggplot(aes(x = age1, y = age2, fill = acf)) +
	  geom_raster() +
	  facet_grid(sex~lag) +
	  scale_fill_viridis_c(option = "A", direction = -1)

# Look at diagonals

fracf %>%
  filter(age1==age2) %>%
  ggplot(aes(x = age1, y = acf, group = lag, col = lag)) +
	  facet_grid(~sex) +
	  geom_line() +
	  scale_color_gradientn(colours = rainbow(10))

fracf %>%
  filter(age1==age2) %>%
  ggplot(aes(x = lag, y = acf, group = age1, col = age1)) +
	  geom_line() +
	  facet_grid(~sex) +
	  scale_color_gradientn(colours = rainbow(10))

fracf %>%
  filter(age1==age2) %>%
  ggplot(aes(x = lag, y = age1, fill = acf)) +
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
  	scale_fill_viridis_c(option = "A", direction = -1)

# ACF

pedacf <- pedestrian %>%
  facf(xvar=hour, yvar=number, time=date, lag.max=20)

pedacf %>%
  filter(lag < 9) %>%
  ggplot(aes(x = hour1, y = hour2, fill = acf)) +
	  geom_raster() +
	  facet_wrap(~lag) +
	  scale_fill_viridis_c(option = "A", direction = -1)

# Look at diagonal

pedacf %>%
  filter(hour1==hour2) %>%
  ggplot(aes(x = hour1, y = acf, group = lag, col = lag)) +
	  geom_line() +
	  scale_color_gradientn(colours = rainbow(10))

pedacf %>%
  filter(hour1==hour2) %>%
  ggplot(aes(x = lag, y = acf, group = hour1, col = hour1)) +
	  geom_line() +
	  scale_color_gradientn(colours = rainbow(10))

pedacf %>%
  filter(hour1==hour2) %>%
  ggplot(aes(x = lag, y = hour1, fill = acf)) +
	  geom_raster() +
	  scale_fill_viridis_c(option = "A", direction = -1)

