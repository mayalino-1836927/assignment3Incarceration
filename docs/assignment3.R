# Introduction + summary statistics
# Introduces the report and dataset, and provides at least 5 computed summary statistics from the data.
# 20.0 pts

library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)
library(styler)

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)
incarceration_trends <- incarceration_trends %>% 
  mutate(year = as.Date(year))
View(incarceration_trends)

# Cross reference total jail pop. rate
kingcounty2018 <- incarceration_trends %>%
  filter(county_name == "King County", state == "WA", year == 2018) %>%
  pull(total_jail_pop_rate)

# Grab a few columns
analysis_data <- incarceration_trends %>% 
  select(aapi_jail_pop_rate, aapi_prison_pop_rate, latinx_jail_pop_rate, latinx_prison_pop_rate, black_jail_pop_rate, black_prison_pop_rate,
         white_jail_pop_rate, white_prison_pop_rate, native_jail_pop_rate, native_prison_pop_rate, total_prison_pop_rate, total_jail_pop_rate,
         year, fips, state, county_name) %>%
  mutate(black_jail_ratio = black_jail_pop_rate/total_jail_pop_rate) %>% 
  mutate(newvaluehere = computehere) %>%
  mutate() %>%
  mutate() %>%
  mutate() %>%
View(analysis_data)


# Time Trend Chart
# Thoughtfully creates a chart of trends over time, describes *why* it was designed that way, and what patterns emerge. Chart meets requirements outlined above.
# 20.0 pts

# Draw a line plot (with line geometry)
# trend by year
ggplot(data = analysis_data) +
  geom_line(mapping = aes(x = year, y = price))

# `smooth` geometry
ggplot(data = analysis_data) +
  geom_smooth(mapping = aes(x = year, y = price))


# Variable Comparison Chart
# Thoughtfully creates a chart comparing two variables, describes *why* it was designed that way, and what patterns emerge. Chart meets requirements outlined above.
# 20.0 pts

# column geometry (a bar chart), mapping a value to
# the x-axis and year to the y-axis. Note that by default, column geometry
# will us the "sum" of all of the y-values, so that the chart is actually of the
# TOTAL value of all of the value in the x-axis
ggplot(data = analysis_data) +
  geom_col(mapping = aes(x = year, y = price))

# Map
# Thoughtfully creates a map of trends across the U.S., describes *why* it was designed that way, and what patterns emerge. Chart meets requirements outlined above.
# 20.0 pts

# Load the county shapefile and join on county fips
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")
View(county_shapes)

map_data <- county_shapes %>% 
  left_join(Sample_data, by="fips") 
View(map_data)

#blank map
ggplot(county_shapes) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white",
    size = .1
  ) +
  coord_map()

# Report Quality and Completeness
# The report is successfully compiled into a hosted website. It doesn't include any unnecessary code, and the code (analysis.R) is well written and commented, lacking any linting errors.
# 20.0 pts
