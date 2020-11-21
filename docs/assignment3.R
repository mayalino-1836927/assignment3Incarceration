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
  mutate(Date = as.Date(Date))
View(incarceration_trends)

# Cross reference total jail pop. rate
kingcounty2018 <- incarceration_trends %>%
  filter(county_name == "King County", state == "WA", year == 2018) %>%
  pull(total_jail_pop_rate)

# Grab a few columns
analysis_data <- incarceration_trends %>% 
  select(black_jail_pop_rate, white_jail_pop_rate, white_prison_pop_rate, total_prison_pop_rate, total_jail_pop_rate, year, fips, state, county_name) %>%
  mutate(black_white_ratio = black_jail_pop_rate/white_jail_pop_rate) %>% 
  mutate(newvaluehere = computehere)
  mutate()
  mutate()
  mutate()
  filter(something == something)
View(analysis_data)

# Load the county shapefile and join on county fips
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")
View(county_shapes)

map_data <- county_shapes %>% 
  left_join(Sample_data, by="fips") 
View(map_data)


summary_info <- list()
summary_info$obs_incarceration <- nrow(incarceration_trends)
summary_info$num_features <- ncol(incarceration_trends)
summary_info$

  aapi_prop_jail_prison <-

  aapi_jail_to_prison <- mutate()

aapi_pretrial_rate <- mutate()

prop_aapi_pop <- incarceration_trends %>%
  group_by(incarceration_trends, ) %>%
  prop() <- filter(counties, deaths == 0) / summarize(counties$state)

aapi_total <- filter(incarceration_trends, aapi_jail_pop) %>%
  prop_aapi() <- aapi_total / sum(incarceration_trends$aapi_pop_15to64)

prop_black_pop <- incarceration_trends %>%

  prop_latinx_pop() <- incarceration_trends %>%

  prop_native_pop() <- incarceration_trends %>%

  prop_white_pop() <- incarceration_trends %>%

  prop_other_pop() <- incarceration_trends %>%

# Time Trend Chart
# Thoughtfully creates a chart of trends over time, describes *why* it was designed that way, and what patterns emerge. Chart meets requirements outlined above.
# 20.0 pts

# Draw a line plot (with line geometry) for `diamonds_sample`. The x-position
# should be mapped to carat, y-position to price, and color to cut.
ggplot(data = diamonds_sample) +
  geom_line(mapping = aes(x = carat, y = price, color = cut))

# That's kind of messy. Try using `smooth` geometry instead.
ggplot(data = diamonds_sample) +
  geom_smooth(mapping = aes(x = carat, y = price, color = cut))

# Draw a plot with column geometry (a bar chart), mapping the diamond's `cut` to
# the x-axis and `price` to the y-axis. Note that by default, column geometry
# will us the "sum" of all of the y-values, so that the chart is actually of the
# TOTAL value of all of the diamonds of that cut!
ggplot(data = diamonds_sample) +
  geom_col(mapping = aes(x = cut, y = price))


# Variable Comparison Chart
# Thoughtfully creates a chart comparing two variables, describes *why* it was designed that way, and what patterns emerge. Chart meets requirements outlined above.
# 20.0 pts
ggplot()

# Map
# Thoughtfully creates a map of trends across the U.S., describes *why* it was designed that way, and what patterns emerge. Chart meets requirements outlined above.
# 20.0 pts

# Load a shapefile of U.S. states using ggplot's `map_data()` function
state_shape <- map_data("state")
# Create a blank map of U.S. states
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white", # show state outlines
    size = .1 # thinly stroked
  ) +
  coord_map() # use a map-based coordinate system

# Report Quality and Completeness
# The report is successfully compiled into a hosted website. It doesn't include any unnecessary code, and the code (analysis.R) is well written and commented, lacking any linting errors.
# 20.0 pts
