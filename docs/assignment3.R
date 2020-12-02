# Introduction + summary statistics
# Introduces the report and dataset, and provides at least 5 computed summary statistics from the data.
# 20.0 pts
library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)
library(styler)

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)
#incarceration_trends <- incarceration_trends %>%
#  mutate(year = as.Date(year))
#View(incarceration_trends)

incarceration_summary_info <- list()
incarceration_summary_info$obs_incarceration <- nrow(incarceration_trends)
incarceration_summary_info$num_features <- ncol(incarceration_trends)
  
# Cross reference total jail pop. rate
#kingcounty2018 <- incarceration_trends %>%
#  filter(county_name == "King County", state == "WA", year == 2018) %>%
#  pull(total_jail_pop_rate)

# Grab a few columns
analysis_data <- na.omit(incarceration_trends) %>%
  select(aapi_jail_pop_rate, aapi_prison_pop_rate, latinx_jail_pop_rate, latinx_prison_pop_rate, black_jail_pop_rate, black_prison_pop_rate,
         white_jail_pop_rate, white_prison_pop_rate, native_jail_pop_rate, native_prison_pop_rate, total_prison_pop_rate, total_jail_pop_rate,
         aapi_pop_15to64, latinx_pop_15to64, black_pop_15to64, white_pop_15to64, native_pop_15to64, year, fips, state, county_name, total_jail_pretrial) %>%
#comparing how much of each race is in jail/prison compared to overall jail/prison population
#(i.e what do the jails/prisons look like in terms of race?)
  mutate(black_jail_ratio = black_jail_pop_rate / total_jail_pop_rate,
         aapi_jail_ratio = aapi_jail_pop_rate / total_jail_pop_rate,
         white_jail_ratio = white_jail_pop_rate / total_jail_pop_rate,
         latinx_jail_ratio = latinx_jail_pop_rate / total_jail_pop_rate,
         native_jail_ratio = native_jail_pop_rate / total_jail_pop_rate,
         black_prison_ratio = black_prison_pop_rate / total_prison_pop_rate,
         aapi_prison_ratio = aapi_prison_pop_rate / total_prison_pop_rate,
         white_prison_ratio = white_prison_pop_rate / total_prison_pop_rate,
         latinx_prison_ratio = latinx_prison_pop_rate / total_prison_pop_rate,
         native_prison_ratio = native_prison_pop_rate / total_prison_pop_rate) %>%
#where is this race incarcerated more?
#are they more likely to be in jail or prison?
  mutate(black_jail_or_prison = black_jail_ratio / black_prison_ratio,
         aapi_jail_or_prison = aapi_jail_ratio / aapi_prison_ratio,
         white_jail_or_prison = white_jail_ratio / white_prison_ratio,
         native_jail_or_prison = native_jail_ratio / native_prison_ratio,
         latinx_jail_or_prison = latinx_jail_ratio / latinx_prison_ratio) %>%
#total ratio in jail AND prison
  mutate(black_jail_and_prison = black_jail_ratio + black_prison_ratio,
         aapi_jail_and_prison = aapi_jail_ratio + aapi_prison_ratio,
         white_jail_and_prison = white_jail_ratio + white_prison_ratio,
         native_jail_and_prison = native_jail_ratio + native_prison_ratio,
         latinx_jail_and_prison = latinx_jail_ratio + latinx_prison_ratio,
         total_jail_and_prison = total_jail_pop_rate + total_prison_pop_rate) %>%
#how much of the community is in jail/prison
  mutate(black_jail_pop_ratio = black_jail_pop_rate / black_pop_15to64,
         aapi_jail_pop_ratio = aapi_jail_pop_rate / aapi_pop_15to64,
         white_jail_pop_ratio = white_jail_pop_rate / white_pop_15to64,
         latinx_jail_pop_ratio = latinx_jail_pop_rate / latinx_pop_15to64,
         native_jail_pop_ratio = native_jail_pop_rate / native_pop_15to64,
         black_prison_pop_ratio = black_prison_pop_rate / black_pop_15to64,
         aapi_prison_pop_ratio = aapi_prison_pop_rate / aapi_pop_15to64,
         white_prison_pop_ratio = white_prison_pop_rate / white_pop_15to64,
         latinx_prison_pop_ratio = latinx_prison_pop_rate / latinx_pop_15to64,
         native_prison_pop_ratio = native_prison_pop_rate / native_pop_15to64)
#View(analysis_data)

analysis_info <- list()
analysis_info$obs_incarceration <- nrow(analysis_data)
analysis_info$num_features <- ncol(analysis_data)

# Time Trend Chart
# Thoughtfully creates a chart of trends over time, describes *why* it was designed that way, and what patterns emerge. Chart meets requirements outlined above.
# 20.0 pts

# Draw a line plot (with line geometry)
# trend by year
black_jail_plot<- ggplot(analysis_data) +
  geom_line(aes(x = year, y = total_jail_pop_rate, group = black_jail_ratio, color = black_jail_ratio)) +
  labs(x = "Year", y = "The Growth of Black Jail Incarceration Over Time",
       title = "Black Jail Incarceration")

black_prison_plot<- ggplot(analysis_data) +
  geom_line(aes(x = year, y = total_prison_pop_rate, group = black_prison_ratio, color = black_prison_ratio)) +
  labs(x = "Year", y = "The Growth of Black Prison Incarceration Over Time",
       title = "Black Prison Incarceration")


# Variable Comparison Chart
# Thoughtfully creates a chart comparing two variables, describes *why* it was designed that way, and what patterns emerge. Chart meets requirements outlined above.
# 20.0 pts

black_jail_prison_chart <- ggplot(data = analysis_data) +
  geom_col(mapping = aes(x = year, y = black_jail_or_prison)) +
  geom_line(mapping = aes(x = year, y = total_jail_and_prison)) +
  ggtitle("Comparing the Black Prison and Jail Incarceration Rate by Year") +
  labs(fill = "Prison > Jail", x = "Year", y = "Prison > Jail")

# Map
# Thoughtfully creates a map of trends across the U.S., describes *why* it was designed that way, and what patterns emerge. Chart meets requirements outlined above.
# 20.0 pts

# Load the county shapefile and join on county fips
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")
#View(county_shapes)

map_data <- county_shapes %>%
  left_join(analysis_data, by="fips")
#View(map_data)

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

black_jail_incarceration_map <- ggplot(analysis_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_ratio),
    color = "#d3d3d3", size = .3
  ) +
  scale_fill_continuous(limits = c(0, max(analysis_data$black_jail_pop_ratio)), na.value = "white",  low = "yellow", high = "red") +
  blank_theme +
  ggtitle("How much of the Black population is in jail?") +
  coord_map() +
  labs(fill = "Black incarceration rate by County")

black_prison_incarceration_map <- ggplot(analysis_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_prison_pop_ratio),
    color = "#d3d3d3", size = .3
  ) +
  scale_fill_continuous(limits = c(0, max(analysis_data$black_prison_pop_ratio)), na.value = "white",  low = "yellow", high = "red") +
  blank_theme +
  ggtitle("How much of the Black population is in prison?") +
  coord_map() +
  labs(fill = "Black Incarceration rate over Black population rate by County")
