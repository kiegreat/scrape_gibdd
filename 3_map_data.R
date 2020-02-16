
library(tidyverse)
library(httr)
library(jsonlite)
library(XML)
library(ggmap)
library(RColorBrewer)
library(lubridate)

extrafont::loadfonts(device = "win")

style_string <- '&style=element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road.arterial%7Celement:labels%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels%7Cvisibility:off&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Cvisibility:off&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e'
basemap <- get_googlemap(center = c(lon = 48.033574, lat = 46.347869), zoom = 12, inject = style_string, maptype = 'roadmap')

maps_theme <-   theme(
  axis.ticks = element_blank(), 
  axis.text = element_blank(), 
  panel.grid = element_blank(),
  strip.background = element_blank(),
  title = element_text(family = 'Roboto Medium'), 
  plot.subtitle = element_text(family = 'Roboto Light'), 
  plot.caption = element_text(family = 'Roboto Light'), 
  legend.title = element_text(family = 'Roboto'),
  text = element_text(family = 'Roboto')
)

df <- readRDS('main_data.rds') %>% 
  distinct_all() %>% 
  mutate(
    year_id = year(timestamp),
    hour_id = hour(timestamp),
    month_id = month(timestamp),
    time_id = hms(str_replace_all(timestamp, pattern = '^.* ', replacement = '')),
    seas = case_when(
      month_id %in% c(1,2,12) ~ 'winter',
      month_id %in% c(3,4,5) ~ 'spring',
      month_id %in% c(6,7,8) ~ 'summer',
      month_id %in% c(9,10,11) ~ 'fall'
    ),
    seas = factor(seas, level = c('spring', 'summer', 'fall', 'winter'), labels = c('Весна', 'Лето', 'Осень', 'Зима'))
  ) %>% 
  filter(
    type == 'Наезд на пешехода', 
    lon > 47.67, lon < 48.42, lat > 45.29, lat < 46.7
  )

# 1. Static map of accidents ----

df_map <- df %>% 
  mutate(to_keep = str_detect(address, pattern = '.*[0-9].*')) %>% 
  filter(
    to_keep == TRUE,
    type == 'Наезд на пешехода'
  )

map1 <- ggmap(basemap) +
  geom_point(data = df_map, aes(x = lon, y = lat), alpha = 0.17, shape = 16, col = 'red') +
  maps_theme +
  theme(legend.position = 'none') +
  labs(
    title = 'Карта ДТП с участием пешеходов',
    subtitle = 'данные ГИБДД за 2015-2019 гг.',
    x = '',
    y = '',
    caption = 'Автор - Кирилл Гудков / Инструмент - R / vk.com/astr.city.data'
  )

map1
ggsave(filename = 'map1.png', plot = map1, device = 'png', path = 'plots', dpi = 400, width = 7, height = 7)

# 2. Accidents on a map by hour ----

# save_map_for_each_day <- function(hour) {
#   
#   df <- df_map %>% filter(hour_id == hour)
#   
#   map <- ggmap(basemap) +
#     geom_point(data = df, aes(x = lon, y = lat), alpha = 0.8, shape = 16, col = 'red') +
#     maps_theme +
#     theme(legend.position = 'none') +
#     labs(
#       title = 'Наезды на пешеходов по времени дня',
#       subtitle = str_c(hour, ':00'),
#       x = '',
#       y = ''
#     )
#   
#   ggsave(filename = str_c(hour, '.png'), plot = map, device = 'png', path = 'plots/dynamic_map', dpi = 400, width = 7, height = 7)
#   print(hour)
# }
# 
# walk(.x = c(0:23), .f = save_map_for_each_day)


