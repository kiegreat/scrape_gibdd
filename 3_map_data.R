
library(tidyverse)
library(httr)
library(jsonlite)
library(XML)
library(ggmap)
library(RColorBrewer)
library(lubridate)

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
  mutate(
    date = as.Date(date, format = '%d.%m.%Y'),
    address = str_replace_all(address, pattern = ',$', replacement = ''),
    timestamp = ymd_hms(str_c(date, ' ', time, ':00'))
  ) %>% 
  mutate_at(.vars = vars('vehicles', starts_with('num')), .funs = as.integer) %>% 
  select(-lon, -lat, -time)

# Geocoding ----
#
# key <- '4a954138-376f-4b20-a94f-97b02bec1cd6'
# 
# geocode_beta <- function(address) {
#   
#   loc <- address %>% 
#     str_replace_all(pattern = ',', replacement = '') %>%
#     str_replace_all(pattern = ' ', replacement = '+')
#   
#   request <- str_c('https://geocode-maps.yandex.ru/1.x/?apikey=', key, '&geocode=', loc) %>% URLencode()
#   
#   xml <- request %>% 
#     readLines(warn = FALSE) %>% 
#     str_c(sep = "\n", collapse = "") %>% 
#     xmlParse(asText = TRUE) %>% 
#     xmlToList()
#   
#   pos <- xml$GeoObjectCollection$featureMember$GeoObject$Point$pos
#   
#   lon <- pos %>% str_replace_all(pattern = ' .*$', replacement = '') %>% as.numeric()
#   lat <- pos %>% str_replace_all(pattern = '^.* ', replacement = '') %>% as.numeric()
#   
#   result <- data.frame('address' = address, 'lon' = lon, 'lat' = lat)
#   print(str_c(address, ' // ', Sys.time()) )
#   
#   return(result)
# }
# geocode <- possibly(.f = geocode_beta, otherwise = NULL)
# 
# df_coords <- map_df(.x = df$address, .f = geocode)
# df_coords <- df_coords %>%  distinct()
#
# saveRDS(df_coords, 'geocoded.rds')
#
# ----

df_coords <- readRDS('geocoded.rds')
df_fin <- df %>% left_join(df_coords, by = 'address')


# - put on a map -

df_map <- df_fin %>% 
  mutate(to_keep = str_detect(address, pattern = '.*[0-9].*')) %>% 
  filter(
    lon > 47.67, lon < 48.42, lat > 45.29, lat < 46.7,
    to_keep == TRUE,
    type == 'Наезд на пешехода'
  )

ggmap(basemap) +
  stat_density2d(data = df_map, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = "polygon") +
  geom_point(data = df_map, aes(x = lon, y = lat), alpha = 0.2) +
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
  maps_theme +
  theme(legend.position = 'none') +
  labs(x = '', y = '') +
  facet_grid(~ year)



# ----













