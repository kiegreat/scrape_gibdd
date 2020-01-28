
library(tidyverse)
library(httr)
library(jsonlite)
library(XML)
library(ggmap)
library(RColorBrewer)
library(lubridate)

df <- readRDS('main_data.rds') %>% mutate(date = as.Date(date, format = '%d.%m.%Y'))

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
# saveRDS(df_coords, 'geocoded.rds')

df_coords <- readRDS('geocoded.rds') %>% distinct()

# - put on a map -

df_map <- df %>% 
  select(address, date, num_dead) %>% 
  inner_join(df_coords, by = 'address') %>% 
  mutate(
    date = as.Date(date, format = '%d.%m.%Y'),
    year = year(date),
    address = str_replace_all(address, pattern = ',$', replacement = ''),
    to_keep = str_detect(address, pattern = '.*[0-9].*'),
    num_dead = as.numeric(num_dead)
  ) %>% 
  filter(
    lon > 47.97, lon < 48.12, lat > 46.29, lat < 46.4,
    num_dead > 0,
    to_keep == TRUE
  )

style_string <- '&style=element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road.arterial%7Celement:labels%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels%7Cvisibility:off&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Cvisibility:off&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e'
basemap <- get_googlemap(center = c(lon = 48.033574, lat = 46.347869), zoom = 12, inject = style_string, maptype = 'roadmap')

ggmap(basemap) +
  stat_density2d(data = df_map, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral")))

ggmap(basemap) +
  geom_point(data = df_map, aes(x = lon, y = lat), col = 'red') +
  facet_wrap(~year)

# ----

df2 <- df %>% 
  mutate(
    astr = str_detect(address, pattern = 'Астрахань'),
    date = as.Date(date, format = '%d.%m.%Y'),
    year = year(date)
  ) %>% 
  filter(num_dead > 0, astr == T, type == 'Наезд на пешехода')

table(df2$year)















