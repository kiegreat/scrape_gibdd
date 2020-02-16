
library(tidyverse)
library(lubridate)
library(ggridges)

extrafont::loadfonts(device = "win")

md <- readRDS('main_data.rds') %>% distinct_all()
# pd <- readRDS('participants_data.rds') %>% distinct_all()
# vd <- readRDS('vehicles_data.rds') %>% distinct_all()

# 1. Accidents by season and hour ----

df1 <- md %>% 
  mutate(
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

xlabels <- unique(str_c(df1$hour_id, ':00'))

gg1 <- ggplot(df1, aes(x = hour_id, fill = seas) ) +
  geom_histogram(binwidth = 1) +
  facet_grid(seas ~ .) +
  scale_x_continuous(breaks = unique(df1$hour_id), labels = xlabels, expand = c(0, 0)) +
  scale_fill_manual(values = c('#d7f0c3', '#bae4bc', '#7bccc4', '#2b8dbe')) +
  #scale_fill_brewer(palette = 4) +
  coord_cartesian(clip = "off") +
  labs(
    title = 'Время ДТП с наездом на пешеходов по сезонам',
    subtitle = 'данные ГИБДД за 2015-2019 гг.',
    x = '',
    y = '',
    caption = 'Автор - Кирилл Гудков / Инструмент - R / vk.com/astr.city.data'
  ) +
  theme(
    panel.background = element_blank(),
    legend.position = 'none',
    title = element_text(family = 'Roboto Medium'), 
    plot.subtitle = element_text(family = 'Roboto Light'), 
    plot.caption = element_text(family = 'Roboto Light'), 
    legend.title = element_text(family = 'Roboto'),
    text = element_text(family = 'Roboto'),
    strip.background = element_blank()
  ) +
  geom_vline(xintercept = c(6.5,8.5,16.5,18.5), linetype = 'dashed')

gg1
ggsave(filename = 'gg1.png', plot = gg1, device = 'png', path = 'plots', dpi = 450, width = 10, height = 5)

rm(df1, gg1, xlabels); gc()

