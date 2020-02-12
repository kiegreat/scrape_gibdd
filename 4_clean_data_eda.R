
library(tidyverse)

md <- readRDS('main_data.rds') %>% distinct_all()
pd <- readRDS('participants_data.rds') %>% distinct_all()
#vd <- readRDS('vehicles_data.rds') %>% distinct_all()
vd <- readRDS('violations_data.rds') %>% distinct_all()

glimpse(md)
glimpse(pd)
glimpse(vd)

# - visualization ideas -

# 1. Hist of acc by hour
# 2. Hist of acc by season
# 3. Hist of acc by weather
# 4. Map of acc by year
# 5. Participants of an acc by sex
# 6. Participants of an acc by driving exp
# 7. Reasons of acc

# 1. ----

df1 <- md %>% 
  mutate(hour = hour(timestamp)) %>% 
  filter(type == 'Наезд на пешехода')

ggplot(df1, aes(x = hour)) + geom_bar()

# 2. ---

df2 <- md %>% 
  mutate(
    month = month(timestamp), 
    seas = case_when(
      month %in% c(1,2,12) ~ 'winter',
      month %in% c(3,4,5) ~ 'spring',
      month %in% c(6,7,8) ~ 'summer',
      month %in% c(9,10,11) ~ 'fall'
    ),
    seas = factor(seas, level = c('spring', 'summer', 'fall', 'winter'))
  ) %>% 
  filter(type == 'Наезд на пешехода')

ggplot(df2, aes(x = seas)) + geom_bar()

# 3. ---

unique(md$weather)

df3 <- md %>% 
  filter(type == 'Наезд на пешехода')

ggplot(df3, aes(x = weather)) + geom_bar()

# pointless

# 5. ---

df5 <- md %>% 
  inner_join(pd, by = 'timestamp') %>% 
  mutate(driving_exp = as.numeric(driving_expirience)) %>% 
  filter(type == 'Наезд на пешехода', participant_category == 'Водитель', driving_exp < 70)

ggplot(df5, aes(x = gender)) + geom_bar()
ggplot(df5, aes(x = driving_exp)) + geom_bar()

ggplot(df5, aes(x = driving_exp)) + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = c(5,10,15,20), col = 'red')

# НАДО ПЕРЕСОБИРАТЬ ОПЯТЬ ДАННЫЕ С НОВЫМ PRIMARY_KEY (timestamp + address)





