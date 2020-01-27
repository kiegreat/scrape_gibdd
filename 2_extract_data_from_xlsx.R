
library(readxl)
library(tidyverse)

# 1. Get all names of zip files

dir.create(path = 'data/xlsx')
f <- list.files(path = 'data', pattern = '.zip')

# 2. Explore how many columns and rows are there in each file and on every worksheet inside

eda_function <- function(x) {
  
  unzip(zipfile = str_c('data/', x), exdir = 'data/xlsx')
  file.rename(from = 'data/xlsx/РљР°СЂС‚РѕС‡РєРё Р”РўРџ, С‡Р°СЃС‚СЊ 1.xls', to = 'data/xlsx/temp_xls.xls')
  
  worksheets <- readxl::excel_sheets(path = 'data/xlsx/temp_xls.xls')
  
  ncols <- map_int(.x = worksheets, .f = ~ncol(read_xls('data/xlsx/temp_xls.xls', sheet = .x)) )
  nrows <- map_int(.x = worksheets, .f = ~nrow(read_xls('data/xlsx/temp_xls.xls', sheet = .x)) )
  
  return(data.frame(ncols = ncols, nrows = nrows))
}

df_eda <- map_df(.x = f, .f = eda_function)

unique(df_eda$ncols)
unique(df_eda$nrows)

# Good news: there are always 8 columns. But the number of rows differs :(

# 3. Get data from all xls files

unzip(zipfile = str_c('data/', f[1]), exdir = 'data/xlsx')
file.rename(from = 'data/xlsx/РљР°СЂС‚РѕС‡РєРё Р”РўРџ, С‡Р°СЃС‚СЊ 1.xls', to = 'data/xlsx/temp_xls.xls')

worksheets <- readxl::excel_sheets(path = 'data/xlsx/temp_xls.xls')
df <- read_xls('data/xlsx/temp_xls.xls', sheet = worksheets[1])
names(df) <- c('col1', 'col2', 'col3', 'col4', 'col5', 'col6', 'col7', 'col8')
df <- df %>% fill(col1)

# First of all form constant part of data:

main_info <- data.frame(
  date = df %>% filter(col1 == 'Дата:') %>% pull(col2),
  type = df[4,2] %>% pull(col2),
  address = df %>% filter(col1 == 'Адрес:') %>% pull(col2),
  weather = df %>% filter(col1 == 'Состояние погоды:') %>% pull(col2),
  road_conditions = df %>% filter(col1 == 'Состояние проезжей части:') %>% pull(col2),
  lightning = df %>% filter(col1 == 'Освещение:') %>% pull(col2),
  vehicles = df %>% filter(col1 == 'Количество ТС') %>% pull(col2),
  time = df %>% filter(col3 == 'Время:') %>% pull(col4),
  num_of_participants = df %>% filter(col3 == 'Число участников') %>% pull(col4),
  num_dead = df %>% filter(col5 == 'Число погибших') %>% pull(col6),
  num_injured = df %>% filter(col7 == 'Число раненых') %>% pull(col8)
)

# Next lets extract all varying data:

participants_info <- data.frame(
  is_escaped = df %>% filter(col1 == 'Сведения об оставлении места ДТП') %>% pull(col2),
  type_of_vehicle = df %>% filter(col1 == 'Тип ТС') %>% pull(col2),
  model_of_vehicle = df %>% filter(col1 == 'Марка/модель ТС') %>% pull(col2),
  color = df %>% filter(col1 == 'Цвет') %>% pull(col2),
  technical_issues = df %>% filter(col1 == 'Технические неисправности') %>% pull(col2),
  participant_category = df %>% filter(col1 == 'Категория участника') %>% pull(col2),
  gender = df %>% filter(col1 == 'Пол') %>% pull(col2),
  is_drunk = df %>% filter(col1 == 'Степень опьянения') %>% pull(col2),
  injuries_category = df %>% filter(col1 == 'Степень тяжести последствий') %>% pull(col2),
  main_violation = df %>% filter(col1 == 'Непосредственные нарушения ПДД') %>% pull(col2),
  additional_violations = df %>% filter(col1 == 'Сопутствующие нарушения ПДД') %>% pull(col2),
  year = df %>% filter(col5 == 'Год выпуска') %>% pull(col6),
  used_safety_belt = df %>% filter(col5 == 'Использовался ли ремень') %>% pull(col6),
  driving_expirience = df %>% filter(col5 == 'Водительский стаж') %>% pull(col6),
  engine_location = df %>% filter(col5 == 'Расположение руля, тип привода') %>% pull(col6)
)

# Join together:








# x <- f[1]
# 
# unzip(zipfile = str_c('data/', x), exdir = 'data/xlsx')
# file.rename(from = 'data/xlsx/РљР°СЂС‚РѕС‡РєРё Р”РўРџ, С‡Р°СЃС‚СЊ 1.xls', to = 'data/xlsx/temp_xls.xls')
# 
# worksheets <- readxl::excel_sheets(path = 'data/xlsx/temp_xls.xls')
# 
# # df_temp <- readxl::read_xls('data/xlsx/temp_xls.xls', sheet = worksheets[1])
# # df_temp2 <- readxl::read_xls('data/xlsx/temp_xls.xls', sheet = worksheets[2])
# 
# ncols <- map_int(.x = worksheets, .f = ~ncol(read_xls('data/xlsx/temp_xls.xls', sheet = .x)) )
# nrows <- map_int(.x = worksheets, .f = ~nrow(read_xls('data/xlsx/temp_xls.xls', sheet = .x)) )


















