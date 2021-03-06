
library(readxl)
library(tidyverse)
library(lubridate)

# 1. Get all names of zip files ----

if(dir.exists('data/xlsx')) { print('Folder is ready') } else { dir.create(path = 'data/xlsx') }
f <- list.files(path = 'data', pattern = '.zip')

# 2. Explore how many columns and rows are there in each file and on every worksheet inside ----

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

# Good news: there are always 8 columns. But the number of rows always differs :(

# 3. Get data from all xls files ----

# Function for main data extraction from a worksheet:

main_data_extraction <- function(worksheet) {
  
  df <- read_xls('data/xlsx/temp_xls.xls', sheet = worksheet)
  names(df) <- c('col1', 'col2', 'col3', 'col4', 'col5', 'col6', 'col7', 'col8')
  df <- df %>% fill(col1)
  
  output <- data.frame(
    date = df %>% filter(col1 == 'Дата:') %>% pull(col2),
    type = df[4,2] %>% pull(col2),
    address = df %>% filter(col1 == 'Адрес:') %>% pull(col2),
    lon = df %>% filter(col1 == 'Широта:') %>% pull(col2),
    lat = df %>% filter(col3 == 'Долгота:') %>% pull(col4),
    weather = df %>% filter(col1 == 'Состояние погоды:') %>% pull(col2),
    road_conditions = df %>% filter(col1 == 'Состояние проезжей части:') %>% pull(col2),
    lightning = df %>% filter(col1 == 'Освещение:') %>% pull(col2),
    vehicles = df %>% filter(col1 == 'Количество ТС') %>% pull(col2),
    time = df %>% filter(col3 == 'Время:') %>% pull(col4),
    num_of_participants = df %>% filter(col3 == 'Число участников') %>% pull(col4),
    num_dead = df %>% filter(col5 == 'Число погибших') %>% pull(col6),
    num_injured = df %>% filter(col7 == 'Число раненых') %>% pull(col8)
  ) %>% 
    mutate(timestamp = ymd_hms(str_c(as.Date(date, format = '%d.%m.%Y'), ' ', time, ':00')) )
  
  return(output)
}

# Function for additional data extraction from a worksheet:

helper <- function(x, type = 'participants') {
  
  if(type == 'participants') {
    
    result <- data.frame(
      vehicle_id = x %>% distinct(car) %>% pull(car),
      participant_category = x %>% filter(col1 == 'Категория участника') %>% pull(col2),
      gender = x %>% filter(col1 == 'Пол') %>% pull(col2),
      is_drunk = x %>% filter(col1 == 'Степень опьянения') %>% pull(col2),
      injuries_category = x %>% filter(col1 == 'Степень тяжести последствий') %>% pull(col2),
      used_safety_belt = x %>% filter(col5 == 'Использовался ли ремень') %>% pull(col6),
      driving_expirience = x %>% filter(col5 == 'Водительский стаж') %>% pull(col6)
    )
    
  } else {
    
    if(length(unique(x$car)) == length(x %>% filter(col1 == 'Непосредственные нарушения ПДД') %>% pull(col2))) {
      result <- data.frame(
        vehicle_id = x %>% distinct(car) %>% pull(car),
        main_violation = x %>% filter(col1 == 'Непосредственные нарушения ПДД') %>% pull(col2)
      )
    } else { }
  }
  
  return(result)
}
helper_possible <- possibly(.f = helper, otherwise = NULL)

additional_data_extraction <- function(worksheet, datatype = 'vehicles') {
  
  df <- read_xls('data/xlsx/temp_xls.xls', sheet = worksheet)
  names(df) <- c('col1', 'col2', 'col3', 'col4', 'col5', 'col6', 'col7', 'col8')
  df <- df %>% fill(col1)
  
  cars <- df %>% 
    mutate(car = ifelse(str_detect(string = col1, pattern = '^ТС.*'), col1, NA)) %>% 
    fill(car) %>% 
    select(car) %>% 
    bind_cols(df) %>% 
    filter(!is.na(car)) %>% 
    group_split(car)
  
  if(datatype == 'vehicles') {
    
    result <- data.frame(
      vehicle_id = cars %>% distinct(car) %>% pull(car),
      type_of_vehicle = cars %>% filter(col1 == 'Тип ТС') %>% pull(col2),
      model_of_vehicle = cars %>% filter(col1 == 'Марка/модель ТС') %>% pull(col2),
      color = cars %>% filter(col1 == 'Цвет') %>% pull(col2),
      technical_issues = cars %>% filter(col1 == 'Технические неисправности') %>% pull(col2),
      year = cars %>% filter(col5 == 'Год выпуска') %>% pull(col6),
      engine_location = cars %>% filter(col5 == 'Расположение руля, тип привода') %>% pull(col6),
      is_escaped = cars[which(cars$col1 == 'Тип ТС') - 1, 3] %>% unname()
    ) %>% 
      mutate(
        date = df %>% filter(col1 == 'Дата:') %>% pull(col2),
        time = df %>% filter(col3 == 'Время:') %>% pull(col4),
        date = as.Date(date, format = '%d.%m.%Y'),
        timestamp = ymd_hms(str_c(date, ' ', time, ':00')),
        address = df %>% filter(col1 == 'Адрес:') %>% pull(col2)
      ) %>% 
      select(-date, -time)
    
  } else if(datatype == 'participants') {
    result <- map_df(.x = cars, .f = ~helper(.x, type = 'participants')) %>% 
      mutate(
        date = df %>% filter(col1 == 'Дата:') %>% pull(col2),
        time = df %>% filter(col3 == 'Время:') %>% pull(col4),
        date = as.Date(date, format = '%d.%m.%Y'),
        timestamp = ymd_hms(str_c(date, ' ', time, ':00')),
        address = df %>% filter(col1 == 'Адрес:') %>% pull(col2)
      ) %>% 
      select(-date, -time)
  } else {
    result <- map_df(.x = cars, .f = ~helper_possible(.x, type = 'violations')) %>% 
      mutate(
        date = df %>% filter(col1 == 'Дата:') %>% pull(col2),
        time = df %>% filter(col3 == 'Время:') %>% pull(col4),
        date = as.Date(date, format = '%d.%m.%Y'),
        timestamp = ymd_hms(str_c(date, ' ', time, ':00')),
        address = df %>% filter(col1 == 'Адрес:') %>% pull(col2)
      ) %>% 
      select(-date, -time)
  }
  
  return(result)
}
additional_data_extraction_possible <- possibly(.f = additional_data_extraction, otherwise = NULL)

# Main function for processing:

main_function <- function(filename, datatype = 'main') {
  
  unzip(zipfile = str_c('data/', filename), exdir = 'data/xlsx')
  file.rename(from = 'data/xlsx/РљР°СЂС‚РѕС‡РєРё Р”РўРџ, С‡Р°СЃС‚СЊ 1.xls', to = 'data/xlsx/temp_xls.xls')
  worksheets <- readxl::excel_sheets(path = 'data/xlsx/temp_xls.xls')
  
  if(datatype == 'main') {
    result <- map_df(.x = worksheets, .f = ~main_data_extraction(.x))
  } else if(datatype == 'vehicles') {
    result <- map_df(.x = worksheets, .f = ~additional_data_extraction(.x, datatype = 'vehicles'))
  } else if (datatype == 'participants') {
    result <- map_df(.x = worksheets, .f = ~additional_data_extraction_possible(.x, datatype = 'participants'))
  } else {
    result <- map_df(.x = worksheets, .f = ~additional_data_extraction(.x, datatype = 'violations'))
  }
  
  return(result)
}

# 4. Launch ----

main_data <- map_df(.x = f, .f = ~main_function(filename = .x, datatype = 'main'))
vehicles_data <- map_df(.x = f, .f = ~main_function(filename = .x, datatype = 'vehicles'))
participants_data <- map_df(.x = f, .f = ~main_function(filename = .x, datatype = 'participants'))
violations_data <- map_df(.x = f, .f = ~main_function(filename = .x, datatype = 'violations'))

# 5. Postprocessing ----

vehicles_data <- vehicles_data %>% 
  mutate(
    address = str_replace_all(address, pattern = ',$', replacement = ''),
    year = as.numeric(year),
    color = ifelse(color == 'Не заполнено', NA, color),
    engine_location = ifelse(engine_location == 'Не заполнено', NA, engine_location),
    engine_location = case_when(
      engine_location %in% c("С передним приводом", "Передний") ~ 'Передний',
      engine_location %in% c("С задним приводом", "Задний") ~ 'Задний',
      engine_location %in% c("Иное расположение рулевого управления", "Иной") ~ 'Иной',
      engine_location %in% c("Полноприводные", "Полноприводный") ~ 'Полноприводный'
    ),
    is_escaped = case_when(
      is_escaped %in% c("Осталось на месте ДТП", "Нет") ~ "Нет",
      is_escaped %in% c(
        "Скрылось с места ДТП", 
        "Скрылось и установлено в срок до 1 суток", 
        "Да",
        "Скрылось и установлено в срок от 1 до 3 суток", 
        "Скрылось и установлено в срок от 3 до 10 суток",
        "от 1  до 3 сут.",
        "до 1 сут.", 
        "свыше 10 сут.",
        "от 3 до 10 сут."
      ) ~ "Да"
    )
  )

participants_data <- participants_data %>% 
  mutate(
    address = str_replace_all(address, pattern = ',$', replacement = ''),
    driving_expirience = as.numeric(driving_expirience)
  )

violations_data <- violations_data %>% 
  mutate(address = str_replace_all(address, pattern = ',$', replacement = ''))

saveRDS(main_data, 'main_data.rds')
saveRDS(vehicles_data, 'vehicles_data.rds')
saveRDS(participants_data, 'participants_data.rds')
saveRDS(violations_data, 'violations_data.rds')




