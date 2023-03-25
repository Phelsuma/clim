# # # Get & Write Climate Data from Clifro # # #  
# gets daily temp data from cliflo (NIWA) via clifro writes data
# as file ""

# should make this a function

library(tidyverse)
library(clifro)
library(lubridate) # may not need now

# login 
user = cf_user(username = UN, password = PW)

# Choose the datatypes https://cran.r-project.org/web/packages/clifro/vignettes/choose-datatype.html
temps = cf_datatype(4, 2, 1) # need to reduce this to return only temp variables of interest 

# Send the query to CliFlo and retrieve the data

#Crail Bay----------------------------------------------------------------------
# Pelorus Sound, Crail Bay (4232) 
# (-41.10308, 173.96406) 48.7kms from Takapourewa

# Get current Crail Bay climate data
crailBay_ws = cf_station(4232) 
crail_daily_datalist = cf_query(user = user, 
                                       datatype = temps, 
                                       station = crailBay_ws,
                                       start_date = "1990-01-01 00",
                                       end_date = "2023-01-01 00") 
#Total number of rows output = 11065
plot(Crail_daily_datalist)

crail_dly_temp_tbbl <- as.data.frame(crail_daily_datalist) %>%
          janitor::clean_names() %>% 
          as_tibble() %>% 
          select(date_local, tmax_c, tmin_c) %>% 
          rename(c('crail_tmax_c' = tmax_c, 'crail_tmin_c' = tmin_c))

dim(crail_dly_temp_tbbl)
head(crail_dly_temp_tbbl)
tail(crail_dly_temp_tbbl)

# change date column class
crail_dly_temp_tbbl$date_local <- ymd_hm(crail_dly_temp_tbbl$date_local) # wants to be seperate

# quick skim
skimr::skim(crail_dly_temp_tbbl) # check classes for each column 

# Repeat for Nelson--------------------------------------------------------------------
# Nelson Aero (4241)
# (-41.299, 173.226) 95.9km from Takapourewa

# Get current Nelson Airport climate data
nelson_aero = cf_station(4241) 
nelson_daily_datalist = cf_query(user = user, 
                                datatype = temps, 
                                station = nelson_aero,
                                start_date = "1990-01-01 00",
                                end_date = "2023-01-01 00") 
# Total number of rows output = 11982

nelson_dly_temp_tbbl <- as.data.frame(nelson_daily_datalist) %>%
          janitor::clean_names() %>% 
          as_tibble() %>% 
          select(date_local, tmax_c, tmin_c)%>% 
          rename(c('nelson_tmax_c' = tmax_c, 'nelson_tmin_c' = tmin_c))

dim(nelson_dly_temp_tbbl)
head(nelson_dly_temp_tbbl)
tail(nelson_dly_temp_tbbl)

# change date column class
nelson_dly_temp_tbbl$date_local <- ymd_hm(nelson_dly_temp_tbbl$date_local) # wants to be seperate

# quick skim
skimr::skim(nelson_dly_temp_tbbl) # check classes for each column 

# Repeat for Brothers--------------------------------------------------------------------
# Brothers Island Aws (4395)
#(-41.1033, 174.44174) 61.3kms from Takapourewa

# Get Brothers climate data
brothers_ws = cf_station(4395)
brothers_daily_datalist = cf_query(user = user, 
                                 datatype = temps, 
                                 station = brothers_ws,
                                 start_date = "1990-01-01 00",
                                 end_date = "2023-01-01 00") 
# Total number of rows output = 9323

brothers_dly_temp_tbbl <- as.data.frame(brothers_daily_datalist) %>%
          janitor::clean_names() %>% 
          as_tibble() %>% 
          select(date_local, tmax_c, tmin_c) %>% 
          rename(c('brothers_tmax_c' = tmax_c, 'brothers_tmin_c' = tmin_c))

dim(brothers_dly_temp_tbbl)
head(brothers_dly_temp_tbbl)
tail(brothers_dly_temp_tbbl)

# change date column class
brothers_dly_temp_tbbl$date_local <- ymd_hm(brothers_dly_temp_tbbl$date_local) # wants to be seperate

# quick skim
skimr::skim(brothers_dly_temp_tbbl) # check classes for each column 

# Join 3  weather stations------------------------------------------------------

head(nelson_dly_temp_tbbl)
head(crail_dly_temp_tbbl)
head(brothers_dly_temp_tbbl)

# join
sounds_temp_join <- left_join(nelson_dly_temp_tbbl, crail_dly_temp_tbbl, by = "date_local")%>%
          left_join(., brothers_dly_temp_tbbl, by = 'date_local') 

## write CSV file "phd_sound_clifro_temps.csv" for temp data tibble ## 
sounds_temp_join %>% write_csv(file = "phd_sound_clifro_temps.csv", col_names = TRUE, na = "NA")


