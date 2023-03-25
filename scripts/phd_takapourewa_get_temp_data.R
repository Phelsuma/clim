# # # Get & Write Climate Data from Clifro # # #  
# gets daily temp data from cliflo (NIWA) via clifro for Takapourewa & writes data
# as file "phd_takapourewa_clifro_temps_1990_present.csv"
# Returns data from 2 stations: Takapourewa_old = cf_station(4153), Takapourewa_AWS = cf_station(26169) & rbinds

library(tidyverse)
library(clifro)
library(lubridate) # may not need now

# login 
user = cf_user(username = UN, password = PW)

# Choose the datatypes https://cran.r-project.org/web/packages/clifro/vignettes/choose-datatype.html
temps = cf_datatype(4, 2, 1) # need to reduce this to return only temp variables of interest 

# Send the query to CliFlo and retrieve the data

# Get current Takapourewa climate data
Takapourewa_AWS = cf_station(26169) # Takapourewa AWS (26169)
daily_datalist_2018_present = cf_query(user = user, 
                          datatype = temps, 
                          station = Takapourewa_AWS,
                          start_date = "2018-09-19 00", # doesn't have data prior to 2018-09-19
                          end_date = "2023-01-01 00") # modify as needed

dim(daily_datalist_2018_present)
head(daily_datalist_2018_present)
tail(daily_datalist_2018_present)

Takapourewa_old = cf_station(4153) # Takapourewa old station (4153)
daily_datalist_pre2018 = cf_query(user = user, 
                                  datatype = temps, 
                                  station = Takapourewa_old,
                                  start_date = "1990-01-01 00",
                                  end_date = "2018-09-19 00") # use '26169' for dates after 2018-09-19 

dim(daily_datalist_pre2018)
head(daily_datalist_pre2018)
tail(daily_datalist_pre2018)

# create tibble for pre_2018 & 2018_present
dly_temp_tbbl_current <- as.data.frame(daily_datalist_2018_present) %>%
          janitor::clean_names() %>% 
          as_tibble() %>% 
          select(date_local, tmax_c, tmin_c)
dly_temp_tbbl_pre_2018 <- as.data.frame(daily_datalist_pre2018) %>%
          janitor::clean_names() %>%
          as_tibble() %>%
          select(date_local, tmax_c, tmin_c)

# merge tibbles
takapourewa_full_daily_temps <- bind_rows(dly_temp_tbbl_pre_2018, dly_temp_tbbl_current)

# change date column class
takapourewa_full_daily_temps$date_local <- ymd_hm(takapourewa_full_daily_temps$date_local) # wants to be seperate

# quick skim
skimr::skim(takapourewa_full_daily_temps) # check classes for each column 

takapourewa_full_daily_temps %>% duplicated() %>% table() # this should be a yay! still!  

## write CSV file "phd_takapourewa_clifro_temps_2018_present" for takapourewa temp data tibble ## 
takapourewa_full_daily_temps %>% write_csv(file = "phd_takapourewa_clifro_temps_1990_present.csv", col_names = TRUE, na = "NA")
