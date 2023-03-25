# fill gaps in Time Series 
library(tidyverse)
library(magrittr)
library(lubridate)
library(imputeTS)
library(purrr)
library(tsibble)
library(feasts)

# read in takapourewa data
neighbors_df  <- readr::read_csv("data/phd_sound_clifro_temps.csv")
# read in neighbor station data
takapourewa_df <- read_csv("data/phd_takapourewa_clifro_temps_1990_present.csv") %>%
          rename(c('takapourewa_tmax_c' = tmax_c,
                   'takapourewa_tmin_c' = tmin_c
          ))

# merge
takapourewa_sounds_full_temps_df <- left_join(neighbors_df, takapourewa_df, by = 'date_local') %>%
          mutate(
                    #date = lubridate::ymd_hms(date_local)) # # # tsibble does not like this
                    date = as.Date(date_local), # # # only likes being in class "Date" to coerece into tsibble
                    julian = yday(date_local))
# make ts
full_tmax_ts_gaps <- takapourewa_sounds_full_temps_df %>%
          select(date, takapourewa_tmax_c, brothers_tmax_c, nelson_tmax_c) %>% # don't select Crail 
          rename(
                    c('takapourewa' = takapourewa_tmax_c,
                      'brothers' = brothers_tmax_c,
                      'nelson_aero' = nelson_tmax_c)) %>%
          # pivot longer 
          # from 11982 rows to 35946 rows
          pivot_longer(cols = c('takapourewa',
                                'brothers',
                                'nelson_aero'),
                       names_to = 'station',
                       values_to = 'tmax_c') %>% 
          as_tsibble(index = date, key = station) %>%   
          group_by_key()

full_tmax_ts_gaps %>% autoplot

full_tmax_ts_gaps %>% has_gaps() # this "should" have gaps
full_tmax_ts_gaps %>% duplicated() %>% table() # FALSE 35946, nice!
full_tmax_ts_gaps %>% scan_gaps() %>% view # gaps
full_tmax_ts_gaps %>% is.na() %>% table() # 5373 NAs

# fill gaps
filled_tmax <- full_tmax_ts_gaps %>%
          fill_gaps(tmax_c = NA)

# impute TS - Brother Island Station 
# start of data at 1997-02-15
filled_brothers <- filled_tmax %>%
          filter(station == 'brothers') %>% 
          # filter years before (1997-02-15)
          filter_index("1997-02-15" ~ .) %>% 
          na_seadec(find_frequency = TRUE, algorithm = "ma") %>% # moving average seasonal decomp
          as_tsibble(index = date, key = station) 

filled_brothers %>% is.na() %>% table() 
filled_brothers %>% has_gaps() # # should be FALSE at this point
# filled_brothers %>% scan_gaps() %>% view

# impute TS - Nelson airport Station 
filled_nelson <- filled_tmax %>%
          filter(station == 'nelson_aero') %>% 
          na_seadec(find_frequency = TRUE, algorithm = "ma") %>% # moving average seasonal decomp
          as_tsibble(index = date, key = station) 
          
filled_nelson %>% is.na() %>% table() 
filled_nelson %>% has_gaps() # should be FALSE at this point
# filled_nelson %>% scan_gaps() %>% view

# impute TS both for 1998 -> present 
neighbors_imputed_tmax_ts_seadec_ma <- filled_tmax %>%
          filter(station != "takapourewa") %>% 
          # filter years after 1998
          filter_index("1998-01-01" ~ .) %>% 
          na_seadec(find_frequency = TRUE, algorithm = "ma") %>% # moving average seasonal decomp
          as_tsibble(index = date, key = station) 

neighbors_imputed_tmax_ts_seadec_ma %>% is.na() %>% table() 
neighbors_imputed_tmax_ts_seadec_ma %>% has_gaps() # should be FALSE

neighbors_imputed_tmax_ts_seadec_ma %>% autoplot()

# find possible outlier (extreme low value) for brothers tmax 
############################################## this does not run as intended 
neighbors_imputed_tmax_ts_seadec_ma %>%
          group_by_key() %>% 
          index_by(date) %>%
          summarize(
                    min = min(tmax_c))

# degree day models 
neighbours_dd_models <- neighbors_imputed_tmax_ts_seadec_ma %>%
          as_tibble() %>% 
          mutate(julian = yday(date),
                 year = year(date),
                 month = month(date)) %>% 
          #filter(julian < 305) %>% # Nov 1st ~~305
          mutate(overthresh = case_when(tmax_c > 10 ~ (tmax_c - 10), TRUE ~ 0)) %>% #
          mutate(cumsum_degree_day = as.numeric(unlist(tapply(overthresh, year, cumsum)))) 

# should be equal and represent 2*365 or 2*366 for leap years
table(neighbours_dd_models$year)

# plot degree day models
neighbours_dd_models %>%
          ggplot() +
          geom_line(aes(
                    x = julian,
                    y = cumsum_degree_day,
                    color = year,
                    group = year)) +
          geom_text(data = dd_df %>% filter(julian == last(julian)), aes(
                    label = year,
                    x = julian + 4,
                    y = cumsum_degree_day + 1,
                    color = year)) +
          scale_colour_gradient(name = "year",
                                low = "blue",
                                high = "red") +
          labs(
                    x = "julian day",
                    y = "degree day (tmax_c)",
                    color = "year",
                    title = "Takapourewa degree day (tmax)") + # scale_color_gradient2 not needed with facet_wrap (in that case use color = factor(year) and pick better colour ramp
          guides(color = "none") +
          theme_minimal()



