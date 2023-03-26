####### try including winter in DD

# fill gaps in Time Series 
library(tidyverse)
library(magrittr)
library(lubridate)
library(imputeTS)
library(purrr)
library(tsibble)
library(feasts)

citation("tidyverse")
citation("magrittr")
citation("lubridate")
citation("imputeTS")
citation("purrr")
citation("tsibble")
citation("feasts")

# read in takapourewa data
neighbors_df  <- readr::read_csv("data/phd_sound_clifro_temps.csv")
# read in neighbor station data
takapourewa_df <- read_csv("data/phd_takapourewa_clifro_temps_1990_present.csv") %>%
          rename(c('takapourewa_tmax_c' = tmax_c,
                   'takapourewa_tmin_c' = tmin_c))

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
# splitting by station because issues otherwise 

# brothers 
brothers_dd_models <- neighbors_imputed_tmax_ts_seadec_ma %>%
          as_tibble() %>% 
          filter(station == 'brothers') %>% 
          mutate(julian = yday(date),
                 year = year(date),
                 month = month(date)) %>% 
          filter(month %in% c('9', '10')) %>% #pre-nesting (sep 1st - nov 1st) per Nelson et al 2018
          mutate(overthresh = case_when(tmax_c > 10 ~ (tmax_c - 10), TRUE ~ 0)) %>% #
          mutate(cumsum_degree_day = as.numeric(unlist(tapply(overthresh, year, cumsum)))) 

# plot degree day models
mid <- median(brothers_dd_models$year) # aesthetics below

brothers_dd_models %>%
          ggplot()+
          geom_line(aes(x = julian,
                        y = cumsum_degree_day,
                        group = year, 
                        color = year))+
          labs(x = "date (julian day)",
               y = "degree day (tmax_c)",
               title = "N Brother degree day (tmax >10c)") + # scale_color_gradient2 not needed with facet_wrap (in that case use color = factor(year) and pick better colour ramp +
           geom_text(data = brothers_dd_models %>% 
                     filter(julian == last(julian)), 
                               aes(label = year,
                                   x = julian + 4,
                                   y = cumsum_degree_day + 1),
                     check_overlap = TRUE)+
          scale_color_gradient2(midpoint = mid, low = "blue", mid = "purple", high = "red")+
          #guides(color = "none")+
          theme_minimal() 

# nelson DD 
nelson_dd_models <- neighbors_imputed_tmax_ts_seadec_ma %>%
          as_tibble() %>% 
          filter(station == 'nelson_aero') %>% 
          mutate(julian = yday(date),
                 year = year(date),
                 month = month(date)) %>% 
          filter(month %in% c('9', '10')) %>% #pre-nesting (sep 1st - nov 1st) per Nelson et al 2018
          mutate(overthresh = case_when(tmax_c > 10 ~ (tmax_c - 10), TRUE ~ 0)) %>% #
          mutate(cumsum_degree_day = as.numeric(unlist(tapply(overthresh, year, cumsum)))) 

nelson_dd_models %>%
          ggplot()+
          geom_line(aes(x = julian,
                        y = cumsum_degree_day,
                        group = year, 
                        color = year))+
          labs(x = "date (julian day)",
               y = "degree day (tmax_c)",
               title = "Nelson degree day (tmax >10c)") + # scale_color_gradient2 not needed with facet_wrap (in that case use color = factor(year) and pick better colour ramp +
          geom_text(data = nelson_dd_models %>% 
                              filter(julian == last(julian)), 
                    aes(label = year,
                        x = julian + 4,
                        y = cumsum_degree_day + 1),
                    check_overlap = TRUE)+
          scale_color_gradient2(midpoint = mid, low = "blue", mid = "purple", high = "red")+
          #guides(color = "none")+
          theme_minimal() 

#### try again with both stations
neighbors_dd <- rbind(nelson_dd_models, brothers_dd_models) 

# geom_col for brothers & nelsons DD 
neighbors_dd %>% 
          select(year, station, cumsum_degree_day) %>% 
          group_by(station, year) %>% 
          summarise(dd = max(cumsum_degree_day)) %>% 
          arrange(year, dd) %>% 
          ggplot()+
          geom_col(aes(x = factor(year), y = dd, fill = station), width = 0.5, position = "dodge")+
          labs(x = "year",
               y = "degree day (tmax_c)",
               title = "degree day (tmax >10c)") + 
          theme_minimal() 

dd_station_year_summary <- neighbors_dd %>% 
          select(year, station, cumsum_degree_day) %>% 
          group_by(station, year) %>% 
          summarise(dd = max(cumsum_degree_day)) %>% 
          pivot_wider(names_from = c(station), values_from = dd) 

# degree days by station facet_wrap ~year
neighbors_dd %>% ggplot()+
          geom_line(aes(x = julian,
                        y = cumsum_degree_day,
                        group = station, 
                        color = station))+
          facet_wrap(~year)+
          labs(x = "date (julian day)",
               y = "degree day (tmax_c)",
               title = "degree day (tmax >10c)") + 
          #guides(color = "none")+
          theme_minimal() 

# year ranks
# brothers
dd_station_year_summary %>% 
          select(year, brothers) %>% 
          arrange(desc(brothers)) %>%  # arrange by hottest 
          mutate(highest_dd_year_rank_brothers = 1:nrow(.)) %>% 
          arrange((highest_dd_year_rank_brothers))  # arrange by year

# nelson
dd_station_year_summary %>% 
          select(year, nelson_aero) %>% 
          arrange(desc(nelson_aero)) %>%  # arrange by hottest 
          mutate(highest_dd_year_rank_nelson = 1:nrow(.)) %>% 
          arrange((highest_dd_year_rank_nelson))  # arrange by year

# lines & points for brothers & nelsons DD 
neighbors_dd %>% 
          select(year, station, cumsum_degree_day) %>% 
          group_by(station, year) %>% 
          summarise(dd = max(cumsum_degree_day)) %>% 
          arrange(year, dd) %>% 
          ggplot()+
          geom_point(aes(x = factor(year), y = dd, color = station, size = 5))+
          geom_line(aes(x = factor(year), y = dd, group = station), alpha = 0.5, linetype = "dashed")+
          labs(x = "year",
               y = "degree day (tmax_c)",
               title = "degree day (tmax >10c)")+
          guides(size = 'none')+
          theme_minimal()
          
# vertical lines
neighbors_dd %>% 
          select(year, station, cumsum_degree_day) %>% 
          group_by(station, year) %>% 
          summarise(dd = max(cumsum_degree_day)) %>% 
          ggplot()+
          geom_point(aes(x = factor(station), y = dd, color = year))+
          scale_color_gradient2(midpoint = mid, low = "blue", mid = "purple", high = "red")+
          geom_text_repel(aes(label = year,
                        x = station,
                        y = dd, 
                        color = year, size = 5),
                        max.overlaps = 33)+
          labs(x = "station",
               y = "degree day (tmax_c)",
               title = "degree day (tmax >10c)")+ 
          guides(size = 'none')+
          theme_minimal() 

