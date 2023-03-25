library(tidyverse)
library(lubridate)
library(imputeTS)
library(purrr)
library(tsibble)
library(feasts)

# T_MAX
# full_tmax_ts_gaps: grouped tsibble for all takapourewa, brothers, nelson tmax with GAPS

# T_MIn
# full_tmin_ts_gaps: grouped tsibble for all takapourewa, brothers, nelson tmin with GAPS

# Read in Sounds Weather Stations data
sounds_temp_join <- read_csv(file = "phd_sound_clifro_temps.csv", col_names = TRUE, na = "NA")

# Read in Takapourewa Station data 
takapourewa <- read_csv(file = "phd_takapourewa_clifro_temps_1990_present.csv", col_names = TRUE, na = "NA") %>%  
          rename(c('takapourewa_tmax_c' = tmax_c, 'takapourewa_tmin_c' = tmin_c))

# join Takapourewa with other Sounds weather stations 
takapourewa_sounds_full_temps_df <- left_join(sounds_temp_join, takapourewa, by = 'date_local') %>%
          mutate(
                    #date = lubridate::ymd_hms(date_local)) # # # tsibble does not like this
                    date = as.Date(date_local), # # # only likes being in class "Date" to coerece into tsibble
                    julian = yday(date_local))
                    #day = day(date_local),
                    #weekday = wday(date_local, label = TRUE), # label = true for day of week being class(ord) rather than numeric 
                    #month = month(date_local), # label = TRUE?
                    #year = year(date_local),
                    #austrl_season = 
                              #as.factor(case_when(month %in% 3:5 ~ "Autumn",
                              #                    month %in% 6:8 ~ "Winter",
                              #                    month %in% 9:11 ~"Spring",
                              #                    TRUE ~ "Summer")))

# subset gap     
#takapourewa_sounds_full_temps_df_subset <- takapourewa_sounds_full_temps_df %>%
          #filter(between(date, as.Date('2004-01-01'), as.Date('2014-01-01')))

# check dims
dim(takapourewa_sounds_full_temps_df) # [1] 11982    11; after removing dups from Takapourewa
# dim(takapourewa_sounds_full_temps_df_subset)

# plots
# plot tmax
sounds_tmax_plot <- takapourewa_sounds_full_temps_df %>% 
          ggplot()+
          geom_line(aes(x = date, y = nelson_tmax_c, color = "Nelson Airport"))+
          geom_line(aes(x = date, y = crail_tmax_c, color = "Crail Bay"), alpha = 0.25)+
          geom_line(aes(x = date, y = brothers_tmax_c, color = "N. Brother"), alpha = 0.25) +
          geom_line(aes(x = date, y = takapourewa_tmax_c, color = "Takapourewa"))+
          geom_vline(aes(xintercept = as.Date('2006-01-01')), color = "black", linetype = "solid")+
          geom_vline(aes(xintercept = as.Date('2011-02-01')), color = "black", linetype = "solid")+
          geom_vline(aes(xintercept = as.Date('2012-03-01')), color = "black", linetype = "dashed")+
          geom_vline(aes(xintercept = as.Date('2012-05-01')), color = "black", linetype = "dashed")+
          labs(y = 'degrees C', title = 't_max', x = NULL)+
          scale_color_manual(name='Stations',
                             breaks=c("Nelson Airport", "Crail Bay", "N. Brother", "Takapourewa"),
                             values=c("Nelson Airport"='grey', 
                                      "Crail Bay"='blue', 
                                      "N. Brother"='purple', 
                                      "Takapourewa" = 'red'))
# plot tmin
sounds_tmin_plot <- takapourewa_sounds_full_temps_df %>% 
          ggplot()+
          geom_line(aes(x = date, y = nelson_tmin_c, color = "Nelson Airport"))+
          geom_line(aes(x = date, y = crail_tmin_c, color = "Crail Bay"), alpha = 0.25)+
          geom_line(aes(x = date, y = brothers_tmin_c, color = "N. Brother"), alpha = 0.25) +
          geom_line(aes(x = date, y = takapourewa_tmin_c, color = "Takapourewa"))+
          geom_vline(aes(xintercept = as.Date('2006-01-01')), color = "black", linetype = "solid")+
          geom_vline(aes(xintercept = as.Date('2011-01-01')), color = "black", linetype = "solid")+
          geom_vline(aes(xintercept = as.Date('2012-03-01')), color = "black", linetype = "dashed")+
          geom_vline(aes(xintercept = as.Date('2012-05-01')), color = "black", linetype = "dashed")+
          labs(x = 'date', y = 'degrees C', title = 't_min')+
          scale_color_manual(name='Stations',
                             breaks=c("Nelson Airport", "Crail Bay", "N. Brother", "Takapourewa"),
                             values=c("Nelson Airport"='grey', 
                                      "Crail Bay"='blue', 
                                      "N. Brother"='purple', 
                                      "Takapourewa" = 'red'))
# combine
sounds_temps_lineplot <- cowplot::plot_grid(sounds_tmax_plot, sounds_tmin_plot, ncol = 1)

#############################################################################################
# Explore gaps in data # 

# object with imputeTS gap plots for each station (n = 4) and metric (n = 2)
Sounds_gap_plots <- takapourewa_sounds_full_temps_df %>% 
          select(takapourewa_tmax_c, takapourewa_tmin_c, brothers_tmax_c, brothers_tmin_c, crail_tmax_c, crail_tmin_c, nelson_tmax_c, nelson_tmin_c) %>%
          as_tibble() %>% 
          map(~ imputeTS::ggplot_na_distribution(., alpha_missing = 0.01, 
                                                 title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL)) # map or walk?
          # %>% list2env(., envir = NULL) # best way to save output?

length(Sounds_ts_gaps_impute) # 8 

# Create gridplot of items in list from imputeTS
Sounds_gaps_grid_plot <- cowplot::plot_grid(
          Sounds_gap_plots[[1]],Sounds_gap_plots[[2]], # Takapourewa
          Sounds_gap_plots[[3]],Sounds_gap_plots[[4]], # Brothers
          Sounds_gap_plots[[5]],Sounds_gap_plots[[6]], # Crail
          Sounds_gap_plots[[7]],Sounds_gap_plots[[8]], # Nelson Aero
          ncol = 2)

################################################################################
##### find longest contiguous non-NA
# tmax # 296 rows (2017/2018)
longest_contingous_complete_case_tmax_df <- takapourewa_sounds_full_temps_df %>% 
          select(!c(crail_tmax_c, crail_tmin_c, # remove crail bay
                   takapourewa_tmin_c, nelson_tmin_c, brothers_tmin_c)) %>% # remove all t_mins 
          na.contiguous() 
dim(longest_contingous_complete_case_tmax_df)

write_csv(longest_contingous_complete_case_tmax_df, "longest_contingous_complete_case_tmax_df.csv") 

longest_contingous_complete_case_tmax_df %>%
          ggplot()+
          geom_line(aes(x = date, y = takapourewa_tmax_c, color = "Takapourewa"))+
          geom_line(aes(x = date, y = brothers_tmax_c, color = "N. Brother"), alpha = 0.5)+
          geom_line(aes(x = date, y = nelson_tmax_c, color = "Nelson Airport"), alpha = 0.5)+
          labs(x = "date", y = "degrees C", title = "t max")+
          scale_color_manual(name='Stations',
                             breaks=c("Takapourewa", "N. Brother", "Nelson Airport"),
                             values=c("Takapourewa" = 'red',
                                      "N. Brother"='purple', 
                                      "Nelson Airport"='blue'))
# corr
longest_contingous_complete_case_tmax_df %>% 
          select(takapourewa_tmax_c, brothers_tmax_c, nelson_tmax_c) %>% ## select only stations 
          cor() # default = pearson
# ~89% with N Brother & ~88% with Nelson Airport

# tmax summary statistics 
longest_contingous_complete_case_tmax_df %>% # tkpw tmax ss 
          select(takapourewa_tmax_c) %>% 
          summarize(min = min(takapourewa_tmax_c),
                    max = max(takapourewa_tmax_c), 
                    mean = mean(takapourewa_tmax_c), 
                    median = median(takapourewa_tmax_c),
                    sd = sd(takapourewa_tmax_c)) 

longest_contingous_complete_case_tmax_df %>% # tkpw tmax ss 
          select(brothers_tmax_c) %>% 
          summarize(min = min(brothers_tmax_c),
                    max = max(brothers_tmax_c), 
                    mean = mean(brothers_tmax_c), 
                    median = median(brothers_tmax_c),
                    sd = sd(brothers_tmax_c)) 

longest_contingous_complete_case_tmax_df %>% # nelson tmax ss 
          select(nelson_tmax_c) %>% 
          summarize(min = min(nelson_tmax_c),
                    max = max(nelson_tmax_c), 
                    mean = mean(nelson_tmax_c), 
                    median = median(nelson_tmax_c),
                    sd = sd(nelson_tmax_c)) 

longest_contingous_complete_case_tmax_df %>% 
          select(takapourewa_tmax_c, brothers_tmax_c) %>% 
          mutate(difference = (brothers_tmax_c - takapourewa_tmax_c)) %>%
          summarize(min = min(difference),
                    max = max(difference), 
                    mean = mean(difference), 
                    median = median(difference),
                    sd = sd(difference)) 

longest_contingous_complete_case_tmax_df %>% 
          select(takapourewa_tmax_c, nelson_tmax_c) %>% 
          mutate(difference = (nelson_tmax_c - takapourewa_tmax_c)) %>%
          summarize(min = min(difference),
                    max = max(difference), 
                    mean = mean(difference), 
                    median = median(difference),
                    sd = sd(difference)) 

#####################################
###### tmin #### 378 rows (2018/2019)
longest_contingous_complete_case_tmin_df <- takapourewa_sounds_full_temps_df %>% 
          select(!c(crail_tmax_c, crail_tmin_c, # remove crail bay
                   takapourewa_tmax_c, nelson_tmax_c, brothers_tmax_c)) %>% # remove all t_max 
          na.contiguous() 
dim(longest_contingous_complete_case_tmin_df)

write_csv(longest_contingous_complete_case_tmin_df, "longest_contingous_complete_case_tmin_df.csv") 

longest_contingous_complete_case_tmin_df %>%
          ggplot()+
          geom_line(aes(x = date, y = takapourewa_tmin_c, color = "Takapourewa"))+
          geom_line(aes(x = date, y = brothers_tmin_c, color = "N. Brother"), alpha = 0.5)+
          geom_line(aes(x = date, y = nelson_tmin_c, color = "Nelson Airport"), alpha = 0.5)+
          labs(x = "date", y = "degrees C", title = "t min")+
          scale_color_manual(name='Stations',
                             breaks=c("Takapourewa", "N. Brother", "Nelson Airport"),
                             values=c("Takapourewa" = 'red',
                                      "N. Brother"='purple', 
                                      "Nelson Airport"='blue'))

# tile viz (move to another file)
# 378 rows (2018/2019)
longest_contingous_complete_case_tmin_df %>% 
          ggplot(aes(x = date, y = 1))+
          geom_tile(aes(fill = takapourewa_tmin_c))+
          scale_fill_gradient2(
                    low = "blue", mid = "purple", high = "red", 
                    midpoint = median(longest_contingous_complete_case_tmin_df$takapourewa_tmin_c))+
          scale_y_discrete(expand = c(0,0))+
          labs(y = NULL, x = "month (2018/2019")

# correlations 
longest_contingous_complete_case_tmin_df %>% 
          select(takapourewa_tmin_c, brothers_tmin_c, nelson_tmin_c) %>% ## select only stations 
          cor() # default = pearson
# ~86% with N. Brother & ~75% with Nelson Airport 

# tmin summary statistics 
longest_contingous_complete_case_tmin_df %>% # takapourewa tmin ss 
          select(takapourewa_tmin_c) %>% 
          summarize(min = min(takapourewa_tmin_c),
                    max = max(takapourewa_tmin_c), 
                    mean = mean(takapourewa_tmin_c), 
                    median = median(takapourewa_tmin_c),
                    sd = sd(takapourewa_tmin_c)) 

longest_contingous_complete_case_tmin_df %>% # brothers tmin ss 
          select(brothers_tmin_c) %>% 
          summarize(min = min(brothers_tmin_c),
                    max = max(brothers_tmin_c), 
                    mean = mean(brothers_tmin_c), 
                    median = median(brothers_tmin_c),
                    sd = sd(brothers_tmin_c)) 

longest_contingous_complete_case_tmin_df %>% # nelson tmin ss 
          select(nelson_tmin_c) %>% 
          summarize(min = min(nelson_tmin_c),
                    max = max(nelson_tmin_c), 
                    mean = mean(nelson_tmin_c), 
                    median = median(nelson_tmin_c),
                    sd = sd(nelson_tmin_c)) 

# difference SS 
longest_contingous_complete_case_tmin_df %>% # difference between brothers & tkpwa tmin ss 
          select(takapourewa_tmin_c, brothers_tmin_c) %>% 
          mutate(difference = (brothers_tmin_c - takapourewa_tmin_c)) %>%
          summarize(min = min(difference),
                    max = max(difference), 
                    mean = mean(difference), 
                    median = median(difference),
                    sd = sd(difference)) 

longest_contingous_complete_case_tmin_df %>% # difference between nelson & tkpwa tmin ss 
          select(takapourewa_tmin_c, nelson_tmin_c) %>% 
          mutate(difference = (nelson_tmin_c - takapourewa_tmin_c)) %>%
          summarize(min = min(difference),
                    max = max(difference), 
                    mean = mean(difference), 
                    median = median(difference),
                    sd = sd(difference)) 
# # # # make tsibbles 
# https://tsibble.tidyverts.org/articles/intro-tsibble.html

# make tmax tsibble takapourewa
# takapourewa_tmax_ts <- longest_contingous_complete_case_tmax_df %>% 
#           select(date, takapourewa_tmax_c) %>% # # # only likes being in class "Date" to coerece into tsibble
#           tsibble::as_tsibble(index = date)
# 
# # ok this worked
# longest_contingous_complete_case_tmax_df %>% 
#           select(date, takapourewa_tmax_c, brothers_tmax_c, nelson_tmax_c) %>% 
#           tsibble::as_tsibble(index = date)

# pivot longer
tmax_ts_df <- longest_contingous_complete_case_tmax_df %>%
          select(date, takapourewa_tmax_c, brothers_tmax_c, nelson_tmax_c) %>%
          rename(
                    c('takapourewa' = takapourewa_tmax_c,
                    'brothers' = brothers_tmax_c,
                    'nelson_aero' = nelson_tmax_c)) %>%
          # pivot longer
          pivot_longer(cols = c('takapourewa',
                                'brothers',
                                'nelson_aero'),
                    names_to = 'station',
                    values_to = 'tmax_c') %>% 
          as_tsibble(index = date, key = station) %>%   
          group_by_key() # grouped by station (takes it out of of sequential order by date)

tmax_ts_df %>% 
          ggplot()+
          geom_boxplot(aes(y = tmax_c, x = station))

# test %>% group_by(date) %>% count() %>% filter(n == 3) %>% nrow() # ok this is fine - no dups ? 

tmax_ts_df %>% has_gaps() # yay no gaps (as we'd hope!) 
tmax_ts_df %>% feasts::autoplot() # only works with the complete_cases

# gap data
duplicated(takapourewa_sounds_full_temps_df, index = date) %>% table() #None formerly 5 duplicated
are_duplicated(takapourewa_sounds_full_temps_df, index = date) %>% table() #None fomerly 100 duplicated whattttttt

# tmax with gaps
full_tmax_ts_gaps <- takapourewa_sounds_full_temps_df %>%
          select(date, takapourewa_tmax_c, brothers_tmax_c, nelson_tmax_c) %>%
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

full_tmax_ts_gaps %>% has_gaps() # this "should" have gaps
full_tmax_ts_gaps %>% duplicated() %>% table() # FALSE 35946, nice!
full_tmax_ts_gaps %>% scan_gaps() %>% view # gaps
full_tmax_ts_gaps %>% is.na() %>% table() # 5373 NAs

# tmin with gaps
full_tmin_ts_gaps <- takapourewa_sounds_full_temps_df %>%
          select(date, takapourewa_tmin_c, brothers_tmin_c, nelson_tmin_c) %>%
          rename(
                    c('takapourewa' = takapourewa_tmin_c,
                      'brothers' = brothers_tmin_c,
                      'nelson_aero' = nelson_tmin_c)) %>%
          # pivot longer
          # from 11982 rows to 35946 rows
          pivot_longer(cols = c('takapourewa',
                                'brothers',
                                'nelson_aero'),
                       names_to = 'station',
                       values_to = 'tmin_c') %>% 
          as_tsibble(index = date, key = station) %>%   
          group_by_key()

full_tmin_ts_gaps %>% duplicated() %>% table() # FALSE 35946, nice!
full_tmin_ts_gaps %>% has_gaps() # supposed to be true for all groups
full_tmin_ts_gaps %>% scan_gaps() %>% view 
full_tmin_ts_gaps %>% is.na() %>% table() # 5572

# pivot longer Tmin (678 rows)
tmin_ts_df <- longest_contingous_complete_case_tmin_df %>%
          select(date, takapourewa_tmin_c, brothers_tmin_c, nelson_tmin_c) %>%
          rename(
                    c('takapourewa' = takapourewa_tmin_c,
                      'brothers' = brothers_tmin_c,
                      'nelson_aero' = nelson_tmin_c)) %>%
          # pivot longer
          pivot_longer(cols = c('takapourewa',
                                'brothers',
                                'nelson_aero'),
                       names_to = 'station',
                       values_to = 'tmin_c') %>% 
          as_tsibble(index = date, key = station) %>%   
          group_by_key() # grouped by station (takes it out of of sequential order by date)

tmin_ts_df %>% 
          ggplot()+
          geom_boxplot(aes(y = tmin_c, x = station))

######################## ACF & PACF with no gap data
# # tmax
#takapourewa
tmax_ts_df %>%
          filter(station == "takapourewa") %>%
          ACF(tmax_c) %>%
          autoplot()
tmax_ts_df %>% 
          filter(station == "takapourewa") %>%
          PACF(tmax_c) %>%
          autoplot()

# # tmin
#takapourewa
tmin_ts_df %>%
          filter(station == "takapourewa") %>%
          ACF(tmin_c) %>%
          autoplot()
tmin_ts_df %>% 
          filter(station == "takapourewa") %>%
          PACF(tmin_c) %>%
          autoplot()


####### decomp with no gaps? 
# decompose() https://feasts.tidyverts.org/reference/index.html


# linear model?
# ML? RF?


