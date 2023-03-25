# need to deal with NAs 

library(tidyverse)
library(lubridate)

dly_temp_tbbl <- read_csv("phd_takapourewa_clifro_temps_1990_present.csv")

skimr::skim(dly_temp_tbbl)

# add date info via lubridate
# & make seasons
dates_tmp <- dly_temp_tbbl %>%
          mutate(
                    date = as.Date(date_local),
                    julian = yday(date_local),
                    day = day(date_local),
                    weekday = wday(date_local, label = TRUE), # label = true for day of week being class(ord) rather than numeric 
                    month = month(date_local), # label = TRUE?
                    year = year(date_local),
                    austrl_season = 
                              as.factor(case_when(month %in% 3:5 ~ "Autumn",
                                                  month %in% 6:8 ~ "Winter",
                                                  month %in% 9:11 ~"Spring",
                                                  TRUE ~ "Summer")))

# dates with NAs by year 
dates_tmp %>%
          filter(is.na(tmax_c)) %>% 
          group_by(year) %>% 
          count() %>% 
          View()

dates_tmp %>%
          filter(is.na(tmin_c)) %>% 
          group_by(year) %>% 
          count()

# dates below threshold
dates_tmp %>% filter(tmax_c < 10) # 10 is approximately development temp

# Vizualize Data 

# histogram of max temps by year & season
dates_tmp %>% # warning for NAs
          ggplot()+
          geom_histogram(aes(x = tmin_c, fill = austrl_season), binwidth = 1)+
          facet_wrap(~year)+
          geom_vline(xintercept = 30, linetype = "dashed", color = "red")+
          geom_vline(xintercept = 22, linetype = "dashed", color = "black")+
          geom_vline(xintercept = 10, linetype = "dashed", color = "blue")
          
# 2006 - 2009 data gaps same for t_min! 
problem_years <- c(2006, 2007, 2008, 2009)
dates_tmp %>%
          filter(year %in% problem_years) %>% 
          select(tmax_c, year) %>% 
          group_by(year) %>% 
          count() 
# == 28, same as number of rows available on CLIFLO
# Total number of rows output = 28 
# tf, Clifro == Cliflo :'(

######################################################
# need to find temp data for 2006:2009
# after gap is when accuracy goes from 1C to 0.1C
######################################################

# grouped boxplots
dates_tmp %>% # warning for NAs
          filter(!year %in% problem_years) %>% droplevels %>% # filter incomplete years
          select(year, tmax_c, tmin_c) %>%
          mutate(year = factor(year)) %>%
          pivot_longer(cols = -year) %>%
          ggplot()+
          geom_boxplot(aes(x = year, y = value, fill = name), alpha = .75)+
          geom_hline(yintercept = -5, linetype = "dashed", color = 'blue')+ # denotes arbitrary line below which I am skeptical of being erroneous
          scale_fill_manual(values = c('#ff4d58', '#4d4dff'))+
          scale_y_continuous(limits = c(-10, 30), breaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30))+
          labs(y = "degrees C", x = "year", fill = "metric")

# outliers of -10: 2003-07-07 & 1996-05-26
# Errors? check with other stations nearby before removing? 
dates_tmp %>%
          select(date, tmin_c) %>% 
          slice_min(order_by = tmin_c, n = 5)

#line plot t_min & t_max by 
# this needs aesthetics work but ok for data exploration 
takapourewa_tmin_tmax_years <- dates_tmp %>% 
          ggplot()+
          geom_line(aes(x = julian, y = tmax_c, color = "Tmax"))+
          geom_line(aes(x = julian, y = tmin_c, color = "Tmin"))+
          facet_wrap(~year)+ # lines connect 
          geom_hline(yintercept = 22, linetype = "dotted")+
          labs(x = "julian day", y = "degrees C", fill = "temp", title = "daily air temps at 9am on Takapourewa")+
          scale_color_manual(name = "Degrees C", values = c("Tmax" = '#ff4d58', "Tmin" = '#4d4dff'))
          
ggsave("takapourewa_tmin_tmax_years.png")
          dev.off()

###############----------------------------- where you left off: March 7th 2023 12:39
          
          # creates plot for comparing time series of Takapourewa vs other Sounds Weather Stations to identify gap
          
          # Read in Sounds Weather Stations data
          sounds_temp_join <- read_csv(file = "phd_sound_clifro_temps.csv", col_names = TRUE, na = "NA")
          
          # Read in Takapourewa Station data 
          takapourewa <- read_csv(file = "phd_takapourewa_clifro_temps_1990_present.csv", col_names = TRUE, na = "NA") %>%  
                    rename(c('takapourewa_tmax_c' = tmax_c, 'takapourewa_tmin_c' = tmin_c))
          
          # join Takapourewa with other Sounds weather stations 
          takapourewa_sounds_full_temps_df <- left_join(sounds_temp_join, takapourewa, by = 'date_local') %>% 
                    mutate(date = as.Date(date_local))
          
          # subset gap     
          takapourewa_sounds_full_temps_df_subset <- takapourewa_sounds_full_temps_df %>%
                    filter(between(date, as.Date('2004-01-01'), as.Date('2014-01-01')))
          
          # check dims
          dim(takapourewa_sounds_full_temps_df)
          dim(takapourewa_sounds_full_temps_df_subset)
          
          
          # plots
          # plot tmax
          sounds_tmax_plot <- takapourewa_sounds_full_temps_df_subset %>% 
                    ggplot()+
                    geom_line(aes(x = date, y = nelson_tmax_c, color = "Nelson Airport"))+
                    geom_line(aes(x = date, y = crail_tmax_c, color = "Crail Bay"), alpha = 0.25)+
                    geom_line(aes(x = date, y = brothers_tmax_c, color = "N. Brother"), alpha = 0.25) +
                    geom_line(aes(x = date, y = takapourewa_tmax_c, color = "Takapourewa"))+
                    geom_vline(aes(xintercept = as.Date('2006-01-01')), color = "red", linetype = "solid")+
                    geom_vline(aes(xintercept = as.Date('2011-02-01')), color = "red", linetype = "solid")+
                    geom_vline(aes(xintercept = as.Date('2012-03-01')), color = "red", linetype = "dashed")+
                    geom_vline(aes(xintercept = as.Date('2012-05-01')), color = "red", linetype = "dashed")+
                    labs(y = 'degrees C', title = 't_max', x = NULL)+
                    scale_color_manual(name='Weather Stations',
                                       breaks=c("Nelson Airport", "Crail Bay", "N. Brother", "Takapourewa"),
                                       values=c("Nelson Airport"='grey', 
                                                "Crail Bay"='blue', 
                                                "N. Brother"='purple', 
                                                "Takapourewa" = 'black'))
          # plot tmin
          sounds_tmin_plot <- takapourewa_sounds_full_temps_df_subset %>% 
                    ggplot()+
                    geom_line(aes(x = date, y = nelson_tmin_c, color = "Nelson Airport"))+
                    geom_line(aes(x = date, y = crail_tmin_c, color = "Crail Bay"), alpha = 0.25)+
                    geom_line(aes(x = date, y = brothers_tmin_c, color = "N. Brother"), alpha = 0.25) +
                    geom_line(aes(x = date, y = takapourewa_tmin_c, color = "Takapourewa"))+
                    geom_vline(aes(xintercept = as.Date('2006-01-01')), color = "red", linetype = "solid")+
                    geom_vline(aes(xintercept = as.Date('2011-01-01')), color = "red", linetype = "solid")+
                    geom_vline(aes(xintercept = as.Date('2012-03-01')), color = "red", linetype = "dashed")+
                    geom_vline(aes(xintercept = as.Date('2012-05-01')), color = "red", linetype = "dashed")+
                    labs(x = 'date', y = 'degrees C', title = 't_min')+
                    scale_color_manual(name='Weather Stations',
                                       breaks=c("Nelson Airport", "Crail Bay", "N. Brother", "Takapourewa"),
                                       values=c("Nelson Airport"='grey', 
                                                "Crail Bay"='blue', 
                                                "N. Brother"='purple', 
                                                "Takapourewa" = 'black'))
          # combine
          sounds_temps_lineplot <- cowplot::plot_grid(sounds_tmax_plot, sounds_tmin_plot, ncol = 1)
          
          
          
          
          
          