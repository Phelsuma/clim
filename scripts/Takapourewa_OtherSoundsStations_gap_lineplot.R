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




