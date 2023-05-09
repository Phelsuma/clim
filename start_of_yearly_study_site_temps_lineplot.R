# This doesnt run yet but goal is to have annual temps as ribbon (t_min, t_max) & t_mean
# highlight years in study 


# this is not the correct data - update with Takapourewa mean daily temps 
# split by annual years
# update as 2023-2025 become available 

library(tidyverse)
library(lubridate)

# read in datat
takapourewa_annual_temps <- read_csv("data/phd_takapourewa_clifro_temps_1990_present.csv")

# pretend this is daily t_mean rather than tmax

study <- c(2022, 2023, 2024) # highlight years in study area among all years

takapourewa_annual_temps %>%
          dplyr::select(date_local, tmax_c) %>% 
          mutate(year = year(date_local)) %>%
          group_by(year) 
          
ggplot() +
          geom_line(aes(x = date_local, y = tmax_c, group = factor(year)))
ggplot(
          takapourewa_annual_temps,
          aes(
                    group = as.factor(year),
                    color = as.factor(study),
                    fill = as.factor(study)
          )
) +
          geom_line(aes(x = date_local, y = t_mean), size = 1, alpha = 0.6) +
          geom_ribbon(aes(x = date_local, ymin = t_min, ymax = t_max), alpha = 0.1)




