# clim

##get data 
- 'Takapourewa_degree_days.R' split on March 7th. 

--> 'phd_takapourewa_get_temp_data.R': retrieves clifro daily temp data (1990- present) for Takapourewa (old & new weather station) and writes CSV file: 
data--> "phd_takapourewa_clifro_temps_1990_present.csv" 

--> 'phd_get_sounds_temps_clifro.R': retrives clfiro daily temp data (1990 - present) for adjacent weather stations within ~100km (Crail Bay, North Brother, Nelson Airport) and writes CSV file: 
data--> "phd_sound_clifro_temps.csv" # data from other sounds stations

## explore data
--> 'phd_takapourewa_explore_temp_data.R': data exploration 
************* Possible errors on 2003-07-07 & 1996-05-26 (t_min outliers of -10C) <- will need to cross-references with nearby stations 

## gap filling data
--> 'Takapourewa_OtherSoundsStations_gap_lineplot.R': Makes lineplot of Takapourewa & other Sounds Weather stations to highlight gap (2010 - 2015) in Takapourewa data. 
--> 'Sounds_stations_gaps': explore gaps in temp data  

## Error testing TSimpute - comparing RMSE of gap filling algorithms 
--> gap_filling_error_testing_tmax
--> gap_filling_error_testing_tmin

## degree days with nelson & brothers 
--> neighbors_dd_models.R: degree day models for Nelson & Brothers

###################################################################
clifro: 
Seers B and Shears N (2015). “New Zealand's Climate Data in R - An Introduction to clifro.” The University of Auckland, Auckland, New
Zealand. <URL: https://stattech.wordpress.fos.auckland.ac.nz/2015/03/25/2015-02-new-zealands-climate-data-in-r-an-introduction-to-clifro/>.
