# tmin
# test gap filling methods
# probably should make some functions if keeping
library(imputeTS) # https://cran.rstudio.com/web/packages/imputeTS/vignettes/Cheat_Sheet_imputeTS.pdf
# Moritz, Steffen and Bartz-Beielstein, Thomas.
# "imputeTS: Time Series Missing Value Imputation in R."
# R Journal 9.1 (2017). doi: 10.32614/RJ-2017-009.
library(Metrics)

longest_contingous_complete_case_tmin_df <- read_csv("longest_contingous_complete_case_tmin_df.csv")

# create gaps
set.seed(2023)

# find a way to do this with replacement or without selecting duplicate values 
# ie by individual rows 
takapourewa_tmin <- longest_contingous_complete_case_tmin_df$takapourewa_tmin_c
len_tmin <- 1:length(takapourewa_tmin)  
ind_tmin <- sample(len_tmin, 25) # select 25 rows by random 
takapourewa_tmin[ind_tmin] <- NA # inserts NA into wherever those numbers occur 
takapourewa_tmin_actual <- longest_contingous_complete_case_tmin_df$takapourewa_tmin_c[ind_tmin] # compare to real data

# gap filling
# linear_interpolate
takapourewa_tmin_interpolated_linear <- na_interpolation(takapourewa_tmin, option = "linear") # interpolate
interpolated_values_linear <- takapourewa_tmin_interpolated_linear[ind_tmin] # interpolated values
# spline interpolate 
takapourewa_tmin_interpolated_spline <- na_interpolation(takapourewa_tmin, option = "spline") # interpolate
interpolated_values_spline <- takapourewa_tmin_interpolated_spline[ind_tmin] # interpolated values
# Kalman smoothing
takapourewa_tmin_kalman <- na_kalman(takapourewa_tmin)
takapourewa_tmin_kalman_values <- takapourewa_tmin_kalman[ind_tmin] # kalman values
# Last Observation Carried Forward (na_locf)
takapourewa_tmin_locf <- na_locf(takapourewa_tmin)
takapourewa_tmin_locf_values <- takapourewa_tmin_locf[ind_tmin] # na_locf values
# moving average (na_ma)
takapourewa_tmin_ma <- na_ma(takapourewa_tmin)
takapourewa_tmin_ma_values <- takapourewa_tmin_ma[ind_tmin] # moving average values
# seasonal decomp (na_seadec) interpolation
takapourewa_tmin_seadec_interp <- na_seadec(takapourewa_tmin, find_frequency = TRUE, algorithm = "interpolation") 
takapourewa_tmin_seadec_interp_values <- takapourewa_tmin_seadec_interp[ind_tmin] # seasonal decomposition values
# seasonal decomp (na_seadec) locf
takapourewa_tmin_seadec_locf <- na_seadec(takapourewa_tmin, find_frequency = TRUE, algorithm = "locf") 
takapourewa_tmin_seadec_locf_values <- takapourewa_tmin_seadec_locf[ind_tmin] # seasonal decomposition locf values
# seasonal decomp (na_seadec) kalman
takapourewa_tmin_seadec_kalman <- na_seadec(takapourewa_tmin, find_frequency = TRUE, algorithm = "kalman")
takapourewa_tmin_seadec_kalman_values <- takapourewa_tmin_seadec_kalman[ind_tmin] # seasonal decomposition kalman values
# seasonal decomp (na_seadec) ma
takapourewa_tmin_seadec_ma <- na_seadec(takapourewa_tmin, find_frequency = TRUE, algorithm = "ma")
takapourewa_tmin_seadec_ma_values <- takapourewa_tmin_seadec_ma[ind_tmin] # seasonal decomposition kalman values
# na_seasplit (not worth considering )
# takapourewa_tmin_seasplit <- na_seasplit(takapourewa_tmin, find_frequency = TRUE) # various algoroithms none accurate
# takapourewa_tmin_seasplit_values <- takapourewa_tmin_seasplit[ind_tmin] 

# actual vs predicted - get residuals
test_predictions <- as_tibble(ind_tmin) %>% 
          rename(row_id = 'value') %>% 
          bind_cols(actual = takapourewa_tmin_actual) %>% 
          bind_cols(linear_interpolated = interpolated_values_linear) %>% # linear interpolated 
          mutate('linear_resid' = actual - linear_interpolated) %>% 
          bind_cols(spline_interpolated = interpolated_values_spline) %>% # spine interpolated 
          mutate('spline_resid' = actual - spline_interpolated) %>%
          bind_cols(kalman = takapourewa_tmin_kalman_values) %>% # Kalman Smoothing
          mutate('kalman_resid' = actual - kalman) %>%
          bind_cols(locf = takapourewa_tmin_locf_values) %>% # Last Observation Carried Forward 
          mutate('locf_resid' = actual - locf) %>%
          bind_cols(ma = takapourewa_tmin_ma_values) %>% # moving average
          mutate('ma_resid' = actual - ma) %>%
          bind_cols(seadec_interp = takapourewa_tmin_seadec_interp_values) %>% # seasonal decomp interpolation
          mutate('seadec_interp_resid' = actual - seadec_interp) %>% 
          bind_cols(seadec_locf = takapourewa_tmin_seadec_locf_values) %>% # seasonal decomp locf
          mutate('seadec_locf_resid' = actual - seadec_locf) %>% 
          bind_cols(seadec_kalman = takapourewa_tmin_seadec_kalman_values) %>% # seasonal decomp kalman
          mutate('seadec_kalman_resid' = actual - seadec_kalman) %>% 
          bind_cols(seadec_ma = takapourewa_tmin_seadec_ma_values) %>% # seasonal decomp ma
          mutate('seadec_ma_resid' = actual - seadec_ma)

options(pillar.sigfig = 7) # summarise 
ss_preds_tmin <- test_predictions %>% 
          summarise(
                    mean_actual = mean(actual),
                    mean_interp_linear = mean(linear_interpolated),
                    mean_interp_spline = mean(spline_interpolated), 
                    mean_kalman = mean(kalman),
                    mean_locf = mean(locf),
                    mean_ma = mean(ma), 
                    mean_seadec_interp = mean(seadec_interp),
                    mean_seadec_locf = mean(seadec_locf), 
                    mean_seadec_kalman = mean(seadec_kalman),
                    mean_seadec_ma = mean(seadec_ma)) %>%
          pivot_longer(everything(), names_to = 'NA_fill', values_to = 'means')%>%
          arrange(desc(means)) 

# Calculate root mean square error (RMSE)
sqrt(mean(test_predictions$linear_resid^2)) # 1.821716
sqrt(mean(test_predictions$spline_resid^2)) # 2.024953
# RMSE (same as above)  
ss_RMSE_tmin <- test_predictions %>%
          summarise(
                    linear_interp = Metrics::rmse(test_predictions$actual,test_predictions$linear_interpolated),# 
                    spline_interp = Metrics::rmse(test_predictions$actual,test_predictions$spline_interpolated),# 
                    kalman = Metrics::rmse(test_predictions$actual, test_predictions$kalman),# 1.755726
                    locf = Metrics::rmse(test_predictions$actual, test_predictions$locf),# 1.754537
                    ma = Metrics::rmse(test_predictions$actual, test_predictions$ma),# 
                    seadec_interp = Metrics::rmse(test_predictions$actual, test_predictions$seadec_interp),# 
                    seadec_locf = Metrics::rmse(test_predictions$actual, test_predictions$seadec_locf),# 
                    seadec_kalman = Metrics::rmse(test_predictions$actual, test_predictions$seadec_kalman),# 
                    seadec_ma = Metrics::rmse(test_predictions$actual, test_predictions$seadec_ma)) %>%  # 
          pivot_longer(everything(), names_to = 'NA_fill', values_to = 'RMSE') %>%
          arrange(RMSE) 

# two best = 

# plot interpolated values 
linear_plot <- ggplot_na_imputations(x_with_na = takapourewa_tmin, 
                                     x_with_imputations = takapourewa_tmin_interpolated_linear, 
                                     x_with_truth = longest_contingous_complete_case_tmin_df$takapourewa_tmin_c,
                                     title = NULL,
                                     subtitle = "linear",
                                     legend = FALSE)
spline_plot <- ggplot_na_imputations(x_with_na = takapourewa_tmin, 
                                     x_with_imputations = takapourewa_tmin_interpolated_spline, 
                                     x_with_truth = longest_contingous_complete_case_tmin_df$takapourewa_tmin_c,
                                     title = NULL,
                                     subtitle = "spline")

seadec_kalman_plot <- ggplot_na_imputations(x_with_na = takapourewa_tmin, 
                                            x_with_imputations = takapourewa_tmin_seadec_kalman, 
                                            x_with_truth = longest_contingous_complete_case_tmin_df$takapourewa_tmin_c,
                                            title = NULL,
                                            subtitle = "seasonal decomposition with kalman smoothing algorithm",
                                            legend = FALSE)
seadec_ma_plot <- ggplot_na_imputations(x_with_na = takapourewa_tmin, 
                                        x_with_imputations = takapourewa_tmin_seadec_ma, 
                                        x_with_truth = longest_contingous_complete_case_tmin_df$takapourewa_tmin_c,
                                        title = NULL,
                                        subtitle = "seasonal decomposition with moving average algorithm")

cowplot::plot_grid(linear_plot, spline_plot, ncol = 1)
cowplot::plot_grid(seadec_kalman_plot, seadec_ma_plot, ncol = 1)

# doesnt look great without row ID because of overlap from duplicates
test_predictions %>% # legend doesnt show (likly due to 2 geom_points?)
          ggplot()+
          geom_abline(intercept=0, slope=1)+
          geom_point(aes(x = actual, y = linear_interpolated), color = "red")+
          geom_point(aes(x = actual, y = spline_interpolated), color = "purple")+
          labs(y = 'predicted', title = "actual vs predicted NA impution via interpolation")+
          scale_color_manual(values=c("red", "purple"), 
                             name="interpolation",
                             breaks=c("linear_interpolated", "spline_interpolated"),
                             labels=c("linear", "spline"))

test_predictions %>% # legend doesnt show (likly due to 2 geom_points?)
          ggplot()+
          geom_abline(intercept=0, slope=1)+
          geom_point(aes(x = actual, y = seadec_kalman), color = "green")+
          geom_point(aes(x = actual, y = seadec_ma), color = "blue")+
          labs(y = 'predicted', title = "actual vs predicted NA impution via seasonal decomp")+
          scale_color_manual(values=c("red", "purple"), 
                             name="seasonal decomp algorithm",
                             breaks=c("kalman_smoothing", "moving_average"),
                             labels=c("kalman_smoothing", "moving_average"))

# residual plots (predicted vs residuals)
# yikes 
test_predictions %>% # 
          ggplot()+
          geom_abline(intercept=0, slope=0)+
          geom_point(aes(x = linear_interpolated, y = linear_resid), color = "red")+
          geom_point(aes(x = spline_interpolated, y = spline_resid), color = "purple")+
          labs(x = 'predicted', y = 'residuals', title = "residual plot - interpolated")

test_predictions %>% 
          ggplot()+
          geom_abline(intercept=0, slope=0)+
          geom_point(aes(x = seadec_kalman, y = seadec_kalman_resid), color = "black")+
          labs(x = 'predicted', y = 'residuals', title = "residual plot - seasonal decompositon with kalman smoothing")

test_predictions %>% 
          ggplot()+
          geom_abline(intercept=0, slope=0)+
          geom_point(aes(x = seadec_ma, y = seadec_ma_resid), color = "black")+
          labs(x = 'predicted', y = 'residuals', title = "residual plot - seasonal decompositon with moving average")

test_predictions %>% # why does it look like this?
          ggplot()+
          geom_abline(intercept=0, slope=0)+
          geom_point(aes(x = seadec_kalman, y = seadec_kalman_resid), color = "green")+
          geom_point(aes(x = seadec_ma, y = seadec_ma_resid), color = "blue")

