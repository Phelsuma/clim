# tmax 
# test gap filling methods
# probably should make some functions if keeping
library(imputeTS) # https://cran.rstudio.com/web/packages/imputeTS/vignettes/Cheat_Sheet_imputeTS.pdf
library(Metrics)

citation("imputeTS")
citation("Metrics")

longest_contingous_complete_case_tmax_df <- read_csv("data/longest_contingous_complete_case_tmax_df.csv")

# create gaps
set.seed(2023)

takapourewa_tmax <- longest_contingous_complete_case_tmax_df$takapourewa_tmax_c
len_tmax <- 1:length(takapourewa_tmax)  
ind_tmax <- sample(len_tmax, 25) # select 25 rows by random 
takapourewa_tmax[ind_tmax] <- NA # inserts NA into wherever those numbers occur 
takapourewa_actual <- longest_contingous_complete_case_tmax_df$takapourewa_tmax_c[ind_tmax] # compare to real data

# gap filling
# linear_interpolate
takapourewa_interpolated_linear <- na_interpolation(takapourewa_tmax, option = "linear") # interpolate
interpolated_values_linear <- takapourewa_interpolated_linear[ind_tmax] # interpolated values
# spline interpolate 
takapourewa_interpolated_spline <- na_interpolation(takapourewa_tmax, option = "spline") # interpolate
interpolated_values_spline <- takapourewa_interpolated_spline[ind_tmax] # interpolated values
# Kalman smoothing
takapourewa_kalman <- na_kalman(takapourewa_tmax)
takapourewa_kalman_values <- takapourewa_kalman[ind_tmax] # kalman values
# Last Observation Carried Forward (na_locf)
takapourewa_locf <- na_locf(takapourewa_tmax)
takapourewa_locf_values <- takapourewa_locf[ind_tmax] # na_locf values
# moving average (na_ma)
takapourewa_ma <- na_ma(takapourewa_tmax)
takapourewa_ma_values <- takapourewa_ma[ind_tmax] # moving average values
# seasonal decomp (na_seadec) interpolation
takapourewa_seadec_interp <- na_seadec(takapourewa_tmax, find_frequency = TRUE, algorithm = "interpolation") 
takapourewa_seadec_interp_values <- takapourewa_seadec_interp[ind_tmax] # seasonal decomposition values
# seasonal decomp (na_seadec) locf
takapourewa_seadec_locf <- na_seadec(takapourewa_tmax, find_frequency = TRUE, algorithm = "locf") 
takapourewa_seadec_locf_values <- takapourewa_seadec_locf[ind_tmax] # seasonal decomposition locf values
# seasonal decomp (na_seadec) kalman
takapourewa_seadec_kalman <- na_seadec(takapourewa_tmax, find_frequency = TRUE, algorithm = "kalman")
takapourewa_seadec_kalman_values <- takapourewa_seadec_kalman[ind_tmax] # seasonal decomposition kalman values
# seasonal decomp (na_seadec) ma
takapourewa_seadec_ma <- na_seadec(takapourewa_tmax, find_frequency = TRUE, algorithm = "ma")
takapourewa_seadec_ma_values <- takapourewa_seadec_ma[ind_tmax] # seasonal decomposition kalman values
# na_seasplit (not worth considering )
# takapourewa_seasplit <- na_seasplit(takapourewa_tmax, find_frequency = TRUE) # various algoroithms none accurate
# takapourewa_seasplit_values <- takapourewa_seasplit[ind_tmax] 

# actual vs predicted - get residuals
test_predictions <- as_tibble(ind_tmax) %>% 
          rename(row_id = 'value') %>% 
          bind_cols(actual = takapourewa_actual) %>% 
          bind_cols(linear_interpolated = interpolated_values_linear) %>% # linear interpolated 
          mutate('linear_resid' = actual - linear_interpolated) %>% 
          bind_cols(spline_interpolated = interpolated_values_spline) %>% # spine interpolated 
          mutate('spline_resid' = actual - spline_interpolated) %>%
          bind_cols(kalman = takapourewa_kalman_values) %>% # Kalman Smoothing
          mutate('kalman_resid' = actual - kalman) %>%
          bind_cols(locf = takapourewa_locf_values) %>% # Last Observation Carried Forward 
          mutate('locf_resid' = actual - locf) %>%
          bind_cols(ma = takapourewa_ma_values) %>% # moving average
          mutate('ma_resid' = actual - ma) %>%
          bind_cols(seadec_interp = takapourewa_seadec_interp_values) %>% # seasonal decomp interpolation
          mutate('seadec_interp_resid' = actual - seadec_interp) %>% 
          bind_cols(seadec_locf = takapourewa_seadec_locf_values) %>% # seasonal decomp locf
          mutate('seadec_locf_resid' = actual - seadec_locf) %>% 
          bind_cols(seadec_kalman = takapourewa_seadec_kalman_values) %>% # seasonal decomp kalman
          mutate('seadec_kalman_resid' = actual - seadec_kalman) %>% 
          bind_cols(seadec_ma = takapourewa_seadec_ma_values) %>% # seasonal decomp ma
          mutate('seadec_ma_resid' = actual - seadec_ma)
          
ss_preds <- test_predictions %>% 
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
          arrange(means) 

# Calculate root mean square error (RMSE)
sqrt(mean(test_predictions$linear_resid^2)) # 1.622984
sqrt(mean(test_predictions$spline_resid^2)) # 1.930751
# RMSE (same as above)  
ss_RMSE_tmax <- test_predictions %>%
          summarise(
                    linear_interp = Metrics::rmse(test_predictions$actual,test_predictions$linear_interpolated),# 
                    spline_interp = Metrics::rmse(test_predictions$actual,test_predictions$spline_interpolated),# 
                    kalman = Metrics::rmse(test_predictions$actual, test_predictions$kalman),# 
                    locf = Metrics::rmse(test_predictions$actual, test_predictions$locf),# 
                    ma = Metrics::rmse(test_predictions$actual, test_predictions$ma),# 1.486927
                    seadec_interp = Metrics::rmse(test_predictions$actual, test_predictions$seadec_interp),# 
                    seadec_locf = Metrics::rmse(test_predictions$actual, test_predictions$seadec_locf),# 
                    seadec_kalman = Metrics::rmse(test_predictions$actual, test_predictions$seadec_kalman),# 
                    seadec_ma = Metrics::rmse(test_predictions$actual, test_predictions$seadec_ma)) %>%  # 1.528830
          pivot_longer(everything(), names_to = 'NA_fill', values_to = 'RMSE') %>%
          arrange(RMSE) 
# two best = ma (1.48693) & seadec_ma (1.52883)

# plot interpolated values 
linear_plot <- ggplot_na_imputations(x_with_na = takapourewa_tmax, 
                      x_with_imputations = takapourewa_interpolated_linear, 
                      x_with_truth = longest_contingous_complete_case_tmax_df$takapourewa_tmax_c,
                      title = NULL,
                      subtitle = "linear",
                      legend = FALSE)
spline_plot <- ggplot_na_imputations(x_with_na = takapourewa_tmax, 
                      x_with_imputations = takapourewa_interpolated_spline, 
                      x_with_truth = longest_contingous_complete_case_tmax_df$takapourewa_tmax_c,
                      title = NULL,
                      subtitle = "spline")
seadec_kalman_plot <- ggplot_na_imputations(x_with_na = takapourewa_tmax, 
                                     x_with_imputations = takapourewa_seadec_kalman, 
                                     x_with_truth = longest_contingous_complete_case_tmax_df$takapourewa_tmax_c,
                                     title = NULL,
                                     subtitle = "seasonal decomposition with kalman smoothing algorithm",
                                     legend = FALSE)
seadec_ma_plot <- ggplot_na_imputations(x_with_na = takapourewa_tmax, 
                      x_with_imputations = takapourewa_seadec_ma, 
                      x_with_truth = longest_contingous_complete_case_tmax_df$takapourewa_tmax_c,
                      title = NULL,
                      subtitle = "seasonal decomposition with moving average algorithm")

cowplot::plot_grid(linear_plot, spline_plot, ncol = 1)
cowplot::plot_grid(seadec_kalman_plot, seadec_ma_plot, ncol = 1)

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
# yikes # test gap filling methods
# probably should make some functions if keeping
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
          
#  needs custom legend 
test_predictions %>% 
          ggplot()+
          geom_abline(intercept=0, slope=0)+
          geom_point(aes(x = ind_tmax, y = seadec_kalman_resid), color = "green")+
          geom_point(aes(x = ind_tmax, y = seadec_ma_resid), color = "blue")+
          geom_point(aes(x = ind_tmax, y = kalman_resid), color = "red")+
          geom_point(aes(x = ind_tmax, y = ma_resid), color = "purple")+
          labs(x = 'index', y = 'residuals', title = "residual plot - model comparison")


