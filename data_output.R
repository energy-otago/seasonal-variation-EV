library(tidyverse)
library(tseries)

# only run if need to process the data
# source("single_script_project.rda") 


# load in data and run the necessary functions with these 2 lines
# after this line (plus packages above) are run, any one of the below figures and data should work Independent of all the other code in the document
load("processed_data.rda")


# commented out code can not be run without VKT data or fuel trade data which is not publicly avilable.









# Figure 1
auck_econ_series = ts(monthly_reg_EV_data$mean_econ[which(monthly_reg_EV_data$weather_region == "auckland")], frequency = 12)
decomp_auck_econ = decompose(auck_econ_series,"multiplicative")
auck_HDD_series = ts(monthly_reg_EV_data$HDD[monthly_reg_EV_data$weather_region == "auckland"], frequency = 12)
decomp_auck_HDD = decompose(auck_HDD_series,"multiplicative")

par(mar = c(3, 4, 1, 4))
plot(decomp_auck_econ$figure*mean(decomp_auck_econ$x), type = 'b', xaxt = "n",
     xlab = "", ylab = "Proportinal Change in Energy Economy", las = 1)
axis(1, labels = month.abb, at = 1:12, las = 1)

par(new=TRUE)
plot(decomp_auck_HDD$figure*mean(decomp_auck_HDD$x), type = 'b', col = 2, xlab="", ylab="",axes=FALSE)
mtext("Average Heating Degree (° Days per Day) (Base temp 14°C)", side=4, line=2, cex = 1, padj = 1, col = 1) 
axis(4, las = 1, col = 2, col.axis = 2)

legend("topright", inset = legend_inset, legend = c("EV Energy Economy", "HDD"), lty = 1, col = 1:2, pch = 1)

write.csv(data.frame(econ = decomp_auck_econ$figure*mean(decomp_auck_econ$x), HDD = decomp_auck_HDD$figure*mean(decomp_auck_HDD$x), month = 1:12), "exported_data/auck_season_decomp_fig_1.csv")







# Figure 2
# decomp_econ = monthly_EV_data$mean_econ %>% 
#   ts(frequency = 12, start = 2017) %>% 
#   decompose("multiplicative")
# decomp_petrol = fuel_trade$Petrol[fuel_trade$year < 2020] %>% 
#   ts(frequency = 4, start = min(fuel_trade$year)) %>% 
#   decompose("multiplicative")
# decomp_vkt = vkt_quart_pass[vkt_quart_pass$year >= 2003,]$total %>% 
#   ts(frequency = 4, start = min(2003)) %>% 
#   decompose("multiplicative")
# 
# plot(decomp_econ$figure, type = 'b', xaxt = "n",
#      xlab = "", ylab = "Proportional Change", las = 1)
# axis(1, labels = month.abb, at = 1:12, las = 1)
# 
# points(unique(fuel_trade$month)-1,decomp_petrol$figure, type = 'b', col = 2, xlab="", ylab="")
# 
# points(c(2,5,8,11), decomp_vkt$figure, type = 'b', col = 3, xlab="", ylab="")
# 
# legend("topright", inset = legend_inset, legend = c("EV Energy Economy", "Petrol Usage", "Ministry of Transport VKT"), lty = 1,pch = 1, col = 1:3)
# 
# write.csv(data.frame(ev_econ = decomp_econ$figure, petrol = decomp_petrol$figure, vkt = decomp_vkt$figure, month = 1:12), "exported_data/VKT_season_decomp_fig_2.csv")
# 





# Table 3
write.csv(eff_hierc_grad_cor_model[["coefficients"]][["random"]][["model"]], "exported_data/hierc_coef_tab_3.csv")






# Table 4
write.csv(eff_hierc_grad_cor_model[["coefficients"]][["fixed"]][-1:-3], "exported_data/region_coef_tab4.csv")
write.csv(EV_data %>% group_by(weather_region) %>% summarise(unique_model_count = n_distinct(vehicle)), "exported_data/region_vehicle_count_tab4.csv")







# Figure 3
car_num = 1

lm_resid_ts = zoo(lm_resid[EV_data$vehicle == (EV_data$vehicle %>% levels)[car_num]],
                  order.by = EV_data$time_month[EV_data$vehicle == (EV_data$vehicle %>% levels)[car_num]])
acf_vehicle1_lm = acf(lm_resid_ts,
    na.action = na.pass,
    lag.max = 18,
    main = "Linear Model",
    xlab = "Lag (months)")

hierc_grad_cor_m_resid_ts = zoo(hierc_grad_cor_m_resid[EV_data$vehicle == (EV_data$vehicle %>% levels)[car_num]],
                                order.by = EV_data$time_month[EV_data$vehicle == (EV_data$vehicle %>% levels)[car_num]])
acf_vehicle1_fm = acf(hierc_grad_cor_m_resid_ts,
    na.action = na.pass,
    lag.max = 18,
    main = "Gradient Mixed Model with Exponential Correlation",
    xlab = "Lag (months)")

car_num = 10

lm_resid_ts = zoo(lm_resid[EV_data$vehicle == (EV_data$vehicle %>% levels)[car_num]],
                  order.by = EV_data$time_month[EV_data$vehicle == (EV_data$vehicle %>% levels)[car_num]])
acf_vehicle10_lm = acf(lm_resid_ts,
    na.action = na.pass,
    lag.max = 18,
    main = "Linear Model",
    xlab = "Lag (months)")

hierc_grad_cor_m_resid_ts = zoo(hierc_grad_cor_m_resid[EV_data$vehicle == (EV_data$vehicle %>% levels)[car_num]],
                                order.by = EV_data$time_month[EV_data$vehicle == (EV_data$vehicle %>% levels)[car_num]])
acf_vehicle10_fm = acf(hierc_grad_cor_m_resid_ts,
    na.action = na.pass,
    lag.max = 18,
    main = "Gradient Mixed Model with Exponential Correlation",
    xlab = "Lag (months)")


acf_data = data.frame(vehicle1_lm = acf_vehicle1_lm$acf, vehicle1_fm = acf_vehicle1_fm$acf, vehicle10_lm = acf_vehicle10_lm$acf, vehicle10_fm = acf_vehicle10_fm$acf)
write.csv(acf_data, file = "exported_data/acf_data_fig3.csv")





# Figure 4
ggplot(EV_test_data_nnv, aes(y = predict_vehicle_econ, x = economy, color = log10(distance))) +
  geom_point() +
  geom_abline(slope = 1) +
  scale_color_gradient2(
    name = "Distance (km)",
    low = "#ffaaaa", mid ="#77aaff", high = "#000000", midpoint = 2.1,
    breaks = c(1:4), labels = 10^(1:4)
  ) +
  xlab("Measured economy (Wh/km)")+
  ylab("Predicted economy (Wh/km)")+
  gg_theme


write.csv(EV_test_data_nnv,'exported_data/predicted_vs_measured_vehicle_fig4.csv')







# Figure 5
# measured vs predicted data for individual vehicle
ggplot(EV_test_data_nnm, aes(y = predict_vehicle_econ, x = economy, color = log10(distance))) +
  geom_point() +
  geom_abline(slope = 1) +
  scale_color_gradient2(
    name = "Distance (km)",
    low = "#ffaaaa", mid ="#77aaff", high = "#000000", midpoint = 2.1,
    breaks = c(1:4), labels = 10^(1:4)
  ) +
  xlab("Measured economy (Wh/km)")+
  ylab("Predicted economy (Wh/km)")+
  gg_theme

write.csv(EV_test_data_nnm,'exported_data/predicted_vs_measured_model_fig5.csv')

# predicted_vs_measured total averaged out power
ggplot(usage_data2, aes(x = date)) +
  geom_line(aes(y = total_mwh, color = "Measured", linetype="Measured")) +
  geom_line(aes(y = pred_mwh, color = "Predicted", linetype="Predicted"), na.rm = TRUE) +
  scale_x_date(
    date_labels = "%b-%Y",
    date_breaks = "6 months",
    limits = c(as.Date("2021-06-01"),as.Date("2023-01-01"))
  ) +
  labs(x = "", y = "Monthly Power Usage (MWh)") +
  scale_color_manual(name = "Power Usage", values = c("Measured" = "black", "Predicted" = "red")) +
  scale_linetype_manual(name = "Power Usage", values = c("Measured" = "solid", "Predicted" = "dashed"), drop = FALSE) +
  gg_theme

write.csv(usage_data2,'exported_data/power_usage_fig5.csv')









# Figure 6
ggplot(vehicle_auck_pred) +
  # geom_jitter(
  #   aes(
  #     x = vehicle_type,
  #     y = vehicle_pred_consum,
  #     size=count
  #   ),
  #   color="black",
  #   alpha=0.2,
  #   width = 0.3
  # ) +
  # scale_size_continuous(
  #   range = c(0.5, 4),
  #   breaks=c(min(vehicle_auck_pred$count),3,10,20,40, max(vehicle_auck_pred$count))
  # ) +
  geom_boxplot(
    aes(
      x = vehicle_type,
      y = vehicle_pred_consum,
      fill = vehicle_type,
      color = vehicle_type
    ),
    alpha=0.6,
    lwd=0.8,
    fatten = 1
  ) +
    ggtitle("Modelled Vehicle Consumption by Vehicle Type (using Auckland Consumption)") +
    xlab("Vehicle Type") +
    ylab("Estimated Consumption (Wh/km)") +
    labs(
      size = "No. Monthly Measurements",
      fill = "Vehicle Type",
      color = "Vehicle Type"
    ) +
  gg_theme

write.csv(vehicle_auck_pred,'exported_data/vehicle_type_auck_pred_fig6.csv')










# Figure 7 

ggplot(clyde_heat_comp_data, aes(x = Month, y = pred_econ, color = model)) +
  geom_line() +
  ylab("Predicted economy (Wh/km)") +
  xlab("") +
  scale_x_discrete(limits = month.abb) +
  gg_theme

write.csv(clyde_heat_comp_data,'exported_data/clyde_heat_pump_comp_fig7.csv')




# Figure 8 could theoretically be done without VKT data however our process we modeled whole scenario at once so this was not done. 

# # Figure 8 
# 
# dist1_pred %>% 
#   group_by(year, month, model) %>% 
#   summarise(
#     model_econ = sum(pred_econ*distance/sum(distance)),
#     model_econ_lc = sum(pred_econ*distance/sum(distance)) - sqrt(sum((std_err*distance/sum(distance))^2)),
#     model_econ_uc = sum(pred_econ*distance/sum(distance)) + sqrt(sum((std_err*distance/sum(distance))^2))
#   ) %>%
#   ggplot() +
#   geom_line(aes(x = month, y = model_econ, color = model, linetype = "Prediction")) +
#   geom_line(aes(x = month, y = model_econ_lc, color = model, linetype = "Confidence Interval")) +
#   geom_line(aes(x = month, y = model_econ_uc, color = model, linetype = "Confidence Interval")) +
#   scale_x_discrete(limits = month.abb) +
#   scale_linetype_manual(
#     name = "Economy", 
#     values = c("Prediction" = "solid", "Confidence Interval" = "dashed")
#   ) +
#   ylab("EV Average NZ wide Economy (Wh/km)") +
#   xlab("") +
#   gg_theme
# 
# write.csv(dist1_pred,'exported_data/dist1_pred_fig8.csv')
# 
# 
# # Figure 9 
# 
# # did Michael use the future VKT numbers
# # Auckland
# dist1_pred %>% 
#   filter(vkt_region == "Auckland") %>%
#   group_by(year, month) %>% 
#   summarise(
#     total_wh = sum(pred_wh),
#     total_wh_lc = sum(pred_wh) - sqrt(sum(wh_std_err^2)),
#     total_wh_uc = sum(pred_wh) + sqrt(sum(wh_std_err^2))
#   ) %>% 
#   ggplot() +
#   geom_line(aes(x = month, y = total_wh/1e9, linetype = "Prediction")) +
#   geom_line(aes(x = month, y = total_wh_lc/1e9, linetype = "Confidence Interval")) +
#   geom_line(aes(x = month, y = total_wh_uc/1e9, linetype = "Confidence Interval")) +
#   scale_x_discrete(limits = month.abb) +
#   scale_linetype_manual(
#     name = "Power Usage",
#     values = c("Prediction" = "solid", "Confidence Interval" = "dashed")
#   ) +
#   ylab("Auckland total EV predicted electricity demand (GWh)") +
#   xlab("") +
#   gg_theme
# 
# # Wellington
# dist1_pred %>% 
#   filter(vkt_region == "Wellington") %>%
#   group_by(year, month) %>% 
#   summarise(
#     total_wh = sum(pred_wh),
#     total_wh_lc = sum(pred_wh) - sqrt(sum(wh_std_err^2)),
#     total_wh_uc = sum(pred_wh) + sqrt(sum(wh_std_err^2))
#   ) %>% 
#   ggplot() +
#   geom_line(aes(x = month, y = total_wh/1e9, linetype = "Prediction")) +
#   geom_line(aes(x = month, y = total_wh_lc/1e9, linetype = "Confidence Interval")) +
#   geom_line(aes(x = month, y = total_wh_uc/1e9, linetype = "Confidence Interval")) +
#   scale_x_discrete(limits = month.abb) +
#   scale_linetype_manual(
#     name = "Power Usage",
#     values = c("Prediction" = "solid", "Confidence Interval" = "dashed")
#   ) +
#   ylab("Auckland total EV predicted electricity demand (GWh)") +
#   xlab("") +
#   gg_theme
# 
# # Central Otago
# dist1_pred %>% 
#   filter(vkt_region == "Central.Otago") %>%
#   group_by(year, month) %>% 
#   summarise(
#     total_wh = sum(pred_wh),
#     total_wh_lc = sum(pred_wh) - sqrt(sum(wh_std_err^2)), 
#     total_wh_uc = sum(pred_wh) + sqrt(sum(wh_std_err^2))
#   ) %>% 
#   ggplot() +
#   geom_line(aes(x = month, y = total_wh/1e9, linetype = "Prediction")) +
#   geom_line(aes(x = month, y = total_wh_lc/1e9, linetype = "Confidence Interval")) +
#   geom_line(aes(x = month, y = total_wh_uc/1e9, linetype = "Confidence Interval")) +
#   scale_x_discrete(limits = month.abb) +
#   scale_linetype_manual(
#     name = "Power Usage",
#     values = c("Prediction" = "solid", "Confidence Interval" = "dashed")
#   ) +
#   ylab("Otago total EV predicted electricity demand (GWh)") +
#   xlab("") +
#   gg_theme
# 
# write.csv(dist1_pred,'exported_data/dist1_pred_fig9.csv')
# 
# 
