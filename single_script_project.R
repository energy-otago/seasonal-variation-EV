library(caret)
library(parallel)
library(doParallel)
library(ggplot2)
library(zoo)
library(tidyverse)
library(nlme)
library(tidyverse)
library(rlist)




#list index (first) is the weather location to use and value (second) is the region in the flip the fleet data 
weather_regions = list("Auckland" = "Auckland", "Upper Hutt" = "Wellington",
                       "Christchurch" = "Christchurch", "Dunedin" = "Coastal Otago",
                       "Hamilton" = "Waikato", "Rotorua" = "Bay of Plenty",
                       "Christchurch" = "Mid Canterbury", "Christchurch" = "North Canterbury",
                       "Christchurch" = "South Canterbury", "Clyde" = "Central Otago",
                       "Nelson" = "Nelson", "Whangarei" = "Whangarei", "Whangarei" = "Far North",
                       "Upper Hutt" = "Wairarapa", "Auckland" = "Rodney", "Dunedin" = "Waitaki",
                       "Nelson" = "Golden Bay",  "Auckland" = "Coromandel", "Palmerston North" = "Manawatu",
                       "Nelson" = "Marlborough", "Stratford" = "Taranaki", "Napier" = "Hawkes Bay",
                       "Invercargill" = "Southland", "Greymouth" = "North Westland",
                       "Turangi" = "Central Plateau", "Gisborne" = "Gisborne")
month_length = c(31,28,31,30,31,30,31,31,30,31,30,31)


#main EV data
EV_data = read.csv("data/ftf_ev_efficiency_distance_models_20230118_v1.1.csv")[-1]
# Map region names to weather regions
EV_data$weather_region = as.factor(names(weather_regions)[match(EV_data$region,  weather_regions)])
EV_data$economy = 1000/EV_data$efficiency # converting from km/kWh (efficiency) to Wh/km (economy)
EV_data$wh = EV_data$kwh*1000 # adding column for Wh instead of kWh for better use with economy
EV_data = EV_data[EV_data$distance !=0, ]




#remove PHEV
EV_data = EV_data[  EV_data$model != "Mitsubishi Outlander" &
                      EV_data$model != "Toyota Prius" &
                      EV_data$model != "Mini Countryman PHEV" &
                      EV_data$model != "Conversion to EV" &
                      EV_data$model !="Audi A3 e-tron" &
                      EV_data$model != "Mg Hs Phev/10Hd", ]

#rename vehicle names to simpler names 
EV_data$model[EV_data$model == "Polestar Polestar: 2 Lrsm 78Kwh/Ev/Fd Hatchback"] = "Polestar 2"
EV_data$model[EV_data$model == "Kia Ev6 Air Rwd Sr: 58Kwh/Ev"] = "Kia EV6"
EV_data$model[EV_data$model == "Mercedes EQC 400 all-electric SUV"] = "Mercedes EQC 400 EV SUV"


#unreasonable measurements (generally low distance and 300+ economy)
EV_data = EV_data[  (EV_data$vehicle != "55783a1b" | EV_data$year != 2022 | EV_data$month != 10) &
                      (EV_data$vehicle != "ae05b6a6" | EV_data$year != 2019 | EV_data$month != 5 ) &
                      (EV_data$vehicle != "667822bd" | EV_data$year != 2020 | EV_data$month != 9 ) &
                      (EV_data$vehicle != "869f5d2a" | EV_data$year != 2022 | EV_data$month != 11)
                    ,]

#bad data vehicles
EV_data = EV_data[  EV_data$vehicle != "38f81643" &
                      # EV_data$vehicle != "ae05b6a6" &
                      # EV_data$vehicle != "667822bd" &
                      EV_data$vehicle != "ad7a2a1e" &
                      EV_data$vehicle != "88ee3e2d" &
                      EV_data$vehicle != "e6959846" &
                      EV_data$vehicle != "8a170585" &
                      EV_data$vehicle != "ea66a66d"
                    , ]


# Load weather data
load("data/weather_data.rda")











# functions to process weather data

# Function to calculate Heating Degree Days (HDD) for a given base temperature 
HDD = function (data, temp = 16) {
  mon_sum = data.frame(unique(data[,c("Year","Month")]))
  
  # Heating Degrees for a given temperature 
  adj_temps = -data$Temp + temp
  
  # Calculate HDD for each unique year and month combination
  calc = function(ym) {
    curr_month_adj_temp = adj_temps[data$Year == ym[1] & data$Month == ym[2]]
    sum(curr_month_adj_temp*(curr_month_adj_temp > 0))/24
  }
  HDD_vec = apply(mon_sum, MARGIN = 1, calc)
  mon_sum$HDD = HDD_vec
  mon_sum$City = data$City[1]
  return(mon_sum)
}


# Function to calculate Cooling Degree Days (CDD) for a given base temperature
CDD = function (data, temp = 22) {
  mon_sum = data.frame(unique(data[,c("Year","Month")]))
  
  # Cooling Degrees for a given temperature 
  adj_temps = data$Temp - temp
  
  # Calculate CDD for each unique year and month combination
  calc = function(ym) {
    curr_month_adj_temp = adj_temps[data$Year == ym[1] & data$Month == ym[2]]
    sum(curr_month_adj_temp*(curr_month_adj_temp > 0))/24
  }
  CDD_vec = apply(mon_sum, MARGIN = 1, calc)
  mon_sum$CDD = CDD_vec
  mon_sum$City = data$City[1]
  return(mon_sum)
}


# Function to calculate average temperature for each year, month, and city
avg_temp = function(data) {
  avg_temp = data %>% 
    group_by(Year, Month, City) %>% 
    summarise(avg_temp = mean(Temp))
  return(avg_temp)
}

# Function to perform cross-validation and find the best combination of HDD and CDD thresholds
# CDD is a struggle to fit but HDD seems to work fairly well
# base_values are just test values to see if the function works
cross_val_base_temp = function(EV_data, weather_data, HDD_base_values = c(16,20,24,30,400), CDD_base_values = c(10,20,23,30)) {
  # Initialize variables
  best_score = Inf
  HDD_scores = c()
  CDD_scores = c()
  scores = c()
  message("Calculating HDD")
  HDD_data_collection = list()
  
  # Iterate through HDD and CDD base values to calculate HDD and CDD for each base value
  for (HDD_temp in HDD_base_values) {
    message(HDD_temp)
    HDD_data = Reduce(rbind, lapply(weather_data, \(data) HDD(data, temp = HDD_temp)))
    for (i in 1:12) {
      HDD_data$HDD[HDD_data$Month == i] = HDD_data$HDD[HDD_data$Month == i]/month_length[i]
    }
    HDD_data_collection = append(HDD_data_collection, list(HDD_data))
  }
  names(HDD_data_collection) = HDD_base_values
  message("Calculating CDD")
  CDD_data_collection = list()
  for (CDD_temp in CDD_base_values) {
    message(CDD_temp)
    CDD_data = Reduce(rbind, lapply(weather_data, \(data) CDD(data, temp = CDD_temp)))
    for (i in 1:12) {
      CDD_data$CDD[CDD_data$Month == i] = CDD_data$CDD[CDD_data$Month == i]/month_length[i]
    }
    CDD_data_collection = append(CDD_data_collection, list(CDD_data))
  }
  names(CDD_data_collection) = CDD_base_values
  
  
  # Create a parallel cluster for faster computation
  cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
  registerDoParallel(cluster)
  message("Running Cross validation") 
  # Cross validation setting
  train_control = trainControl(method = "repeatedcv",
                               number = 7,
                               repeats = 50
                               , allowParallel = TRUE # increases run time if short list of values to check but for longer list runs faster when included
  )
  
  # Loop through different combinations of HDD and CDD thresholds
  for (HDD_temp in HDD_base_values) {
    EV_data_temp = merge(EV_data, HDD_data_collection[[as.character(HDD_temp)]], by.x = c("year", "month", "weather_region"), by.y = c("Year", "Month", "City"), all.x = T)
    for (CDD_temp in CDD_base_values) {
      EV_data_temp2 = merge(EV_data_temp, CDD_data_collection[[as.character(CDD_temp)]], by.x = c("year", "month", "weather_region"), by.y = c("Year", "Month", "City"), all.x = T)
      #Using simple linear model due to much faster fitting allows for more cross validation runs
      model = train(economy ~ HDD + CDD + weather_region + model,
                    data = EV_data_temp2,
                    na.action=na.omit,
                    weights = distance,
                    method = "lm",
                    trControl = train_control)
      message("HDD: ", HDD_temp, " CDD: ", CDD_temp, " RSME: ",model$results$RMSE)
      HDD_scores = append(HDD_scores, HDD_temp)
      CDD_scores = append(CDD_scores, CDD_temp)
      scores = append(scores, model$results$RMSE)
      
      if(model$results$RMSE < best_score) {
        best_score = model$results$RMSE
        best_HDD = HDD_temp
        best_CDD = CDD_temp
      }
    }
  }
  
  stopCluster(cluster)
  # best_HDD = 16
  # best_CDD = 24
  # cv_scores_plot = ggplot(mapping = aes(x = HDD_scores, y = CDD_scores, colour = scores)) + geom_point()
  return(list(best_HDD, best_CDD, HDD_scores, CDD_scores, scores, cv_scores_plot))
}

# Function to combine EV data with calculated HDD, CDD, and average temperature data
combine_ev_temp = function(EV_data, weather_data, HDD_temp = 16, CDD_temp = 22) {
  
  # add the HDD to each data point in the weather data
  HDD_data = Reduce(rbind, lapply(weather_data, \(data) HDD(data, temp = HDD_temp)))
  CDD_data = Reduce(rbind, lapply(weather_data, \(data) CDD(data, temp = CDD_temp)))
  avg_temp_data = Reduce(rbind, lapply(weather_data, avg_temp))
  
  # convert HDD to HDD per day 
  for (i in 1:12) {
    HDD_data$HDD[HDD_data$Month == i] = HDD_data$HDD[HDD_data$Month == i]/month_length[i]
    CDD_data$CDD[CDD_data$Month == i] = CDD_data$CDD[CDD_data$Month == i]/month_length[i]
  }
  
  # Merge all calculated data with EV data
  EV_data = merge(EV_data, HDD_data, by.x = c("year", "month", "weather_region"), by.y = c("Year", "Month", "City"), all.x = T)
  EV_data = merge(EV_data, CDD_data, by.x = c("year", "month", "weather_region"), by.y = c("Year", "Month", "City"), all.x = T)
  EV_data = merge(EV_data, avg_temp_data, by.x = c("year", "month", "weather_region"), by.y = c("Year", "Month", "City"), all.x = T)
  
  
  return(list(EV_data, HDD_data, CDD_data))
}











# back to combining weather and EV data


# temp calculations 
# base_temp calculations are very slow to run so have been disabled. can run if new data but the results i have gotten have been saved 
# base_temps = cross_val_base_temp(EV_data, weather_data, HDD_base_values = 12:18, CDD_base_values = seq(16, 30, by = 2))
# HDD_base_temp = base_temps[[1]] # historical record of best values 14 14 14 14 14 14 14 14 14 started CDD by 2 14 14 14 increase n,k fold 14 14 14 14 14 new data 14 14 14
# CDD_base_temp = base_temps[[2]] # historical record of best values 22 19 20 23 25 22 25 26 24 started CDD by 2 24 24 26 increase n,k fold 26 24 28 26 24 new data 26 24 24
# cv_scores_plot = base_temps[[6]]

# temporary variable so that I do not have to run the slow cross validation each time
HDD_base_temp = 14 # 14 seems to remove all correlation for CDD if CDD is < 24 ish
CDD_base_temp = 24
# Combining EV data with calculated temperature data
temp_list = combine_ev_temp(EV_data, weather_data, HDD_temp = HDD_base_temp, CDD_temp = CDD_base_temp)
EV_data = temp_list[[1]]
HDD_data = temp_list[[2]]
CDD_data = temp_list[[3]]

#only use EV data from 2017
EV_data = EV_data[EV_data$year >= 2017,]
#remove EV data with missing weather regions
EV_data = na.omit(EV_data)


# Assign a time index based on months starting from 2017
EV_data$time_month = (EV_data$year-2017)*12 + EV_data$month
EV_data$date = as.yearmon(paste(EV_data$year, EV_data$month), "%Y %m") %>% as.Date


# Multi-region vehicle and model handling

# if one vehicle spans multiple regions, split into 2 different regions
multi_region_vehicle_regions = list()
multi_region_vehicle = c()
for (vehicle in unique(EV_data$vehicle)) {
  unique_weather_regions = EV_data[EV_data$vehicle == vehicle,]$weather_region %>% unique
  no_unique = unique_weather_regions %>% length
  if (no_unique > 1){
    multi_region_vehicle = multi_region_vehicle %>% append(vehicle)
    multi_region_vehicle_regions = multi_region_vehicle_regions %>% append(list(unique_weather_regions) )
  }
}
multi_region_vehicle
multi_region_vehicle_regions

for (vehicle_idx in seq_along(multi_region_vehicle)) {
  for (i in 2:length(multi_region_vehicle_regions[[vehicle_idx]]))
    EV_data[(EV_data$vehicle == multi_region_vehicle[vehicle_idx]) 
            & (EV_data$weather_region == multi_region_vehicle_regions[[vehicle_idx]][i]), "vehicle"] = EV_data[(EV_data$vehicle == multi_region_vehicle[vehicle_idx]) 
                                                                                                               & (EV_data$weather_region == multi_region_vehicle_regions[[vehicle_idx]][i]), "vehicle"] %>% paste(paste("multi_region", i), sep = "")
}

# original method: remove vehicles that span multiple different models.
# however on confirmation with Daniel it appears that it means that the first model they were using must be wrong model
multi_model_vehicle_models = list()
multi_model_vehicle = c()
for (vehicle in unique(EV_data$vehicle)) {
  unique_models = EV_data[EV_data$vehicle == vehicle,]$model %>% unique
  no_unique = unique_models %>% length
  if (no_unique > 1){
    multi_model_vehicle = multi_model_vehicle %>% append(vehicle)
    multi_model_vehicle_models = multi_model_vehicle_models %>% append(list(unique_models) )
  }
}
multi_model_vehicle
multi_model_vehicle_models

for (vehicle_id in multi_model_vehicle) {
  most_recent_model = EV_data$model[EV_data$time_month[EV_data$vehicle == vehicle_id] %>% which.max]
  EV_data$model[EV_data$vehicle == vehicle_id] = most_recent_model
}



# converting to lower case with no space names for weather regions if needed
# could potentially do this earlier in code if it becomes a problem (or maybe just make new column?)
levels(EV_data$weather_region) = tolower(levels(EV_data$weather_region))
levels(EV_data$weather_region) = sub(" ", "_", levels(EV_data$weather_region))





# Update factor levels based on popularity for better visualization
region_pop = EV_data %>%
  group_by(weather_region) %>% 
  summarise(count = n_distinct(vehicle), month_measurment = n()) %>% 
  arrange(-count)

model_pop = EV_data %>%
  group_by(model) %>% 
  summarise(count =  n_distinct(vehicle), month_measument = n()) %>%
  mutate(freq = count / sum(count)) %>% 
  arrange(-count)

vehicle_pop = EV_data %>%
  group_by(vehicle, model) %>% 
  summarise(count =  n()) %>% 
  arrange(-count)


EV_data$weather_region = factor(EV_data$weather_region, levels = region_pop$weather_region)
EV_data$model = factor(EV_data$model, levels = model_pop$model)
EV_data$vehicle = factor(EV_data$vehicle, levels = vehicle_pop$vehicle)

# also converting HDD and CDD to lower case
HDD_data$weather_region = sub(" ", "_", HDD_data$City) %>% tolower
CDD_data$weather_region = sub(" ", "_", CDD_data$City) %>% tolower

# Calculate average monthly HDD and CDD
HDD_mon_avg = HDD_data %>% 
  group_by(Month, weather_region) %>% 
  summarise(HDD = mean(HDD))
CDD_mon_avg = CDD_data %>% 
  group_by(Month, weather_region) %>% 
  summarise(CDD = mean(CDD))


#EV data with columns averaged by month
monthly_EV_data = EV_data %>% 
  group_by(year, month) %>% 
  summarise(mean_kwh = mean(kwh), mean_dist = mean(distance), mean_ef = mean(efficiency*distance)/mean(distance), mean_econ = mean(economy*distance)/mean(distance))
monthly_EV_data$m = 1:nrow(monthly_EV_data)

#EV data with columns averaged by month and weather region
monthly_reg_EV_data = EV_data %>% 
  na.omit() %>% 
  group_by(year, month, weather_region) %>% 
  summarise(mean_kwh = mean(kwh), mean_dist = mean(distance), mean_ef = mean(efficiency*distance)/mean(distance), mean_econ = mean(economy*distance)/mean(distance), HDD = mean(HDD), CDD = mean(CDD), avg_temp = mean(avg_temp))




















# Model used 
# Simple Linear model
eff_lm = lm(economy ~ HDD + CDD + weather_region + model,
            weights = distance,
            na.action=na.omit,
            data = EV_data
)


# Hierarchical model
eff_hierc_grad_cor_model = lme(fixed = economy ~ HDD + CDD + weather_region,
                               random = ~ 1 + HDD | model/vehicle,
                               weights = ~ I(1/distance),
                               correlation = corAR1(form = ~ time_month | model/vehicle),
                               na.action=na.omit,
                               data = EV_data
)

lm_resid = resid(eff_lm)
hierc_grad_cor_m_resid = resid(eff_hierc_grad_cor_model, type = "normalized")









# Prediction

# Train-test Split and training model

EV_training_data = EV_data[EV_data$year <2022,]
EV_test_data = EV_data[EV_data$year >=2022,]
EV_test_data_nnv = EV_test_data[EV_test_data$vehicle %in% EV_training_data$vehicle,]
EV_test_data_nnm = EV_test_data[EV_test_data$model %in% EV_training_data$model,]

econ_model = lme(fixed = economy ~ HDD + CDD + weather_region,
                   random = ~ 1 + HDD | model/vehicle,
                   weights = ~ I(1/distance),
                   correlation = corAR1(form = ~ time_month | model/vehicle),
                   data = EV_training_data
                   )
# prediction at the vehicle and model level for the test data with the model only using the test data
EV_test_data_nnv$predict_vehicle_econ = predict(econ_model, newdata = EV_test_data_nnv, level = 2)
EV_test_data_nnv$wh_pred = EV_test_data_nnv$predict_vehicle_econ * EV_test_data_nnv$distance

EV_test_data_nnm$predict_vehicle_econ = predict(econ_model, newdata = EV_test_data_nnm, level = 1)
EV_test_data_nnm$wh_pred = EV_test_data_nnm$predict_vehicle_econ * EV_test_data_nnm$distance

#total averaged out power for all vehicle in the test prediction
total_wh = function(time_month, wh) {
  usage_data = data.frame("time_month" = EV_data$time_month %>% unique, "date" = EV_data$date %>% unique)
  data = data.frame(time_month, wh)
  monthly_total = data %>%
    group_by(time_month) %>%
    summarise(total_mwh = sum(wh/1e6), no_vehicle = n())
  # summarise(total_mwh = mean(wh), no_vehicle = n())
  usage_data$total_mwh = NA
  usage_data$total_mwh[monthly_total$time_month] = monthly_total$total_mwh
  usage_data$no_vehicle = NA
  usage_data$no_vehicle[monthly_total$time_month] = monthly_total$no_vehicle
  return(usage_data)
}

usage_data2 = rbind(
  total_wh(EV_training_data$time_month, EV_training_data$wh),
  total_wh(EV_test_data_nnm$time_month, EV_test_data_nnm$wh)
) %>% na.omit
usage_data2 = cbind(usage_data2,
                    data.frame(
                      "pred_mwh" = total_wh(EV_test_data_nnm$time_month, EV_test_data_nnm$wh_pred)$total_mwh,
                      "pred_no_vehicle" = total_wh(EV_test_data_nnm$time_month, EV_test_data_nnm$wh_pred)$no_vehicle
                    )
)








# Baseline efficiency of vehicles sorted by vehicle type
#model_pop is the factor sorted by popularity matched to the the vehicle type. if any data changes this must be updated correspondingly. 
vehicle_type = data.frame("model_name" = model_pop$model, "vehicle_type" = c("hatchback", "hatchback", "hatchback", "hatchback", "van", "sedan", "hatchback", "sedan","compact SUV","hatchback","hatchback","compact SUV","compact SUV","sedan","hatchback","sedan","SUV","compact SUV","hatchback", "van","SUV","compact SUV","SUV","hatchback"))
vehicle_auck_pred = vehicle_pop
vehicle_auck_pred$HDD = 0
vehicle_auck_pred$CDD = 0 
vehicle_auck_pred$weather_region = "auckland"
vehicle_auck_pred$vehicle_pred_consum = predict(eff_hierc_grad_cor_model, newdata = vehicle_auck_pred, level = 2)
vehicle_auck_pred$model_pred_consum = predict(eff_hierc_grad_cor_model, newdata = vehicle_auck_pred, level = 1)
vehicle_auck_pred$vehicle_type = vehicle_type$vehicle_type[match(vehicle_auck_pred$model, vehicle_type$model_name)]









# Heatpump vs non-heatpump nissan leaf predictions
clyde_heat_comp_data = HDD_mon_avg[HDD_mon_avg$weather_region == "clyde", ]
clyde_heat_comp_data = cbind(clyde_heat_comp_data, CDD_mon_avg[CDD_mon_avg$weather_region == "clyde", "CDD"])
clyde_heat_comp_data$weather_region = "clyde"
clyde_heat_comp_data = clyde_heat_comp_data[rep(seq_len(nrow(clyde_heat_comp_data)), 2), ]
clyde_heat_comp_data$model =c(rep("Nissan Leaf (24 kWh) 2013-2016", 12), rep("Nissan Leaf (24 kWh) 2011-2012", 12))
clyde_heat_comp_data$pred_econ = predict(eff_hierc_grad_cor_model, newdata = clyde_heat_comp_data, level = 1)
clyde_base_comp_data = clyde_heat_comp_data
clyde_base_comp_data$HDD = 0
clyde_base_comp_data$CDD = 0
clyde_heat_comp_data$pred_base_econ = predict(eff_hierc_grad_cor_model, newdata = clyde_base_comp_data, level = 1)








# Some functions or variables used for plotting

# stylings
gg_theme = theme_bw(base_size = 14)
gg_color_palette = scale_color_brewer(palette = "Dark2")
#base r stylings
legend_inset = c(0.020, 0.036)
margins = list(mar = c(3, 4, 1, 1))

plot_decomp = function(decomp, title = "Decomposition of Time Series", xlab = "Year", ylab = "", breaks = 6) {
  Time = attributes(decomp$x)$tsp
  Time = seq(Time[1],Time[2], length.out=round((Time[2]-Time[1])*Time[3]+1))
  
  # Convert td to data frame
  dat = cbind(Time, with(decomp, data.frame(Observed=x, Trend=trend, Seasonal=seasonal, Random=random)))
  
  plot = ggplot(gather(dat, component, value, -Time), aes(Time, value)) +
    facet_grid(component ~ ., scales="free_y") +
    scale_x_continuous(n.breaks = breaks) +
    geom_line() +
    labs(y=ylab, x=xlab) +
    #    ggtitle(title)+
    theme(plot.title=element_text(hjust=0.5)) +
    gg_theme
  
  return(plot)
}








save(EV_data,
     weather_regions,
     monthly_EV_data, 
     monthly_reg_EV_data, 
     model_pop, 
     region_pop, 
     vehicle_pop, 
     HDD_data, 
     CDD_data,
     HDD_mon_avg, 
     CDD_mon_avg, 
     CDD_base_temp, 
     HDD_base_temp, 
     month_length,
     eff_lm,
     eff_hierc_grad_cor_model,
     lm_resid,
     hierc_grad_cor_m_resid,
     EV_training_data,
     EV_test_data,
     EV_test_data_nnv,
     EV_test_data_nnm,
     econ_model,
     usage_data2,
     vehicle_type,
     vehicle_auck_pred,
     clyde_heat_comp_data,
     gg_theme,
     gg_color_palette,
     legend_inset,
     margins,
     plot_decomp,
     file = "processed_data.rda")


