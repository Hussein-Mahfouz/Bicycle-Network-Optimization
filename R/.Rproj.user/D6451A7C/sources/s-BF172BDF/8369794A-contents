library(tidyverse)


# read in the data
flow <- readr::read_csv(paste0("../data/",chosen_city,"/flows_dist_elev_for_potential_flow.csv"))

######## 
# PLOTTING DISTANCE VS FLOW
########
#plot distance vs flow
# remove intra flows
flow_plot <- flow %>% dplyr::filter(`Area of residence` != `Area of workplace`)

# all motorized trips
flow_plot$motor <- flow_plot$`Underground, metro, light rail, tram` + flow_plot$Train +
  flow_plot$`Bus, minibus or coach` + flow_plot$Taxi + flow_plot$`Motorcycle, scooter or moped` + 
  flow_plot$`Driving a car or van` + flow_plot$`Passenger in a car or van`

# all non_motorized trips
flow_plot$active <- flow_plot$Bicycle + flow_plot$`On foot` 

# all trips by public transport or acyive modes
flow_plot$sustainable <- flow_plot$active + flow_plot$`Underground, metro, light rail, tram` + 
  flow_plot$Train + flow_plot$`Bus, minibus or coach` 

# all trips by private vehicles
flow_plot$private <- flow_plot$Taxi + flow_plot$`Motorcycle, scooter or moped` + 
  flow_plot$`Driving a car or van` + flow_plot$`Passenger in a car or van` 

# subset for histograms
flow_plot <- flow_plot %>% dplyr::select(`Area of residence`, `Area of workplace`, 
                                         `All categories: Method of travel to work`, 
                                         Bicycle, motor, active, sustainable, private, dist)

# repeat each row based on the value in Bicycle (for histogram!)
flow_long_bike <- flow_plot %>% tidyr::uncount(Bicycle) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)

ggplot(flow_long_bike, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5, binwidth = 250) +
  labs(title = "Bicycle Trips", x="Commuting Distance (km)", y = "No. of trips")

###
flow_long_motor <- flow_plot %>% tidyr::uncount(motor) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)

ggplot(flow_long_motor, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5) +
  labs(title = "All Motorized Trips", x="Commuting Distance (km)", y = "No. of trips")

### 
flow_long_active <- flow_plot %>% tidyr::uncount(active) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)

ggplot(flow_long_active, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5) +
  labs(title = "Active Trips", x="Commuting Distance (km)", y = "No. of trips")

### 
flow_long_sustainable <- flow_plot %>% tidyr::uncount(sustainable) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)

ggplot(flow_long_sustainable, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5) +
  labs(title = "Trips Made by Sustainable Modes", 
       x="Commuting Distance (km)", y = "No. of trips")

### 
flow_long_private <- flow_plot %>% tidyr::uncount(private) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)

ggplot(flow_long_private, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5) +
  labs(title = "Trips Made by Private Vehicles", 
       x="Commuting Distance (km)", y = "No. of trips")

# to plot different histograms together
histogram<- rbind(flow_long_motor, flow_long_bike, flow_long_sustainable)

ggplot(histogram, aes(x=dist)) + 
  geom_histogram(data=flow_long_motor, fill = "blue", alpha = 0.9) +
  geom_histogram(data=flow_long_sustainable, fill = "red", alpha = 0.5) +
  geom_histogram(data= flow_long_bike, fill = "green", alpha = 0.5) 

rm(flow, flow_plot, flow_long_active, flow_long_bike, flow_long_motor, flow_long_private, 
   flow_long_sustainable, histogram)
########



