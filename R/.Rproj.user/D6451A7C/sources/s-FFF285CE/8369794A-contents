library(tidyverse)


# read in the data
flow <- readr::read_csv(paste0("../data/",chosen_city,"/flows_for_desire_lines.csv"))

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
                                         Bicycle, motor, active, sustainable, private, dist, 
                                         potential_demand)

# repeat each row based on the value for 'All Categories...' for histogram
flow_long_all <- flow_plot %>% tidyr::uncount(`All categories: Method of travel to work`) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)
# convert from m to km
flow_long_all$dist <- flow_long_all$dist / 1000

ggplot(flow_long_all, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5, binwidth = 0.5) +
  labs(title = "Trips Made By All Modes", x="Commuting Distance (km)", y = "No. of trips")


# repeat each row based on the value in Bicycle (for histogram!)
flow_long_bike <- flow_plot %>% tidyr::uncount(Bicycle) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)
# convert from m to km
flow_long_bike$dist <- flow_long_bike$dist / 1000

ggplot(flow_long_bike, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5, binwidth = 0.5) +
  labs(title = "Trips Made By Bicycle", x="Commuting Distance (km)", y = "No. of trips")

ggsave(paste0("../data/", chosen_city,"/Plots/histogram_distance_cycling.png"))

###
flow_long_motor <- flow_plot %>% tidyr::uncount(motor) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)

# convert from m to km
flow_long_motor$dist <- flow_long_motor$dist / 1000

ggplot(flow_long_motor, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5) +
  labs(title = "All Motorized Trips", x="Commuting Distance (km)", y = "No. of trips")

### 
flow_long_active <- flow_plot %>% tidyr::uncount(active) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)

# convert from m to km
flow_long_active$dist <- flow_long_active$dist / 1000

ggplot(flow_long_active, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5) +
  labs(title = "Active Trips", x="Commuting Distance (km)", y = "No. of trips")

### 
flow_long_sustainable <- flow_plot %>% tidyr::uncount(sustainable) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)

# convert from m to km
flow_long_sustainable$dist <- flow_long_sustainable$dist / 1000

ggplot(flow_long_sustainable, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5) +
  labs(title = "Trips Made by Sustainable Modes", 
       x="Commuting Distance (km)", y = "No. of trips")

### 
flow_long_private <- flow_plot %>% tidyr::uncount(private) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)

# convert from m to km
flow_long_private$dist <- flow_long_private$dist / 1000

ggplot(flow_long_private, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5) +
  labs(title = "Trips Made by Private Vehicles", 
       x="Commuting Distance (km)", y = "No. of trips")


### POTENTIAL DEMAND - CALCULATED FROM SCRIPT 3

# repeat each row based on the value for 'potential demand' for histogram
flow_long_potential <- flow_plot %>% tidyr::uncount(potential_demand) %>%
  dplyr::select(`Area of residence`, `Area of workplace`, dist)
# convert from m to km
flow_long_potential$dist <- flow_long_potential$dist / 1000

ggplot(flow_long_potential, aes(x = dist)) + 
  geom_histogram(color = "black", alpha = 0.5) +
  labs(title = "Potential Cycling Demand", x="Commuting Distance (km)", y = "No. of trips")

                             ######################################
############### Plot histograms overlaying bicycle distribtion on total flow distribution ###############
                             ######################################
histogram<- rbind(flow_long_all, flow_long_bike, flow_long_potential)

### current cycling trips vs all commuter trips
cols <- c("All" = "grey60", "Bicycle (Current)" = "darkred")
ggplot(histogram, aes(x=dist)) + 
  geom_histogram(data=flow_long_all, color = 'grey50', aes(fill = "All")) +
  geom_histogram(data= flow_long_bike, color = 'grey50', aes(fill = "Bicycle (Current)")) +
  scale_fill_manual(name = "Mode", values = cols) +
  labs(title = "Cycling Trips Compared to All Trips", 
       x="Commuting Distance (km)", y = "No. of trips", color = "Legend")

ggsave(paste0("../data/", chosen_city,"/Plots/histogram_distance_all_vs_cycling.png"))

### potential cycling trips vs all commuter trips
cols <- c("All" = "grey60", "Bicycle (Potential)" = "darkgreen")
ggplot(histogram, aes(x=dist)) + 
  geom_histogram(data=flow_long_all, color = 'grey50', aes(fill = "All")) +
  geom_histogram(data= flow_long_potential, color = 'grey50', aes(fill = "Bicycle (Potential)")) +
  scale_fill_manual(name = "Mode", values = cols) +
  labs(title = "Cycling Trips Compared to All Trips", 
       x="Commuting Distance (km)", y = "No. of trips", color = "Legend") 

ggsave(paste0("../data/", chosen_city,"/Plots/histogram_distance_all_vs_cycling_potential.png"))

### current cycling trips vs potential cycling trips
cols <- c("Potential" = "darkgreen", "Current" = "darkred")
ggplot(histogram, aes(x=dist)) + 
  geom_histogram(data=flow_long_potential, color = 'grey50', aes(fill = "Potential")) +
  geom_histogram(data= flow_long_bike, color = 'grey50', aes(fill = "Current")) +
  scale_fill_manual(name = "", values = cols) +
  labs(title = "Cycling Trips", 
       x="Commuting Distance (km)", y = "No. of trips", color = "Legend") 

ggsave(paste0("../data/", chosen_city,"/Plots/histogram_distance_cycling_potential_vs_current.png"))



rm(flow, flow_plot, flow_long_all, flow_long_active, flow_long_bike, flow_long_motor, flow_long_private, 
   flow_long_sustainable, flow_long_potential, histogram, cols)
########










rm(flow, flow_plot, flow_long_active, flow_long_bike, flow_long_motor, flow_long_private, 
   flow_long_sustainable, histogram, flow_potential, flow_long_potential)
