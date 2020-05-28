library(dplyr)
library(readr)
library(tidyr)

# this is a lookup table matching MSOAs to major towns and cities
city_names <- read_csv('../data-raw/Middle_Layer_Super_Output_Area__2011__to_Major_Towns_and_Cities__December_2015__Lookup_in_England_and_Wales.csv') 
# change column name
city_names <- city_names %>% rename(city = TCITY15NM)

# flow data from the 2011 census https://www.nomisweb.co.uk/census/2011/bulk/rOD1
flows <- read_csv('../data-raw/flow_data.csv')

###############
# MERGING NAMES WITH FLOW DATA (TO GET INTERNAL FLOWS IN ANY CITY)
###############

# add a column with the city name corresponding to each Residence MSOA
flows <- flows %>% left_join(city_names[,c("MSOA11CD", "city")],
                             by = c("Area of residence" = "MSOA11CD")) %>%
                   rename(city_origin = city) # rename column so that we know it is referring to the 'Area of residence'
  
# add a column with the city name corresponding to each Workplace MSOA
flows <- flows %>% left_join(city_names[,c("MSOA11CD", "city")],
                             by = c("Area of workplace" = "MSOA11CD")) %>%
                   rename(city_dest = city) # rename column so that we know it is referring to the 'Area of workplace'

# Subset flows to keep only those that are within a specific city
# function to return rows where origin and destination match the specified city name 

flows_internal <- function(name) {
  x <- flows %>% filter(city_origin == name, city_dest == name)
  return(x)
}

# use function to get flows between all MSOAs in London
flows_london <- flows_internal("London")

# save as csv to use in next step
write_csv(flows_london, path = "../data/flows_london.csv")

# remove variables from global environment
rm(flows, flows_london)


