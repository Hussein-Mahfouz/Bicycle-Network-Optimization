# function to get internal flows within a specified city. 
# returns rows where origin and destination match the specified city name
# works on 'flows' dataframe in '1_flow_data'

flows_internal <- function(name) {
  x <- flows %>% filter(city_origin == name, city_dest == name)
  return(x)
}

# function to filter MSOAs that are within a certain city
# only runs on the city_names dataframe in '1_flow_data'
msoas_city <- function(name) {
  x <- city_names %>% filter(city == name)
  return(x)
}


# function to split c(lat, lon) to two seperate columns  FROM JM London (https://github.com/r-spatial/sf/issues/231)
# lat = Y lon = X
split_lon_lat <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


# get compact bounding box with buffer. CRS transformation is specific to UK. It needs to be modified so 
# that user can input a CRS that is appropriate to study area

###
# bb <- osmdata::getbb ("oxford uk", format_out = "polygon")
# Result of above query can be passed directly to streetnet, but we want to add a buffer. 
# Buffers can be added using expand in `dodgr_streetnet("oxford uk", expand = 0.05)`. the problem with this 
# is that it gets a rectangular bb. I want something more compact (i.e buffer around city boundary). This is 
# done to deal with RAM limitations
###

bb_buffer <- function(city, buffer){
  x <- osmdata::getbb (city, format_out = "sf_polygon") %>% 
    sf::st_transform(crs = 27700) %>%  # change crs to one that uses metres
    sf::st_buffer(dist = buffer) %>%     # add buffer in metres
    sf::st_transform(crs = 4326) %>%   # get original crs
    sf::st_coordinates()               # get coordinates since dodgr_streetnet takes matrix argument
  return(x)
}

