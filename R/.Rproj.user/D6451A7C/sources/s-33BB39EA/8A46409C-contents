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