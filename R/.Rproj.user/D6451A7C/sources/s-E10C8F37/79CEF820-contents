library(sf)

graph_sf <- readRDS("../data/alt_city/graph_with_flows.RDS")
# column to prioritize by. Change later
graph_sf$flow_normalized <- graph_sf$flow / graph_sf$d_weighted

######
#GROWING A NETWORK
######

# copy of graph to edit
x <- graph_sf
#get edge_id of edge with highest flow
edge_sel <- x$edge_id[which.max(x$flow_normalized)]
# prepare row for adding to new df
x <- x %>% filter(edge_id == edge_sel) %>% 
  mutate(sequence= 0)

i <- 1
while (i < 50){
  
  # remove rows that have already been selected
  remaining <- graph_sf %>% filter(!(edge_id %in% x$edge_id))
  # identify road segments that neighbour existing selection
  neighb_id <- graph_sf$edge_id[which(graph_sf$from_id %in% x$from_id | 
                                        graph_sf$from_id %in% x$to_id |
                                        graph_sf$to_id %in% x$from_id | 
                                        graph_sf$to_id %in% x$to_id)]
  # get neighbouring edges
  neighb <- remaining %>% filter(edge_id %in% neighb_id)
  # get id of best neighboring edge
  edge_sel <- neighb$edge_id[which.max(neighb$flow_normalized)]
  # get nest neighboring edge as df row
  edge_next <- graph_sf %>% filter(edge_id == edge_sel) %>% 
    mutate(sequence= i)
  # append it to the solution
  x <- rbind(x, edge_next)
  
  i = i+1
}

plot(st_geometry(x))
plot(x["sequence"])


#FUNCTIONS

# CHOOSE NUMBER OF SEGMENTS
# inputs
# graph: the graph you want to run the growth function on
# segments: the number of road segments you want to build. 
# col_name: the column that you are choosing segments based on, passed inside ""
growth <- function(graph, segments, col_name) {
  # copy of graph to edit
  x <- graph
  #get edge_id of edge with highest flow
  edge_sel <- x$edge_id[which.max(x[[col_name]])]
  # prepare row for adding to new df
  x <- x %>% filter(edge_id == edge_sel) %>% 
    mutate(sequence= 0)
  
  i <- 1
  while (i < segments){
    # remove rows that have already been selected
    remaining <- graph %>% filter(!(edge_id %in% x$edge_id))
    # identify road segments that neighbour existing selection
    neighb_id <- graph$edge_id[which(graph$from_id %in% x$from_id | 
                                       graph$from_id %in% x$to_id |
                                       graph$to_id %in% x$from_id | 
                                       graph$to_id %in% x$to_id)]
    # get neighbouring edges
    neighb <- remaining %>% filter(edge_id %in% neighb_id)
    # get id of best neighboring edge
    edge_sel <- neighb$edge_id[which.max(neighb[[col_name]])]
    # get nest neighboring edge as df row
    edge_next <- graph %>% filter(edge_id == edge_sel) %>% 
      mutate(sequence= i)
    # append it to the solution
    x <- rbind(x, edge_next)
    
    i = i+1
  }
  return(x)
}

# check it
test <- growth(graph_sf, 75, "flow_normalized")
plot(st_geometry(test))
plot(st_geometry(graph_sf), col = 'lightgrey')
plot(test["sequence"], add = TRUE)

# CHOOSE LENGTH OF INVESTMENT (KM)
# inputs
# graph: the graph you want to run the growth function on
# km: investment threshold in km
# col_name: the column that you are choosing segments based on, passed inside ""
growth2 <- function(graph, km, col_name) {
  # copy of graph to edit
  x <- graph
  #get edge_id of edge with highest flow
  edge_sel <- x$edge_id[which.max(x[[col_name]])]
  # prepare row for adding to new df
  x <- x %>% filter(edge_id == edge_sel) %>% 
    mutate(sequence= 0)
  j <- x$d
  i <- 1
  while (j/1000 < km){
    # remove rows that have already been selected
    remaining <- graph %>% filter(!(edge_id %in% x$edge_id))
    # identify road segments that neighbour existing selection
    neighb_id <- graph$edge_id[which(graph$from_id %in% x$from_id | 
                                       graph$from_id %in% x$to_id |
                                       graph$to_id %in% x$from_id | 
                                       graph$to_id %in% x$to_id)]
    # get neighbouring edges
    neighb <- remaining %>% filter(edge_id %in% neighb_id)
    # get id of best neighboring edge
    edge_sel <- neighb$edge_id[which.max(neighb[[col_name]])]
    # get nest neighboring edge as df row
    edge_next <- graph %>% filter(edge_id == edge_sel) %>% 
      mutate(sequence= i)
    # append it to the solution
    x <- rbind(x, edge_next)
    
    i = i+1
    j = j + edge_next$d
  }
  return(x)
}

# check it
test2 <- growth2(graph_sf, 75, "flow_normalized")
plot(st_geometry(test2))
plot(test2["sequence"])
plot(st_geometry(graph_sf), col = 'lightgrey')
plot(test2["sequence"], add = TRUE)












# Understanging OSM

cycle <- graph_sf %>% dplyr::filter(highway == 'cycleway')
no_cycle <- graph_sf %>% dplyr::filter(highway != 'cycleway')

plot(st_geometry(no_cycle))
plot(st_geometry(cycle), add = TRUE, col = 'red')
int <- st_intersection(no_cycle, cycle)
plot(st_geometry(int))

int <- st_intersection(graph_sf, cycle)

weight_profiles <-dodgr::weighting_profiles$weighting_profiles %>% 
  filter(name == 'bicycle')

x <- graph_sf %>% group_by(highway) %>% 
  summarize(segments=n(), `length (m)` = sum(d))
plot(st_geometry(x[8,]))

plot(x["highway"])
