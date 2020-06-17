library(sf)

graph_sf <- readRDS(paste0("../data/", chosen_city, "/graph_with_flows_default.RDS"))
# column to prioritize by. Change later
#graph_sf$flow_normalized <- graph_sf$flow / graph_sf$d_weighted

######
#GROWING A NETWORK
######

# copy of graph to edit
x <- graph_sf
#get edge_id of edge with highest flow
edge_sel <- x$edge_id[which.max(x$flow)]
# prepare row for adding to new df
x <- x %>% filter(edge_id == edge_sel) %>% 
  mutate(sequence= 0)
# we don't want to include edges that already have cycling infrastructure in the iterator
# because they won't cost anything
if (x$cycle_infra == 0) {
  i <- 1} else {
    i <- 0}

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
  edge_sel <- neighb$edge_id[which.max(neighb$flow)]
  # get nest neighboring edge as df row
  edge_next <- graph_sf %>% filter(edge_id == edge_sel) %>% 
    mutate(sequence= i)
  # append it to the solution
  x <- rbind(x, edge_next)
  # if chosen edge already has infrastructure, don't count it
  if (edge_next$cycle_infra == 0) {
    i = i+1} 
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
  
  # we don't want to include edges that already have cycling infrastructure in the iterator
  # because they won't cost anything
  if (x$cycle_infra == 0) {
    i <- 1} else {
      i <- 0}

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
    # Only count selected edges that have no cycling infrastructure
    if (edge_next$cycle_infra == 0) {
      i = i+1} 
  }
  return(x)
}

# check it
test <- growth(graph_sf, 75, "flow")
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
  # i keeps track of which iteration a chosen edge was added in
  i <- 1
  if (x$cycle_infra == 0) {
    j <- x$d} else {
      j <- 0}
  
  #while length of chosen segments is less than specified length 
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
    # Only count length of selected edges that have no cycling infrastructure.
    # if condition is not met, j will not be changed in this iteration
    if (edge_next$cycle_infra == 0) {
      j = j + edge_next$d} 
  }
  return(x)
}

# check it
test2 <- growth2(graph_sf, 75, "flow")
plot(st_geometry(test2))
plot(test2["sequence"])
plot(st_geometry(graph_sf), col = 'lightgrey')
plot(test2["sequence"], add = TRUE)

# check that length argument was respected. length_total should be equal to passed length for 
# cycle_infra == 0
test2 %>% st_drop_geometry %>% group_by(cycle_infra) %>%
  summarize(length_total = sum(d))







