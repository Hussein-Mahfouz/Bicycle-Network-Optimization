library(sf)

graph_sf <- readRDS(paste0("../data/", chosen_city, "/graph_with_flows_default_communities.RDS"))





x <- graph_sf
#get edge_id of edge with highest flow
edge_sel <- x$edge_id[which.max(x$flow)]

# Group by community and get the edge with the highest flow in each group
t <- x %>% group_by(Community) %>% top_n(1, flow)
# above might return more than one edge per group (edges tied for highest flow), so here we group the 
# result by Community and select the longer edge
t2 <- t %>% group_by(Community) %>% top_n(1, d) %>%
  dplyr::mutate(sequence = 0)

t2 <- x %>% group_by(Community) %>% 
  top_n(1, flow) %>% 
  select(edge_id)


split <- x %>%
  group_split(Community)

split_4 <- split[[4]] %>% select(from_id)

edge_sel <- split[[4]]$edge_id[which.max(split[[4]]$flow)]

x2 <- x %>% filter(edge_id == edge_sel) 

t2 <- rbind(t2, x2)

for (i in 1:length(split)){
  edge_sel <- split[[i]]$edge_id[which.max(split[[i]]$flow)]
  x2 <- x %>% filter(edge_id == edge_sel)  %>% mutate(sequence = 1)
  t2 <- rbind(t2, x2)
}






neighb_id <- graph_sf$edge_id[which(graph_sf$from_id %in% t$from_id | 
                                      graph_sf$from_id %in% t$to_id |
                                      graph_sf$to_id %in% t$from_id | 
                                      graph_sf$to_id %in% t$to_id)]



################################################ FUNCTION 1 ################################################
###########################################################################################################  
### THIS FUNCTION IDENTIFIES THE EDGE WITH THE HIGHEST FLOW IN EACH COMMUNITY. THESE EDGES ACT AS ###
### SEEDS SO THAT THE GRAPH CAN GROW FROM MULTIPLE LOCATIONS, NOT JUST ONE INITIAL EDGE. WHILE ONE ###
### SEED IS CHOSEN FROM EACH COMMUNITY, THERE IS NO REQUIREMENT FOR THE EDGES APPENDED LATER TO BELONG ###
### TO A SPECIFIC COMMUNITY. IE WE DO NOT CONTROL FOR THE NUMBER OF EDGES ADDED FROM EACH COMMUNITY. ###
###########################################################################################################  


# 1. select investment length (km)
# 2. Identify edge with highest flow in each community
# 3. Add these edges to solution
# 4. Identify all edges that are connected to the current solution
# 5. Select edge with highest flow and append it to the solution
# 6. Repeat steps 4 & 5 until the length of the edges in the solution reaches the investment length
###############################################################

growth_community <- function(graph, km, col_name) {
  # copy of graph to edit
  x <- graph
  # Group by community and get the edge with the highest flow in each group
  x <- x %>% group_by(Community) %>% top_n(1, flow)
  # above might return more than one edge per group (edges tied for highest flow), so here we group the 
  # result by Community and select the longer edge
  x <- x %>% group_by(Community) %>% top_n(1, d) %>%
    dplyr::mutate(sequen = 0)
  # i keeps track of which iteration a chosen edge was added in
  i <- 1
  # j counts km added. We don't count segments that already have cycling infrastructure
  j <- sum(x$d) - sum(x$cycle_infra * x$d)

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
      mutate(sequen = i)
    # append it to the solution
    x <- rbind(x, edge_next)
    
    i = i+1
    # Only count length of selected edges that have no cycling infrastructure.
    # if condition is not met, j will not be changed in this iteration

    j = j + (edge_next$d - (edge_next$d * edge_next$cycle_infra))
  }
  return(x)
}


test <- growth_community(graph_sf, 20, "flow")

plot(st_geometry(graph_sf), col = 'lightgrey')
plot(test["sequen"], add = TRUE)

#dplyr::filter isn't working so filtering with base r
test2 <- test[test$sequen <= 30,]
plot(test2["sequen"])
     
plot(st_geometry(graph_sf), col = 'lightgrey')
plot(test2["sequen"], add = TRUE)


################################################ FUNCTION 2 ################################################

