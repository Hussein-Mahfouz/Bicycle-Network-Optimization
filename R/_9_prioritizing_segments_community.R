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
###########################################################################################################  
### THIS FUNCTION ALSO SELECTS A SEED FROM EACH COMMUNITY. IN EACH ITERATION ONE EDGE FROM EACH COMMUNITY ###
### IS CHOSEN. THE CHOSEN EDGE MUST BE CONNECTED TO THE EDGES IN THE COMMUNITY THAT HAVE ALREADY BEEN ###
### CHOSEN. IF THERE ARE NO MORE EDGES IN TA PARTICULAR COMMUNITY, THE FUNCTION SKIPS OVER THAT COMMUNITY ###
### AND CONTINUES WITH THE REMAINING COMMUNITIES
###########################################################################################################  

# 1. select investment length (km)
# 2. Identify edge with highest flow in each community
# 3. Add these edges to solution
# 4. For edges in each community
#      - identify edges that neighbor the edges in the solution from that community
#      - Select edge with highest flow and append it to the solution
# 5. Keep looping over the communities and selecting edges until you reach the investment length
# 6. If there are no neighboring edges remaining for a community, skip it

###############################################################
growth_community_2 <- function(graph, km) {
  # copy of graph to edit
  x <- graph
  # Group by community and get the edge with the highest flow in each group
  x <- x %>% group_by(Community) %>% top_n(1, flow)
  # above might return more than one edge per group (edges tied for highest flow), so here we group the 
  # result by Community and select the longer edge
  x <- x %>% group_by(Community) %>% top_n(1, d) %>%
    dplyr::mutate(sequen = 0)
  ####################
  # split the graph into a list of dataframes with length = number of communities
  split <- graph %>%
    group_split(Community)
  
  
  ####################
  # i keeps track of which iteration a chosen edge was added in
  i <- 1
  # j counts km added. We don't count segments that already have cycling infrastructure
  j <- sum(x$d) - sum(x$cycle_infra * x$d)
  
  #while length of chosen segments is less than the specified length 
  while (j/1000 < km){
    
    # for each community
    for (k in 1:length(split)){
      # edges in community k that have already been chosen 
      chosen    <- split[[k]] %>% filter((edge_id %in% x$edge_id))
      # edges in community k that have not been chosen yet
      remaining <- split[[k]] %>% filter(!(edge_id %in% x$edge_id))
      
      if (nrow(remaining) > 0){
        # all edges that neighbor the edges in the community that have already been chosen
        neighb_id <- split[[k]]$edge_id[which(split[[k]]$from_id %in% chosen$from_id | 
                                                split[[k]]$from_id %in% chosen$to_id |
                                                split[[k]]$to_id %in% chosen$from_id | 
                                                split[[k]]$to_id %in% chosen$to_id)]
        
        # filter out the remaining edges to keep only the ones that neighbor the chosen edges
        #### PROBLEM COULD BE HERE
        neighb <- remaining %>% filter(edge_id %in% neighb_id)
        # it may be the case that the remaining edges in the community are not connected to the chosen edges
        # the edges in each community do not necessarily form one component. If this is the case, then neighb will 
        # return an empty sf feature, so an if function is added to only continue if neighb is not empty
        if (nrow(neighb) > 0){
        #get the edge_id of the edge with the highest flow out of the neighb df
        edge_sel <- neighb$edge_id[which.max(neighb$flow)]
        # get nthe edge from it's edge id, and add a sequen column to show when it was added to the solution
        edge_next <- graph %>% filter(edge_id == edge_sel) %>% 
          mutate(sequen = i)
        # append edge to the solution
        x <- rbind(x, edge_next)
        # Only count length of selected edges that have no cycling infrastructure.
        j = j + (edge_next$d - (edge_next$d * edge_next$cycle_infra))
        } else{
          x <- x
          j = j
        }
      } else{
        x <- x
        j = j
      }
    }
    i = i+1
  }
  return(x)
}



test <- growth_community_2(graph_sf, 50)

test %>% st_drop_geometry %>% group_by(cycle_infra) %>% summarize(length = sum(d))

plot(st_geometry(graph_sf), col = 'lightgrey')
plot(test["Community"], add = TRUE)
plot(test["Community"])



################################################ FUNCTION 3 ################################################
###########################################################################################################  
### THIS FUNCTION IS ALMOST IDENTICAL TO FUNCTION 2, WITH ONE EXCEPTION. IN FUNCTION 2, IF THERE ARE NO ###
### REMAINING NEIGHBORS IN A COMMUNITY, WE STOP ADDING EDGES FROM THAT COMMUNITY. IN SOME CASES, THE  ###
### EDGES IN A COMMUNITY FORM MORE THAN 1 CONNECTED COMPONENT, SO THERE MAY STILL BE EDGES, EVEN THOUGH ###
### NONE OF THEM ARE CONNECTED TO THE CURRENT SOLUTION. IF THIS IS THE CASE, WE FIND THE BEST (EG. HIGHEST ###
### FLOW) REMAINING EDGE INSTEAD AND ADD IT TO THE SOLUTION
### REMAINING EDGE IN THE COMMUNITY 
###########################################################################################################  

# 1. select investment length (km)
# 2. Identify edge with highest flow in each community
# 3. Add these edges to solution
# 4. For edges in each community
#      - identify edges that neighbor the edges in the solution from that community
#      - Select edge with highest flow and append it to the solution
# 5. Keep looping over the communities and selecting edges until you reach the investment length
# 6. If there are no neighboring edges remaining for a community, check all remaing edges in the community
#    and add the best one, regardless of connectivity

###############################################################




growth_community_3 <- function(graph, km) {
  # copy of graph to edit
  x <- graph
  # Group by community and get the edge with the highest flow in each group
  x <- x %>% group_by(Community) %>% top_n(1, flow)
  # above might return more than one edge per group (edges tied for highest flow), so here we group the 
  # result by Community and select the longer edge
  x <- x %>% group_by(Community) %>% top_n(1, d) %>%
    dplyr::mutate(sequen = 0)
  ####################
  # split the graph into a list of dataframes with length = number of communities
  split <- graph %>%
    group_split(Community)
  
  
  ####################
  # i keeps track of which iteration a chosen edge was added in
  i <- 1
  # j counts km added. We don't count segments that already have cycling infrastructure
  j <- sum(x$d) - sum(x$cycle_infra * x$d)
  
  #while length of chosen segments is less than the specified length 
  while (j/1000 < km){
    
    # for each community
    for (k in 1:length(split)){
      # edges in community k that have already been chosen 
      chosen    <- split[[k]] %>% filter((edge_id %in% x$edge_id))
      # edges in community k that have not been chosen yet
      remaining <- split[[k]] %>% filter(!(edge_id %in% x$edge_id))
      
      if (nrow(remaining) > 0){
        # all edges that neighbor the edges in the community that have already been chosen
        neighb_id <- split[[k]]$edge_id[which(split[[k]]$from_id %in% chosen$from_id | 
                                                split[[k]]$from_id %in% chosen$to_id |
                                                split[[k]]$to_id %in% chosen$from_id | 
                                                split[[k]]$to_id %in% chosen$to_id)]
        
        # filter out the remaining edges to keep only the ones that neighbor the chosen edges
        #### PROBLEM COULD BE HERE
        neighb <- remaining %>% filter(edge_id %in% neighb_id)
        # it may be the case that the remaining edges in the community are not connected to the chosen edges
        # the edges in each community do not necessarily form one component. If this is the case, then neighb will 
        # return an empty sf feature, so an if function is added to only continue if neighb is not empty
        if (nrow(neighb) > 0){
          #get the edge_id of the edge with the highest flow out of the neighb df
          edge_sel <- neighb$edge_id[which.max(neighb$flow)]
          # get nthe edge from it's edge id, and add a sequen column to show when it was added to the solution
          edge_next <- graph %>% filter(edge_id == edge_sel) %>% 
            mutate(sequen = i)
          # append edge to the solution
          x <- rbind(x, edge_next)
          
          j = j + (edge_next$d - (edge_next$d * edge_next$cycle_infra))
        } else{
          #get the edge_id of the edge with the highest flow out of the neighb df
          edge_sel <- remaining$edge_id[which.max(remaining$flow)]
          # get nthe edge from it's edge id, and add a sequen column to show when it was added to the solution
          edge_next <- graph %>% filter(edge_id == edge_sel) %>% 
            mutate(sequen = i)
          # append edge to the solution
          x <- rbind(x, edge_next)
          # Only count length of selected edges that have no cycling infrastructure.
          j = j + (edge_next$d - (edge_next$d * edge_next$cycle_infra))
        }
      } else{
        x <- x
        j = j
      }
    }
    i = i+1
  }
  return(x)
}



test <- growth_community_3(graph_sf, 50)

test %>% st_drop_geometry %>% group_by(cycle_infra) %>% summarize(length = sum(d))

plot(st_geometry(graph_sf), col = 'lightgrey')
plot(test["Community"], add = TRUE)
plot(test["Community"])

