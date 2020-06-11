library(tidyverse)
library(pct)
library(sf)

###### 1. GET THE DATA - START ######
flow <- pct::get_pct(region = "london", layer = "rf")
###### 1. GET THE DATA - END ######

###### 2. PROBABILITY OF CYCLING (GLM) - START ######
#copy the data to another df and keep only the necessary columns
uptake_decay <- flow %>% 
  st_drop_geometry %>%
  dplyr::select(geo_code1, geo_code2, all, bicycle, foot, rf_dist_km, rf_avslope_perc) %>%
  rename(dist = rf_dist_km, slope = rf_avslope_perc)
# save it for next time
write_csv(uptake_decay, path = "reprex/flows_london_decay.csv")

# get % of cyclists
uptake_decay$perc_cycle <- uptake_decay$bicycle / uptake_decay$all
# use this df for glm, as intra flows are all assigned distance 0 and so affect the results
uptake_no_intra <- uptake_decay %>% dplyr::filter(geo_code1 != geo_code2)

# sqrt to get bell shape!  https://itsleeds.github.io/pct/reference/uptake_pct_govtarget.html
glm1 <- glm(perc_cycle ~ dist + sqrt(dist) + slope, 
            data = uptake_no_intra, family = "quasibinomial")
# use destination as an additional predictor
#glm2 <- glm(perc_cycle ~ dist + sqrt(dist) + slope + geo_code2, 
#            data = uptake_no_intra, family = "quasibinomial")

# If coefficient (logit) is positive, the effect of this predictor on cycling is positive and vice versa
#coeff <- coef(glm1) %>% as.data.frame() 
summary(glm1)
#summary(glm2)
# predict cycling probability on all OD pairs
uptake_decay$prob_cycle <- predict(glm1, data.frame(dist = uptake_decay$dist, slope = uptake_decay$slope), type = "response")

# get goodness of fit
rsq  <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}

rsq(uptake_decay$perc_cycle,uptake_decay$prob_cycle)

###### 2. PROBABILITY OF CYCLING (GLM) - END ######

###### 3. DISTRIBUTE ADDITIONAL FLOWS - START ######

# what is the current proportion of cyclists
cycle_current <- sum(uptake_decay$bicycle) / sum(uptake_decay$all)
# Let's assume we want cycling mode share to increase to 20%
cycle_target <- 0.2
# no. of additional cycling trips needed to acheive target
cycle_add <- round((cycle_target * sum(uptake_decay$all)) - sum(uptake_decay$bicycle))

# this column is the pool out of which some fraction will be converted to cyclists
uptake_decay$non_active <- uptake_decay$all - (uptake_decay$bicycle + uptake_decay$foot)

# this would be the additional number of cyclists if we did not have a target to calibrate to
# it is basically the probability from the glm * non_active commuters
uptake_decay$cycle_added_unweighted <- uptake_decay$prob_cycle * uptake_decay$non_active

# But we need to adjust these values so that the additional cyclists = cycle_add
# We solve for X (multiply_factor):
# the idea is that cycle_added_unweighted(1)*X + cycle_added_unweighted(2) *X ..... = Additional Cycling Trips (cycle_add)
multiply_factor <- cycle_add / sum(uptake_decay$cycle_added_unweighted)
# Get the additional number of trips for each OD pair using multiplication factor found above
uptake_decay$cycling_increase <- (uptake_decay$cycle_added_unweighted) * multiply_factor
# Add additional trips to current trips to get future demand
uptake_decay$potential_demand <- round(uptake_decay$cycling_increase) + uptake_decay$bicycle

# lets see if any of the potential demand values are above the total flow
uptake_decay$cycle_fraction = uptake_decay$potential_demand / uptake_decay$all
# Ideally, all values should be between 0 and 1
max(uptake_decay$cycle_fraction) 
min(uptake_decay$cycle_fraction) 

# mode share of potential_demand column should = cycle_target (20%)
sum(uptake_decay$potential_demand) / sum(uptake_decay$all)

###### 3. DISTRIBUTE ADDITIONAL FLOWS - END ######

###### 4. VISUALIING RESULTS ######

# Show how the number of additional cyclists has been distributed
ggplot(uptake_decay) +
  geom_smooth(aes(dist, potential_demand), color = 'green') +
  geom_smooth(aes(dist, bicycle), color = "red") +
  labs( x="Commuting Distance (km)", y = "No. of Cyclists")

# show the cycling mode share vs distance
ggplot(uptake_decay) +
  geom_smooth(aes(dist, perc_cycle), color = 'red') + # old mode share
  geom_smooth(aes(dist, cycle_fraction), color = "green") + # new mode share
  labs( x="Commuting Distance (km)", y = "Cycling Mode Share (%)")




