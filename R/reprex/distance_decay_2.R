library(tidyverse)
library(pct)
library(sf)

###### 1. GET THE DATA - START ######
flow <- pct::get_pct(region = "london", layer = "rf")

###### 1. GET THE DATA - END ######

###### 2. PROBABILITY OF CYCLING (GLM) - START ######
#copy the data to another df and keep only the necessary columns
uptake <- flow %>% 
  st_drop_geometry %>%
  dplyr::select(geo_code1, geo_code2, all, bicycle, foot, rf_dist_km, rf_avslope_perc) %>%
  rename(dist = rf_dist_km, slope = rf_avslope_perc)
# save it for next time
write_csv(uptake, path = "reprex/flows_london_decay.csv")
#uptake <- read_csv("reprex/flows_london_decay.csv")

# get % of cyclists
uptake$perc_cycle <- uptake$bicycle / uptake$all
# use this df for glm, as intra flows are all assigned distance 0 and so affect the results
uptake_no_intra <- uptake %>% dplyr::filter(geo_code1 != geo_code2)

# sqrt to get bell shape!  https://itsleeds.github.io/pct/reference/uptake_pct_govtarget.html
glm1 <- glm(perc_cycle ~ dist + sqrt(dist) + slope, 
            data = uptake_no_intra, family = "quasibinomial")


# If coefficient (logit) is positive, the effect of this predictor on cycling is positive and vice versa
#coeff <- coef(glm1) %>% as.data.frame() 
summary(glm1)
#summary(glm2)

# predict cycling probability on all OD pairs, including those with distance = 0
uptake$prob_cycle <- predict(glm1, data.frame(dist = uptake$dist, slope = uptake$slope), type = "response")

# get goodness of fit
rsq  <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}

rsq(uptake$perc_cycle,uptake$prob_cycle)
###### 2. PROBABILITY OF CYCLING (GLM) - END ######


###### 3. DISTRIBUTE ADDITIONAL FLOWS - START ######

# what is the current cycling mode share
cycle_current <- sum(uptake$bicycle) / sum(uptake$all)
#get it as a %
cycle_current*100

# Let's assume we want cycling mode share to increase to 20%
cycle_target <- 0.2
# no. of additional cycling trips needed to acheive target
cycle_add <- round((cycle_target * sum(uptake$all)) - sum(uptake$bicycle))
cycle_add
# this column is the pool out of which some fraction will be converted to cyclists 
# we don't imagine a transition from walking to cycling
uptake$non_active <- uptake$all - (uptake$bicycle + uptake$foot)

# this would be the additional number of cyclists if we did not have a target to calibrate to
# it is basically the probability from the glm * non_active commuters
uptake$cycle_added_unweighted <- uptake$prob_cycle * uptake$non_active

# But we need to adjust these values so that the additional cyclists = cycle_add
# We solve for X (multiply_factor):
# the idea is that cycle_added_unweighted(1)*X + cycle_added_unweighted(2) *X ..... = Additional Cycling Trips (cycle_add)
multiply_factor <- cycle_add / sum(uptake$cycle_added_unweighted)
multiply_factor
# Get the additional number of trips for each OD pair using multiplication factor found above
uptake$cycling_increase <- (uptake$cycle_added_unweighted) * multiply_factor
# Add additional trips to current trips to get future demand
uptake$potential_demand <- round(uptake$cycling_increase) + uptake$bicycle

# lets see if any of the potential demand values are above the total flow
uptake$cycle_fraction = uptake$potential_demand / uptake$all
# Ideally, all values should be between 0 and 1
min(uptake$cycle_fraction) 
max(uptake$cycle_fraction) 

# mode share of potential_demand column should = cycle_target (20%)
sum(uptake$potential_demand) / sum(uptake$all)

###### 3. DISTRIBUTE ADDITIONAL FLOWS - END ######


###### 4. VISUALIING RESULTS ######

# get data in long format for ggplot
uptake %>% 
  select(dist, perc_cycle, cycle_fraction) %>%
  mutate(perc_cycle = perc_cycle*100,
         cycle_fraction = cycle_fraction*100) %>%
  pivot_longer(cols = c(perc_cycle, cycle_fraction)) -> p

# plot
ggplot(p, aes(x= dist, y=value, color = name)) +
  geom_smooth() + 
  labs(title = paste0('Distribution of Cycling Increase \nif Mode Share Reaches ', round(cycle_target*100), '%'),
       x="Commuting Distance (km)", y = "Cycling Mode Share (%)") + 
  scale_color_manual(name = "", labels = c("Potential", "Current"), values=c("darkgreen", "darkred")) +
  theme_minimal()


