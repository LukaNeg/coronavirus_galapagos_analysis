#####################################
######### RUN THE ANALYSES ##########
#####################################

### Create a simulation in which each day a random sample of people is chosen from each country
### if any of the numbers selected are less than or equal to the number of corona cases in that country
### then corona has reached galapagos and restart the simulation and save that number of days.
### Each day that passes, the number of cases in each country increases based on the exponential growth
### function of the virus spread.
corona_spread_simu <- function(day=1, active_cases, no_china=F, reduce_visit=F){
  #profvis({
  total_active <- active_cases[day]
  if(total_active<0) total_active <- 1
  total_active_per_country <- projected_CV_by_country(total_active)
  month <- tolower(as.character(month(min(CV_time_data$date)+day, label=T, abbr=F)))
  daily_vis <- daily_visitors(month=month)
  if(no_china) daily_vis["China"] <- 0
  if(reduce_visit) daily_vis <- round(daily_vis *0.5)
  
  # for each country, sample the total urban population. If any samples are less than the total active, then CV arrived to Galapagpos:
  for(i in 1:nrow(data_fin)){
    urb_pop <- data_fin$cntry_urban_pop[i]
    prop_infect <- total_active_per_country[i]/urb_pop
    samp <- runif(daily_vis[i], min=0,max=1)
    CV_infected <- which(samp <= prop_infect)
    
    #samp <- sample(urb_pop, size=daily_vis[i])
    #CV_infected <- which(samp <= total_active_per_country[i])

    if(length(CV_infected)>0) break #if infected, then break the loop
  }
  if(length(CV_infected)>0) return(data_fin$country[i]) else return(0)
  #})
}


