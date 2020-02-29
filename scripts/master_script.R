###########################################
##### MASTER SCRIPT TO RULE THEM ALL ######
###########################################

source("scripts/00_functions.R")
source("scripts/01_dataclean.R")
source("scripts/02_simulation_func.R")

# for profiling what takes time when running the code: https://csgillespie.github.io/efficientR/performance.html


### Load data:


### Run cleaning function scripts:


### Run analysis and plotting scripts:

### Model different growth scenarios:
days.pred <- c(1:500)
plot(CV_time_data$cases_cum~CV_time_data$date)
## Create alternative scenario in which the last growth of cases is actually not a spike:
CVdata.nospike <- mutate(CV_time_data, 
                         cases = ifelse(cases>15000, 5000, cases),
                         cases_cum = cumsum(cases))
plot(CVdata.nospike$cases_cum~days)

# (note, all of these scenarios assume there was no spike in recent cases):
plot(CVdata.nospike$cases_cum~days, pch=16)

### Scenario 1: exponential growth from current data:
corona_expon <- growth_model(type="exp", days=days.pred, CVdata=CVdata.nospike)
scenario1 <- growth_model(type="exp", days=days.pred, CVdata=CVdata.nospike, return_active=T)
points(corona_expon ~ days.pred, lwd=2, type="l",col = "red")

### Scenario 2: Logistic Growth with level off at 1 billion people:
corona_log.1bil <- growth_model(max_infected=1000000000, type="log", days=days.pred, CVdata=CVdata.nospike)
scenario2 <- growth_model(max_infected=1000000000, type="log", days=days.pred, CVdata=CVdata.nospike, return_active=T)
points(corona_log.1bil ~ days.pred, lwd=2, type="l",col = "blue")

### Scenario 3: Logistic Growth with level off at 2 million people:
corona_pred.logist.nosp <- growth_model(max_infected=1500000, type="log", days=days.pred, CVdata=CVdata.nospike)
scenario3 <- growth_model(max_infected=1500000, type="log", days=days.pred, CVdata=CVdata.nospike, return_active=T)
points(corona_pred.logist.nosp ~ days.pred, lwd=2, type="l",col = "green")

### Scenario 4: Linear Growth:
corona_pred.lin <- growth_model(max_infected=1000000, type="lin", days=days.pred, CVdata=CVdata.nospike)
scenario4 <- growth_model(max_infected=1000000, type="lin", days=days.pred, CVdata=CVdata.nospike, return_active=T)
points(corona_pred.lin ~ days.pred, lwd=2, type="l",col = "purple")


# library("profvis")
# profvis({
### RUN THE SIMULATION SCENARIO 3:
# function to run simulation:
run_simulation <- function(scenario=scenario1, title="scenario1", runs=200, no_china=F, reduce_visit=F){
  time_till_arrival <- NULL
  country_from <- NULL
  days_run <- c(1:200)
  for(i in 1:runs){ # run for 10 times
    print(i)
    for(d in days_run){
      infected <- corona_spread_simu(day=d, active_cases=scenario, no_china=no_china, reduce_visit=reduce_visit)
      if(infected!=0){
        time_till_arrival <- c(time_till_arrival, d)
        country_from <- c(country_from, infected)
        break
      }
    }
  }
  # }) # for the profiling
  
  dates_of_arrival <- min(CV_time_data$date)+time_till_arrival
  #hist(dates_of_arrival, breaks="weeks")
  #plot(density(as.numeric(dates_of_arrival-min(CV_time_data$date))))
  #table(country_from)
  
  arrivals <- table(dates_of_arrival)
  dates <- as.Date(names(table(dates_of_arrival)))
  prob_arrival <- cumsum(arrivals)/max(cumsum(arrivals))*100
  
  plot(prob_arrival~dates, type="l", 
       ylab="Probability of Arrival", main=title)
  
  perc_50 <- prob_arrival[which(prob_arrival>50)[1]]
  perc_80 <- prob_arrival[which(prob_arrival>80)[1]]
  perc_95 <- prob_arrival[which(prob_arrival>95)[1]]
  abline(v=as.Date(names(perc_50)))
  abline(v=as.Date(names(perc_80)))
  abline(v=as.Date(names(perc_95)))
  text(names(perc_50), x=as.Date(names(perc_50))-10, y=85)
  text(names(perc_80), x=as.Date(names(perc_80))+10, y=15)
  text(names(perc_95), x=as.Date(names(perc_95))+10, y=50)
  text("50%", x=as.Date(names(perc_50))-10, y=90)
  text("80%", x=as.Date(names(perc_80))+10, y=20)
  text("95%", x=as.Date(names(perc_95))+10, y=55)
  return(prob_arrival)
}

run_simulation(scenario=scenario1, title="Exp. growth of CV", runs=500, no_china=F, reduce_visit=F)
run_simulation(scenario=scenario1, title="Exp. growth of CV with half tourism", runs=500, no_china=F, reduce_visit=T)
run_simulation(scenario=scenario3, title="Log. growth levels @ 2mil", runs=500, no_china=F, reduce_visit=F)
run_simulation(scenario=scenario3, title="Log. growth levels @ 2mil (half tourism)", runs=500, no_china=F, reduce_visit=T)

run_simulation(scenario=scenario4, title="Lin. growth of CV", runs=500, no_china=F, reduce_visit=F)



### Include any info about travel bans that might be affecting visitation rates.




### Secretary of Health for Galapagos in San Cristobal: 
# Dr. Ochoa (responsible for epidemiology in galapagos)

# Contact the group of scientists in the US that study cardiovascular disease and
# seem like a contact for the Galapagos and for me to connect with 
# juan.ochoa@hoj.gob.ec
# jochoaariza@yahoo.es
# Send my number

# Create a visualization to share these results
# Shiny app

# add threshold at which coronavirus amount in the world will def get to gal


