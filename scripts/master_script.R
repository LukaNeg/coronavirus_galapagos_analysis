###########################################
##### MASTER SCRIPT TO RULE THEM ALL ######
###########################################

source("scripts/00_functions.R")
source("scripts/01_dataclean.R")
source("scripts/02_analysis.R")

### Load data:


### Run cleaning function scripts:


### Run analysis and plotting scripts:

### Then create a simulation in which each day a random sample of people is chosen from each country
### if any of the numbers selected are less than or equal to the number of corona cases in that country
### then corona has reached galapagos and restart the simulation and save that number of days.
### Each day that passes, the number of cases in each country increases based on the exponential growth
### function of the virus spread.
corona_spread_simu <- function(day=1){
  total_active <- active_cases[day]
  total_active_per_country <- projected_CV_by_country(total_active)
  month <- tolower(as.character(month(min(CV_time_data$date)+day, label=T, abbr=F)))
  daily_vis <- daily_visitors(month=month)
  
  # for each country, sample the total urban population. If any samples are less than the total active, then CV arrived to Galapagpos:
  for(i in 1:nrow(data_fin)){
    urb_pop <- data_fin$cntry_urban_pop[i]
    CV_infected <- which(sample(urb_pop, size=daily_vis[i]) <= total_active_per_country[i])
    if(length(CV_infected)>0) break #if infected, then break the loop
  }
  if(length(CV_infected)>0) return(data_fin$country[i]) else return(0)
}


#time_till_arrival <- NULL
#country_from <- NULL
for(i in 1:500){ # run for 10 times
  print(i)
  for(d in 1:200){
    infected <- corona_spread_simu(day=d)
    if(infected!=0){
      time_till_arrival <- c(time_till_arrival, d)
      country_from <- c(country_from, infected)
      break
    }
  }
}

dates_of_arrival <- min(CV_time_data$date)+time_till_arrival
hist(dates_of_arrival, breaks="weeks")
plot(density(as.numeric(dates_of_arrival-min(CV_time_data$date))))
table(country_from)

arrivals <- table(dates_of_arrival)
dates <- as.Date(names(table(dates_of_arrival)))
prob_arrival <- cumsum(arrivals)/max(cumsum(arrivals))*100

plot(prob_arrival~dates, type="l", 
     ylab="Probability of Arrival")

perc_50 <- prob_arrival[which(near(prob_arrival,50, tol=1.5))]
perc_80 <- prob_arrival[which(near(prob_arrival,80, tol=1.5))]
perc_100 <- prob_arrival[which(near(prob_arrival,100, tol=0.1))]
abline(v=as.Date(names(perc_50)))
abline(v=as.Date(names(perc_80)))
text(names(perc_50), x=as.Date(names(perc_50))-10, y=80)
text(names(perc_80), x=as.Date(names(perc_80))+10, y=20)
text("50%", x=as.Date(names(perc_50))-10, y=90)
text("80%", x=as.Date(names(perc_80))+10, y=30)


sample(3, size=2)
time_till_arrival <- time_till_arrival[-c(77:72)]

### Include any info about travel bans that might be affecting visitation rates.
