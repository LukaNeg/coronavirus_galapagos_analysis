###########################################
##### MASTER SCRIPT TO RULE THEM ALL ######
###########################################

source("scripts/00_functions.R")
source("scripts/01_dataclean.R")
source("scripts/02_analysis.R")

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

### Scenario 3: Logistic Growth with level off at 1 million people:
corona_pred.logist.nosp <- growth_model(max_infected=2000000, type="log", days=days.pred, CVdata=CVdata.nospike)
scenario3 <- growth_model(max_infected=2000000, type="log", days=days.pred, CVdata=CVdata.nospike, return_active=T)
points(corona_pred.logist.nosp ~ days.pred, lwd=2, type="l",col = "green")

### Scenario 4: Linear Growth:
corona_pred.lin <- growth_model(max_infected=1000000, type="lin", days=days.pred, CVdata=CVdata.nospike)
scenario4 <- growth_model(max_infected=1000000, type="lin", days=days.pred, CVdata=CVdata.nospike, return_active=T)
points(corona_pred.lin ~ days.pred, lwd=2, type="l",col = "purple")


### RUN THE SIMULATION SCENARIO 4:
#time_till_arrival <- NULL
#country_from <- NULL
for(i in 1:5){ # run for 10 times
  print(i)
  for(d in days.pred){
    infected <- corona_spread_simu(day=d, active_cases=scenario4)
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
