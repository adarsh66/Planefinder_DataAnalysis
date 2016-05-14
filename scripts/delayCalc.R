################################################################################
## Measuring takeoff and delays in flights take off from each airport
## Adarsh Janakiraman (Jan 2016)
################################################################################


library(dplyr)
library(ggplot2)
library(lubridate)
library(waffle)

theme_stripped <- theme(panel.background = element_blank(),
                        panel.grid = element_blank())

colNames  <- c("AirportID", "AirportName", "City","Country","Code", "ICAO", "Latitude", "Longitude", "Altitude", "Timezone", "DST", "TzName")
airports   <- read.csv("data/airports.dat", header = FALSE, col.names=colNames)

colNames <- c("AirlineID", "AirlineName","Alias","IATA","AirlineICAO", "Callsign","AirlineCountry","Active")
airlines <- read.csv("data/airlines.dat", header=FALSE, col.names = colNames)


flights <- readRDS("data/assembled_flights.rds")
flights$time <- as.POSIXct(flights$mtime, origin = "1969-12-31 23:00:10")
names(flights) <- sub(" ", "", names(flights))
flights$destination  <- substr(flights$Route,5,7)
flights$origin <- substr(flights$Route,1,3)
flights$IATA <- substr(flights$FlightNumber, 1,2)

#joining flights and airports data
flightsAndAiports <- left_join(flights, airports, by =c("origin" = "Code"))

#join it with the Airline data too
flightsAndAiports <- left_join(flightsAndAiports, airlines, by =c("IATA" = "IATA"))

#filtering for UK only flights (using geometrics)
#flightsUK <- filter(flights, Latitude  < 58 & Latitude  > 50 &
#                      Longitude < 2  & Longitude > -10)

#filtering by Country code after airports join (seems more accurate. smaller db)
flightsUK <- filter(flightsAndAiports, Country == 'United Kingdom')


altThreshold1 <- 100
altThreshold2 <- 5
# need to normalise the times
# and include only flights which are taking off (alt[1] <100)
takeOff <- flightsUK %>%
  group_by(FlightNumber) %>%
  mutate(ntime = mtime - min(mtime)) %>%
  filter(Altitude.x[1] < altThreshold1 & ntime < 4000)

# takeOff <- takeOff %>% 
#   filter(Altitude > altThreshold2) %>%
#   mutate(flightDelay = ntime - min(ntime))
  

FlightNumbers <- unique(takeOff$FlightNumber)
takeOff %>%
  filter(FlightNumber == FlightNumbers[1]) %>%
  select(matches("time"), Altitude.x, ntime) %>%
  head

delayInfo <- takeOff %>% 
      filter(Altitude.x >0) %>% 
      filter(ntime==min(ntime)) %>% 
      select(ntime, Altitude.x, time, Country.x, City.x, AirportName.x, Type, AirlineName, AirlineCountry)

hist(delayInfo$ntime/60, breaks=50, col = "red", xlab = "Delay time (minutes)", main = "")

plotdelayhist <- ggplot(delayInfo, aes(x=ntime/60))   + geom_histogram(bins = 30, fill="red", colour="black")  + scale_x_continuous("Delay time (mins)", breaks=c(10,20,30,40,50)) 
delayfilename <- "plots/delayHist.png"
ggsave(delayfilename, plot = plotdelayhist )

#Binning the time of the day by hour to calc avg delay time for ever hour
delayInfo$timebin <- cut.POSIXt(delayInfo$time,"hour")

#Time of day vs. take off time
delayScatter <- ggplot(delayInfo, aes(x=time, y=ntime/60)) +  geom_point(colour="red") +  geom_smooth(method="loess") + ylab("Delay time (mins)") + xlab("Time of day") 
delayfilename <- "plots/delayScatterPlot.png"
ggsave(delayfilename, plot = delayScatter )

##############################################################################
## Plot time of day vs delay by Airport
#Min number of flights on the day to qualify for the plot
airportMinThreshold <- 10
qualifiedAirports <- delayInfo %>% 
  group_by(AirportName) %>% 
  summarise(total.count=n()) %>% 
  filter(total.count >airportMinThreshold)

delayInfoRestricted <- delayInfo %>%
  filter(AirportName %in% qualifiedAirports$AirportName)

delayByTimePlot <- ggplot(delayInfoRestricted, aes(x=timebin, y=ntime)) 
delayByTimePlot <- delayByTimePlot +  geom_boxplot()

print(delayByTimePlot)

delayByTimeLine <-  ggplot(delayInfoRestricted, aes(x=timebin, y=ntime, group=AirportName, color=AirportName)) + stat_summary(fun.y="median", geom="line")
print(delayByTimeLine)

###############################################################################
## Plot take off time (delay) by Airport 
## Top N airports considered for the plot now

#Min number of flights on the day to qualify for the plot
airportMinThreshold <- 10
qualifiedAirports <- delayInfo %>% 
  group_by(AirportName) %>% 
  summarise(total.count=n()) %>% 
  filter(total.count >airportMinThreshold)

#Top 10 airports included in the analysis
#topAirports <- 10
#qualifiedAirports <- delayInfo %>% 
#  group_by(AirportName) %>% 
#  summarise(total.count=n()) %>% 
#  arrange(desc(total.count)) %>%
#  head(topAirports)

delayInfoRestricted <- delayInfo %>%
    filter(AirportName %in% qualifiedAirports$AirportName)

#Take off time (delay) vs. Airport name
delaysByAirportPlot <- ggplot(delayInfoRestricted, aes(x= reorder(AirportName, ntime, median, order=TRUE) , y=ntime/60)) +  geom_violin(fill="grey80") + stat_summary(fun.y = "median", geom="point", colour="red")
delaysByAirportPlot <- delaysByAirportPlot + 
                        theme(text = element_text(size=10),
                        axis.text.x = element_text(angle=90, vjust=1))
delaysByAirportPlot <- delaysByAirportPlot + scale_y_continuous(name="Delay time (mins)", breaks=c(10,20,30,40,50)) + scale_x_discrete(name="")
delaysByAirportPlot <- delaysByAirportPlot + coord_flip()
#Save the plot to file
print(delaysByAirportPlot)
delayfilename <- "plots/delaysByAirport_UK.png"
ggsave(delayfilename, plot = delaysByAirportPlot )

###############################################################################

###############################################################################
## Plot take off time by Aircraft type
## Top N aicraft types included

#Top 10 aircraft Types included in the analysis
topAircrafts <- 10
qualifiedTypes <- delayInfo %>% 
  group_by(Type) %>% 
  summarise(total.count=n()) %>% 
  arrange(desc(total.count)) %>%
  head(topAircrafts)

delayInfoRestricted <- delayInfo %>%
  filter(Type %in% qualifiedTypes$Type)

#Take off time (delay) vs. Airport name
delaysByTypePlot <- ggplot(delayInfoRestricted, aes(x=Type, y=ntime)) +
  geom_violin() + theme_stripped
#print(delaysByTypePlot)
#Save the plot to file
delayfilename <- "plots/delaysByType_UK.pdf"
ggsave(delayfilename, plot = delaysByTypePlot )
###############################################################################

###############################################################################
## My own function
quantile_75 <- function(x) {
  quantile(x, probs=0.75)
}


###############################################################################
## Plot take off time (delay) by AirlineName
## Top N airline groups included
## Sort by median delay time

#Top 10 aircraft Types included in the analysis
topAirlines <- 10
qualifiedTypes <- delayInfo %>% 
  group_by(AirlineName) %>% 
  summarise(total.count=n()) %>% 
  arrange(desc(total.count)) %>%
  head(topAirlines)

delayInfoRestricted <- delayInfo %>%
  filter(AirlineName %in% qualifiedTypes$AirlineName)

#Take off time (delay) vs. Airport name
delaysByAirlinePlot <- ggplot(delayInfoRestricted, aes(x=reorder(AirlineName, ntime, median, order=TRUE), y=ntime/60)) +
  geom_violin(fill="grey80") + stat_summary(fun.y="median", geom="point", colour="red")
delaysByAirlinePlot <- delaysByAirlinePlot + theme(text = element_text(size=10),
                        axis.text.x = element_text(angle=90, vjust=1))
delaysByAirlinePlot <- delaysByAirlinePlot + scale_y_continuous(name="Delay time (mins)", breaks=c(10,20,30,40,50)) + scale_x_discrete(name="")
delaysByAirlinePlot <- delaysByAirlinePlot + coord_flip()
print(delaysByAirlinePlot)
#Save the plot to file
delayfilename <- "plots/delaysByAirline_UK.png"
ggsave(delayfilename, plot = delaysByAirlinePlot )
###############################################################################

###############################################################################
#Showing the waffle chart for the top 8 airports in the UK. 70 was found to 
#be the threshold for top 8 (manually found this out).
airportMinThreshold <- 50
qualifiedAirports <- delayInfo %>% 
  group_by(AirportName.x) %>% 
  summarise(total.count=n()) %>% 
  filter(total.count >airportMinThreshold)

#rprepares the vector required for the waffle chart
parts <- as.vector(t(qualifiedAirports$total.count))
names(parts) <- as.vector(t(qualifiedAirports$AirportName))
parts <- sort(parts, decreasing = TRUE)
#dividing by 50 to scale it up. this function doesnt handle scale too well
airportTakeOffWaffle <- waffle(parts/50, rows=3)
print(airportTakeOffWaffle)
filename <- "plots/airportTakeOffWaffleChart.png"
ggsave(filename, plot = airportTakeOffWaffle )

###############################################################################
## Scatter plot of number flights vs Avg delay time per airport

#Min number of flights on the day to qualify for the plot
airportMinThreshold <- 10
qualifiedAirports <- delayInfo %>% 
  group_by(AirportName) %>% 
  summarise(total.count=n()) %>% 
  filter(total.count >airportMinThreshold)

delayInfoRestricted <- delayInfo %>%
  filter(AirportName %in% qualifiedAirports$AirportName)

delayInfoRestricted <- delayInfoRestricted  %>% 
          group_by(AirportName) %>% 
          summarise(numflights=n(), avgntime=mean(ntime)) 

delayByAirportScatter <- ggplot(delayInfoRestricted, aes(x=numflights, y=avgntime/60)) +
  geom_point(colour="red", size=5) +
  geom_smooth(method="lm") + 
  xlab("Number of flights (Log scale)") + ylab("Avg delay time (mins)") +
  scale_x_continuous(trans = "log") + 
  geom_text(aes(label=AirportName), nudge_x = -0.25, nudge_y = 0.5)
print(delayByAirportScatter)
filename <- "plots/delayByAirportScatter.png"
ggsave(filename, plot = delayByAirportScatter )
