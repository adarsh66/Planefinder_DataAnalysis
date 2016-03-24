################################################################################
## Measuring cruising altitude for different flights
## Adarsh Janakiraman (Jan 2016)
################################################################################

library(dplyr)
library(ggplot2)
library(lubridate)


theme_stripped <- theme(panel.background = element_blank(),
                        panel.grid = element_blank())

colNames  <- c("AirportID", "AirportName", "City","Country","Code", "ICAO", "Latitude", "Longitude", "Altitude", "Timezone", "DST", "TzName")
airports   <- read.csv("data/airports.dat", header = FALSE, col.names=colNames)

colNames <- c("AirlineID", "AirlineName","Alias","IATA","AirlineICAO", "Callsign","AirlineCountry","Active")
airlines <- read.csv("data/airlines.dat", header=FALSE, col.names = colNames)


flights <- readRDS("data/assembled_flights.rds")
flights$time <- as.POSIXct(flights$mtime, origin = "1969-12-31 23:00:10")
names(flights) <- sub(" ", "", names(flights))

#joining flights and airports data
flightsAndAiports <- left_join(flights, airports, by =c("origin" = "Code"))

#join it with the Airline data too
flightsAndAiports <- left_join(flightsAndAiports, airlines, by =c("IATA" = "IATA"))

#filtering by Country code after airports join (seems more accurate. smaller db)
flightsUK <- filter(flightsAndAiports, Country == 'United Kingdom')

#filtering to extract the max(Altitude) achieved by the flight. 
#Assumption from viewing the graphs is that most flights cruising altitude is around
# 5% of the max altitude. Not observed eratic behavior of occupying multiple cruising alts
cruisingAlt <- flightsUK %>%
  group_by(FlightNumber) %>%
  mutate(ntime = mtime - min(mtime)) %>%
  mutate(maxAlt = max(Altitude.x)) %>%
  filter(Altitude.x == maxAlt)

#############################################################################
## Histogram of the cruising altitudes 
hist(cruisingAlt$Altitude.x, breaks=1000)
## Shows that there are some narrow bands under which majority of the flights
## cruise around


#############################################################################
## cruising Altitude by type of flight . 
cruisingAltPlt <- ggplot(cruisingAlt, aes(x=Type, y=maxAlt)) +  geom_boxplot()
cruisingAltPlt <- cruisingAltPlt + theme(text = element_text(size=10),
                  axis.text.x = element_text(angle=90, vjust=1))
print(cruisingAltPlt)
filename <- "plots/cruisingAltByType_UK.pdf"
ggsave(filename, plot = cruisingAltPlt )


##############################################################################
## cruising Altitude by start of journey
cruisingAltPlt <- ggplot(cruisingAlt, aes(x=origin, y=maxAlt)) +  geom_boxplot()
cruisingAltPlt <- cruisingAltPlt + theme(text = element_text(size=10),
                                         axis.text.x = element_text(angle=90, vjust=1))
print(cruisingAltPlt)
filename <- "plots/cruisingAltByType_UK.pdf"
ggsave(filename, plot = cruisingAltPlt )