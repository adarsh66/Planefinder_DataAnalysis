################################################################################
## Brexit data munge
## Adarsh Janakiraman (Apr 2016)
################################################################################

library(dplyr)
library(ggplot2)
library(igraph)

flightsAndAiports <- readRDS(file='data/assembled_flights_join_airports.rds')
#List of EU 
EUCountries <- c('Austria','Belgium','Bulgaria','Croatia','Cyprus',
                 'Czech Republic','Denmark','Estonia','Finland','France',
                 'Germany','Greece','Hungary','Ireland','Italy','Latvia',
                 'Lithuania','Luxembourg','Malta','Netherlands','Poland',
                 'Portugal','Romania','Slovakia','Slovenia','Spain','Sweden')

flightsToEUfromUK <- filter(flightsAndAiports, Country.x=='United Kingdom' & Country.y %in% EUCountries)
flightsToWorldfromUK <- filter(flightsAndAiports, Country.x=='United Kingdom' & !(Country.y %in% EUCountries))

flightsToUKfromEU <- filter(flightsAndAiports, Country.y=='United Kingdom', Country.x %in% EUCountries)
flightsToUKfromWorld <- filter(flightsAndAiports, Country.y=='United Kingdom' & !(Country.x %in% EUCountries))

flightsToEUfromUK <- flightsToEUfromUK %>% distinct(FlightNumber)

########################################################################
## Flights grouped by Country

topEUdestinations <- flightsToEUfromUK %>% 
  group_by(Country.y) %>% 
  summarise(total.count=n()) %>% 
  arrange(desc(total.count)) %>%
  head(10)

flightsToEUfromUK.restricted <- flightsToEUfromUK %>%
          filter(Country.y %in% topEUdestinations$Country.y )%>% 
          distinct(FlightNumber)

topEUCountries.plot <- ggplot(flightsToEUfromUK.restricted, aes(x=reorder(Country.y, table(Country.y)[Country.y]))) +
                    geom_bar(fill="red", colour="black") + 
                    xlab("Country") + 
                    ylab("Number of flights")
topEUCountries.plot <- topEUCountries.plot + coord_flip()
print(topEUCountries.plot)
ggsave('plots/topEUdestCountries.png', plot = topEUCountries.plot)

########################################################################
## Flights grouped by City
topEUdestinations <- flightsToEUfromUK %>% 
  group_by(City.y) %>% 
  summarise(total.count=n()) %>% 
  arrange(desc(total.count)) %>%
  head(20)

flightsToEUfromUK.restricted <- flightsToEUfromUK %>%
  filter(City.y %in% topEUdestinations$City.y )

topEUCities.plot <- ggplot(flightsToEUfromUK.restricted, aes(x=reorder(City.y, table(City.y)[City.y]))) +
  geom_bar()
topEUCities.plot <- topEUCities.plot + coord_flip()

print(topEUCities.plot)
ggsave('plots/topEUdestCities.png', plot = topEUCities.plot)

#######################################################################
## Grouped by source and dest region -- > (UK, EU, WORLD)
flightsAndAiports['FromRegion'] = 'World'
flightsAndAiports['ToRegion'] = 'World'

#Now we update for UK
flightsAndAiports <- mutate(flightsAndAiports, 
                            FromRegion = ifelse(Country.x == 'United Kingdom', 
                            'UK', FromRegion))
flightsAndAiports <- mutate(flightsAndAiports, 
                            ToRegion = ifelse(Country.y == 'United Kingdom', 
                                                'UK', ToRegion))

#Now update the EU regions
flightsAndAiports <- mutate(flightsAndAiports, 
                            FromRegion = ifelse(Country.x %in% EUCountries, 
                                                'EU', FromRegion))
flightsAndAiports <- mutate(flightsAndAiports, 
                            ToRegion = ifelse(Country.y %in% EUCountries, 
                                              'EU', ToRegion))

uniqueFlights <- flightsAndAiports %>% distinct(FlightNumber)

regionalFlights <- uniqueFlights %>% 
                    filter(!is.na(FromRegion) & !is.na(ToRegion)) %>% 
                    group_by(FromRegion, ToRegion) %>% 
                    summarise(total.count=n()) %>%  
                    select(FromRegion, ToRegion, total.count)

E(regional.network)$color<-ifelse(E(regional.network)$total.count>3000, "blue", "red")
E(regional.network)$color<-ifelse(E(regional.network)$total.count>5000, "grey", E(regional.network)$color)

par(mai=c(0,0,1,0))
regionalFlights.plot <- plot.igraph(regional.network, edge.curved=TRUE, 
            edge.width=E(regional.network)$total.count/700, 
            vertex.label.dist=2, vertex.label.cex=1.5,)

print(regionalFlights.plot)
ggsave('plots/flightnetwork.png', plot = regionalFlights.plot)
