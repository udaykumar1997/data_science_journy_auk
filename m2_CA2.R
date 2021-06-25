########################################################################
## exercise: Climate Change
########################################################################

library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

str(temp_carbon)
str(greenhouse_gases)
str(historic_co2)

temp_carbon %>% .$year %>% max()

temp_carbon %>% filter(!is.na(carbon_emissions)) %>%
  pull(year) %>% max()

temp_carbon %>% filter(!is.na(carbon_emissions)) %>%
  .$year %>% max()

temp_carbon %>% filter(!is.na(carbon_emissions)) %>%
  select(year) %>% max()

temp_carbon %>% filter(!is.na(carbon_emissions)) %>%
  max(.$year)

a<-temp_carbon %>% filter(!is.na(carbon_emissions) & !is.na(year)) %>%
  select(carbon_emissions) %>% max()

temp_carbon %>% filter(!is.na(carbon_emissions) & (year==1751 | year==2014)) %>%
  select(year,carbon_emissions)

temp_carbon %>% filter(!is.na(carbon_emissions) & (year==1751 | year==2014)) %>%
  select(year,carbon_emissions)

temp_carbon %>% filter(!is.na(temp_anomaly) & !is.na(year)) %>%
  .$year %>% min()

temp_carbon %>% filter(!is.na(temp_anomaly) & !is.na(year)) %>%
  select(year,temp_anomaly)

p<-temp_carbon %>% filter(!is.na(temp_anomaly)) %>% 
  ggplot(aes(x=year,y=temp_anomaly))+
  geom_line(col="red")

p+ylab("Temperature anomaly (degrees C)")+
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")+
  geom_hline(yintercept = 0)+
  geom_line(aes(year,ocean_anomaly),col="blue")+
  geom_line(aes(year,land_anomaly),col="brown")+
  geom_vline(xintercept = 2018)

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  
co2_time <- historic_co2 %>% filter(!is.na(co2) & !is.na(!is.na(year)) & year>=-800000) %>% 
  ggplot(aes(x=year,y=co2,color=source))
co2_time + geom_line()
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


co2_time <- historic_co2 %>% filter(!is.na(co2) & !is.na(year) & year>=-800000 & year<=-775000) %>% 
  ggplot(aes(x=year,y=co2,color=source))
co2_time +geom_line()

co2_time <- historic_co2 %>% filter(!is.na(co2) & !is.na(year) & year>=-375000 & year<=-330000) %>% 
  ggplot(aes(x=year,y=co2,color=source))
co2_time +geom_line()

co2_time <- historic_co2 %>% filter(!is.na(co2) & !is.na(year) & year>=-140000 & year<=-120000) %>% 
  ggplot(aes(x=year,y=co2,color=source))
co2_time +geom_line()

co2_time <- historic_co2 %>% filter(!is.na(co2) & !is.na(year) & year>=-3000 & year<=2018) %>% 
  ggplot(aes(x=year,y=co2,color=source))
co2_time +geom_line()