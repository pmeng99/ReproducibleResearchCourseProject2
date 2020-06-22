library(ggplot2)
library(dplyr)
library(lattice)
library(stringr)
library(scales)
library(gridExtra)

if (!file.exists("StormData.csv.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "StormData.csv.bz2", method = "curl")
}



##
## read in the raw data
##
sd <- read.csv("StormData.csv.bz2", header = TRUE, na.strings = "", colClasses = c(
  rep("NULL", 5), "character", "character", "factor", rep("NULL", 14), "double", "double", "double", "NULL", "double", rep("NULL", 10))) 

##
## Fatalities by Event Type
##

## by state
fatalities_state <- sd %>% 
  group_by(STATE, EVTYPE) %>% 
  summarise(Total.Fatality = sum(FATALITIES, na.rm = TRUE)) %>% 
  filter(Total.Fatality > 0) %>%
  group_by(STATE) %>%
  arrange(STATE, desc(Total.Fatality)) %>%
  mutate(Percentage = percent(Total.Fatality / sum(Total.Fatality)), Cumulative.Percentage = percent(cumsum(Total.Fatality) / sum(Total.Fatality)))

splitted_by_state <- split(fatalities_state, fatalities_state$STATE)
for (splitted in  splitted_by_state) {
  print(splitted)
}

ggplot(fatalities_state, aes(x = reorder(EVTYPE, -total), y = total)) + 
  geom_bar(stat="identity") + xlab("Event") + ylab("Fatality") + ggtitle("Fatality by Event") +
  geom_text(stat = 'identity', aes(label = percentage, vjust = -0.2)) +
  facet_grid(. ~ STATE)


## US as a whole
total_fatalities = sum(sd$FATALITIES)
fatalities_us <- sd %>% 
  group_by(EVTYPE) %>% 
  summarise(total = sum(FATALITIES, na.rm = TRUE)) %>% 
  arrange(desc(total)) %>% 
  mutate(cumulative_percentage = cumsum(total) / total_fatalities, percentage = percent(total / total_fatalities)) %>%
  filter(cumulative_percentage < .75)
  
g1 <- ggplot(fatalities_us, aes(x = reorder(EVTYPE, -total), y = total)) + 
  geom_bar(stat="identity") + xlab("Event") + ylab("Fatality") + ggtitle("Fatality by Event") +
  geom_text(stat = 'identity', aes(label = percentage, vjust = -0.2))

##
## Injuries by Event Type
##

## US as a whole
total_inguries = sum(sd$INJURIES)
injuries_by_type <- sd %>% 
  group_by(EVTYPE) %>% 
  summarise(total = sum(INJURIES, na.rm = TRUE)) %>% 
  arrange(desc(total)) %>% 
  mutate(cumulative_percentage = cumsum(total) / total_inguries, percentage = percent(total / total_inguries)) %>%
  filter(cumulative_percentage < .85)

g2 <- ggplot(injuries_by_type, aes(x = reorder(EVTYPE, -total), y = total)) + 
  geom_bar(stat="identity") + xlab("Event") + ylab("Injury") + ggtitle("Injury by Event") +
  geom_text(stat = 'identity',aes(label = percentage, vjust = -0.2))

grid.arrange(g1, g2, nrow = 2)

#
# Property Damage by Event Type
#
total_damage <- sum(sd$PROPDMG, na.rm = TRUE) + sum(sd$CROPDMG, na.rm = TRUE)
damage_by_type <- sd %>% 
  group_by(EVTYPE) %>% 
  summarise(total = sum(PROPDMG, na.rm = TRUE) + sum(CROPDMG, na.rm = TRUE)) %>% 
  arrange(desc(total)) %>% 
  mutate(cumulative_percentage = cumsum(total) / total_damage, percentage = percent(total / total_damage)) %>%
  filter(cumulative_percentage < .85)

g3 <- ggplot(damage_by_type, aes(x = reorder(EVTYPE, -total), y = total))  
g3 + geom_bar(stat="identity") + xlab("Event") + ylab("Damage") + ggtitle("Damage by Event") +
  geom_text(stat = 'identity', aes(label = percentage, vjust = -0.2))

