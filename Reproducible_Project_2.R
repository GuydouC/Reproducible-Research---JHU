library(tidyverse) # to load required package
Storm <- read_csv("Data_Science_Project/Storm.csv") # to load correct dataset
View(Storm)
names(Storm)

# to create new dataset with selected columns
data <- Storm %>%
    select(BGN_DATE, COUNTY, COUNTYNAME, STATE, EVTYPE,
           FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
str(data)
summary(data)
rm(Storm) # to remove the former dataset
data$DATE <- mdy_hms(data$BGN_DATE) # to create new column date
data$MONTH <- month(data$DATE, label = T, abbr = F) # to create new column with the months
data$YEAR <- year(data$DATE) # to extract the Year in the date

table(data$PROPDMGEXP) # to see the letters used to characterise the amount of dollar
table(data$CROPDMGEXP) # to see the letters used to characterise the amount of dollar

# To change the letters into the corresponding number 
data$CROPDMGEXP[!grepl("K|M|B", data$CROPDMGEXP, ignore.case = TRUE)] <- 0
data$PROPDMGEXP[!grepl("K|M|B", data$PROPDMGEXP, ignore.case = TRUE)] <- 0

data$PROPDMGEXP[grep("K", data$PROPDMGEXP, ignore.case = TRUE)] <- "3"
data$PROPDMGEXP[grep("M", data$PROPDMGEXP, ignore.case = TRUE)] <- "6"
data$PROPDMGEXP[grep("B", data$PROPDMGEXP, ignore.case = TRUE)] <- "9"
data$PROPDMGEXP <- as.numeric(as.character(data$PROPDMGEXP))

data$CROPDMGEXP[grep("K", data$CROPDMGEXP, ignore.case = TRUE)] <- "3"
data$CROPDMGEXP[grep("M", data$CROPDMGEXP, ignore.case = TRUE)] <- "6"
data$CROPDMGEXP[grep("B", data$CROPDMGEXP, ignore.case = TRUE)] <- "9"
data$CROPDMGEXP <- as.numeric(as.character(data$CROPDMGEXP))

data$PROPCOST <- data$PROPDMG * 10^data$PROPDMGEXP
data$CROPCOST <- data$CROPDMG * 10^data$CROPDMGEXP

sort(table(data$PROPCOST), decreasing = T)[1:20]
sort(table(data$CROPCOST), decreasing = T)[1:20]

data$EVTYPE <- tolower(data$EVTYPE) # to change EVTYPE into lower case

data$events <- data$EVTYPE
data$events <- gsub('.avalan.', 'avalanche', data$events)
data$events <- gsub('.storm.', 'storm', data$events)
data$events <- gsub('.*storm.*', 'storm', data$events)
data$events <- gsub('.*avalan.*', 'avalanche', data$events)
data$events <- gsub('.*tornado.*', 'tornado', data$events)
data$events <- gsub('.*tstm.*', 'storm', data$events)
data$events <- gsub('.*flood.*', 'flood', data$events)
data$events <- gsub('.*fire.*', 'fire', data$events)
data$events <- gsub('.*cold.*', 'cold', data$events)
data$events <- gsub('.*freez.*', 'cold', data$events)
data$events <- gsub('.*frost.*', 'cold', data$events)
data$events <- gsub('.*ice.*', 'cold', data$events)
data$events <- gsub('.*low.*', 'cold', data$events)
data$events <- gsub('.*hail.*', 'hail', data$events)
data$events <- gsub('.*dry.*', 'heat', data$events)
data$events <- gsub('.*icy.*', 'cold', data$events)
data$events <- gsub('.*summary.*', 'summary', data$events)
data$events <- gsub('.*warmth.*', 'heat', data$events)
data$events <- gsub('.*warm.*', 'heat', data$events)
data$events <- gsub('.*hurricane.*', 'hurricane', data$events)
data$events <- gsub('.*drought.*', 'heat', data$events)
data$events <- gsub('.*hot.*', 'heat', data$events)
data$events <- gsub('.*dri.*', 'heat', data$events)
data$events <- gsub('.*snow.*', 'snow', data$events)
data$events <- gsub('.*torn.*', 'tornado', data$events)
data$events <- gsub('.*volca.*', 'volcano', data$events)
data$events <- gsub('.*rain.*', 'rain', data$events)
data$events <- gsub('.*heat.*', 'heat', data$events)
data$events <- gsub('.*lightning.*', 'lightning', data$events)
data$events <- gsub('.*wint.*', 'cold', data$events)
data$events <- gsub('.*wind.*', 'wind', data$events)
data$events <- gsub('.*precipitation.*', 'rain', data$events)
data$events <- gsub('.*floo.*', 'flood', data$events)
data$events <- gsub('.*whir.*', 'wind', data$events)
data$events <- gsub('.*precip.*', 'rain', data$events)
data$events <- gsub('.*blizzard.*', 'rain', data$events)
data$events <- gsub('.*wet.*', 'wet', data$events)
data$events <- gsub('.*wat.*', 'wet', data$events)
data$events <- gsub('.*wayt.*', 'wet', data$events)
data$events <- gsub('.*urban.*', 'flood', data$events)
data$events <- gsub('.*cloud.*', 'cloud', data$events)
data$events <- gsub('.*surf.*', 'surf', data$events)
data$events <- gsub('.*fog.*', 'fog', data$events)

head(sort(table(data$events), decreasing = T), n = 15) # to see only the 15 first greatest events

data$All_Events <- 'OTHER'

# to set to other all the features less than 1000
data$All_Events[grep("storm", data$events, ignore.case = TRUE)] <- "STORM"
data$All_Events[grep("hail", data$events, ignore.case = TRUE)] <- "HAIL"
data$All_Events[grep("flood", data$events, ignore.case = TRUE)] <- "FLOOD"
data$All_Events[grep("tornado", data$events, ignore.case = TRUE)] <- "TORNADO"
data$All_Events[grep("wind", data$events, ignore.case = TRUE)] <- "WIND"
data$All_Events[grep("snow", data$events, ignore.case = TRUE)] <- "SNOW"
data$All_Events[grep("lightning", data$events, ignore.case = TRUE)] <- "LIGHTNING"
data$All_Events[grep("rain", data$events, ignore.case = TRUE)] <- "RAIN"
data$All_Events[grep("cold", data$events, ignore.case = TRUE)] <- "COLD"
data$All_Events[grep("cloud", data$events, ignore.case = TRUE)] <- "CLOUD"
data$All_Events[grep("heat", data$events, ignore.case = TRUE)] <- "HEAT"
data$All_Events[grep("fire", data$events, ignore.case = TRUE)] <- "FIRE"
data$All_Events[grep("wet", data$events, ignore.case = TRUE)] <- "WET"
data$All_Events[grep("fog", data$events, ignore.case = TRUE)] <- "FOG"
data$All_Events[grep("surf", data$events, ignore.case = TRUE)] <- "SURF"

data$events <- data$All_Events
data$All_Events <- NULL

# To create the table of the of the elements that affect the the Health
data %>% group_by(events) %>%
    summarise(Total = sum(FATALITIES + INJURIES, na.rm = T)) %>%
    mutate(Percent = round(Total / sum(Total) * 100, 3),
           Type = rep('Fatalities and Injuries')) %>%
    arrange(desc(Total))

ToPlotHealth <- data %>% group_by(events) %>%
    summarise(Total = sum(FATALITIES + INJURIES, na.rm = T)) %>%
    mutate(Percent = round(Total / sum(Total) * 100, 3),
           Type = rep('Fatalities and Injuries')) %>%
    arrange(desc(Total))
ToPlotHealth$events <- factor(ToPlotHealth$events,
                              levels = ToPlotHealth$events[order(ToPlotHealth$Percent,
                                                                 decreasing = T)])
ggplot(ToPlotHealth, aes(x = events, y = Percent, fill = events)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = paste0(round(Percent, 3), '%')), vjust = -.5) +
    labs(x = 'Health Damage', y = 'Percentage (%)') + 
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    theme_minimal() + theme(legend.position = 'none') +
    ggtitle('Events that cause the Health damage in percentage in the US \nFrom 1950 - 2011')

# To see the economic impacts per type of events
data %>% group_by(events) %>%
    summarise(Total = sum(PROPCOST + CROPCOST, na.rm = T)) %>%
    mutate(Percent = round(Total / sum(Total) * 100, 3),
           Type = rep('Economic Damage')) %>%
    arrange(desc(Total))

ToPlotEcono <- data %>% group_by(events) %>%
    summarise(Total = sum(PROPCOST + CROPCOST, na.rm = T)) %>%
    mutate(Percent = round(Total / sum(Total) * 100, 3), Type = rep('Economic Damage')) %>%
    arrange(desc(Total))


ToPlotEcono$events <- factor(ToPlotEcono$events,
                             levels = ToPlotEcono$events[order(ToPlotEcono$Percent,
                                                               decreasing = T)])
ggplot(ToPlotEcono, aes(x = events, y = Percent, fill = events)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = paste0(round(Percent, 3), '%')), vjust = -.5) +
    labs(x = 'Economic Damage', y = 'Percentage (%)') + 
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    theme_minimal() + theme(legend.position = 'none') +
    ggtitle('Events that cause the economic damage in percentage in the US \nFrom 1950 - 2011')