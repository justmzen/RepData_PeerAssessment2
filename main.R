# Author: justmzen
# Topic: Reproducible Research - Assignment 2
# File name: dataProcessing.R
# May 2020
# 

### Data Processing

## Libraries loading
library('data.table')
library('ggplot2')

## Data loading
# Storm Data downloading
if(!file.exists('./stormData.bz2')){
        fileURL <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
        download.file(url = fileURL, destfile = './stormData.bz2')
        rm(fileURL)
        dateDownloaded <- date()
}

# Loading the data set
sd <- as.data.table(read.csv(file = './stormData.bz2'))

# Variables list
names(sd)

# Data manipulation
sd$BGN_DATE <- as.Date(sd$BGN_DATE, format = '%m/%d/%Y %H:%M:%S')


# Data set subsetting
# Filtering by date
sd <- sd[BGN_DATE >= as.Date('01/01/1996', format = '%m/%d/%Y')]

# Choosing useful variables
sd <- sd[, .(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP,
             CROPDMG, CROPDMGEXP)]

# Managing EXP values
conversionValues <- c('0' = 10^0,
                      '1' = 10^1,
                      '2' = 10^2,
                      '3' = 10^3,
                      '4' = 10^4,
                      '5' = 10^5,
                      '6' = 10^6,
                      '7' = 10^7,
                      '8' = 10^8,
                      '9' = 10^9,
                      '+' = 10^1,
                      '-' = 10^0,
                      '?' = 10^0,
                      'h' = 10^2, 'H' = 10^2,
                      'k' = 10^3, 'K' = 10^3,
                      'm' = 10^6, 'M' = 10^6,
                      'b' = 10^9, 'B' = 10^9
)

# Conversion
sd <- sd[, PROPDMGEXP := conversionValues[sd[, PROPDMGEXP]]]
sd <- sd[is.na(PROPDMGEXP), PROPDMGEXP := 10^0]
sd <- sd[, CROPDMGEXP := conversionValues[sd[, CROPDMGEXP]]]
sd <- sd[is.na(CROPDMGEXP), CROPDMGEXP := 10^0]
rm(conversionValues)

# Replacing PROP and CROP columns
sd <- sd[, .(BGN_DATE, EVTYPE, FATALITIES, INJURIES,
             PROPDMGTOT = PROPDMG*PROPDMGEXP,
             CROPDMGTOT = CROPDMG*CROPDMGEXP)]



# Damage evaluation: sum of damages by event type
damageToBelongings <- sd[, .(damage = sum(PROPDMGTOT) + sum(CROPDMGTOT)),
                         by = EVTYPE]
damageToBelongings <- damageToBelongings[order(damage, decreasing = TRUE), ]
damageToBelongings <- damageToBelongings[1:15, ]
damageToBelongings$EVTYPE <- droplevels(damageToBelongings$EVTYPE)

# Injuries and deaths by event type
damageToPeople <- sd[,.(injuries = sum(INJURIES),
                        fatalities = sum(FATALITIES),
                        peopleInvolved = sum(INJURIES) + sum(FATALITIES)),
                     by = EVTYPE]
damageToPeople <- damageToPeople[order(peopleInvolved, decreasing = TRUE), ]
damageToPeople <- damageToPeople[1:15, ]
damageToPeople$EVTYPE <- droplevels(damageToPeople$EVTYPE)


### Results
# Total damage on properties and crops
head(damageToBelongings)

ggplot(data = damageToBelongings, 
       aes(x = reorder(EVTYPE, -damage), y = damage)) +
        geom_bar(stat = 'identity') + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = 'Total damage in USD from 1996 by event type',
             x = 'Event type', y = 'Total damage [USD]')

# Total people involved
head(damageToPeople)

ggplot(data = damageToPeople,
       aes(x = reorder(EVTYPE, -peopleInvolved), y = peopleInvolved)) +
        geom_bar(stat = 'identity') + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = 'Total number of people involved from 1996 by event type',
             x = 'Event type', y = 'People')

