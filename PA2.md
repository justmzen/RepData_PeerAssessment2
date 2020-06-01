Synopsis
========

Storms and other severe weather events can cause both public health and
economic problems for communities and municipalities. Many severe events
can result in fatalities, injuries, and property damage, and preventing
such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and
Atmospheric Administration’s (NOAA) storm database. This database tracks
characteristics of major storms and weather events in the United States,
including when and where they occur, as well as estimates of any
fatalities, injuries, and property damage.

The analysis of such database shows that droughts, hails and floods are
the most harmful event types with respect to economics, whereas
tornadoes, excessive heat and floods are the most harmful with respect
to public health. In particular, floods are in the top 3 in both cases.

Data Processing
===============

Data loading
------------

The first part consists in downloading the [Storm
Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)
from the online repository and loading it as a data.table variable.

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

Variables management
--------------------

The variables in the data set are:

    # Variables list
    names(sd)

    ##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"     "COUNTYNAME"
    ##  [7] "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"    "BGN_LOCATI" "END_DATE"  
    ## [13] "END_TIME"   "COUNTY_END" "COUNTYENDN" "END_RANGE"  "END_AZI"    "END_LOCATI"
    ## [19] "LENGTH"     "WIDTH"      "F"          "MAG"        "FATALITIES" "INJURIES"  
    ## [25] "PROPDMG"    "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
    ## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_" "REMARKS"   
    ## [37] "REFNUM"

BGN\_DATE is the date related to each observation, hence it is useful to
specify it as date class.

    # Data manipulation
    sd$BGN_DATE <- as.Date(sd$BGN_DATE, format = '%m/%d/%Y %H:%M:%S')

Data subsetting
---------------

Since the objective is comparing the effects of different weather
events, the data set has been subsetted for dates after Jan. 1996, date
in which the observatory started recording all event types.

Furthermore, only just 8 variables out of 37 are useful to answer the
initial question:  
- BGN\_DATE: date of the event  
- EVTYPE: event type  
- FATALITIES: number of deaths  
- INJURIES: number of injuried people  
- PROPDMG: property damage as coefficient  
- PROPDMGEXP: exponent (of 10) of the property damage  
- CROPDMG: crop damage as coefficient  
- CROPDMGEXP: exponent (of 10) of the crop damage

    # Data set subsetting
    # Filtering by date
    sd <- sd[BGN_DATE >= as.Date('01/01/1996', format = '%m/%d/%Y')]

    # Choosing useful variables
    sd <- sd[, .(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP,
                 CROPDMG, CROPDMGEXP)]

Conversion of values in PROPDMGEXP and CROPDMGEXP variables
-----------------------------------------------------------

The mentioned variables present observations which are both numerical
and alphabetical.  
The work entitled [“How To Handle Exponent Value of PROPDMGEXP and
CROPDMGEXP”](https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html)
discusses the issue in depth.  
The following values found in the two variables will be converted in:  
- h/H: 100  
- k/K: 1,000  
- m/M: 1,000,000  
- b/B: 1,000,000,000  
- +: 10  
- -, ? and blank: 0

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

Finally, the real value of damages to properties and crops are evaluated
by the moltiplication between the coefficient (i.e. PROPDMG and CROPDMG)
and the relative exponent (i.e. PROPDMGEXP and CROPDMGEXP).  
The new variables are named PROPDMGTOT and CROPDMGTOT, respectively, and
replace the four variables used to calculate them.

    # Replacing PROP and CROP columns
    sd <- sd[, .(BGN_DATE, EVTYPE, FATALITIES, INJURIES,
                 PROPDMGTOT = PROPDMG*PROPDMGEXP,
                 CROPDMGTOT = CROPDMG*CROPDMGEXP)]

Global calculations by event type
---------------------------------

The global damages are evaluated by event type and are calculated as the
sum of the damages to properties and crops. Only the first 15 will be
considered.

    # Damage evaluation: sum of damages by event type
    damageToBelongings <- sd[, .(damage = sum(PROPDMGTOT) + sum(CROPDMGTOT)),
                             by = EVTYPE]
    damageToBelongings <- damageToBelongings[order(damage, decreasing = TRUE), ]
    damageToBelongings <- damageToBelongings[1:15, ]
    damageToBelongings$EVTYPE <- droplevels(damageToBelongings$EVTYPE)

The same thing is applied to the people involved, calculated as the sum
of injuried and dead people.

    # Injuries and deaths by event type
    damageToPeople <- sd[,.(injuries = sum(INJURIES),
                            fatalities = sum(FATALITIES),
                            peopleInvolved = sum(INJURIES) + sum(FATALITIES)),
                         by = EVTYPE]
    damageToPeople <- damageToPeople[order(peopleInvolved, decreasing = TRUE), ]
    damageToPeople <- damageToPeople[1:15, ]
    damageToPeople$EVTYPE <- droplevels(damageToPeople$EVTYPE)

Results
=======

The most harmful event types on properties and crops are plotted:

    # Total damage on properties and crops
    ggplot(data = damageToBelongings, 
           aes(x = reorder(EVTYPE, -damage), y = damage)) +
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = 'Total damage in USD from 1996 by event type',
                 x = 'Event type', y = 'Total damage [USD]')

![](PA2_files/figure-markdown_strict/unnamed-chunk-19-1.png)

The bar plot shows that the 6 most harmful event types on properties and
crops are:

    head(damageToBelongings)

    ##         EVTYPE       damage
    ## 1:     DROUGHT 1.256654e+12
    ## 2:        HAIL 7.071216e+11
    ## 3:       FLOOD 6.512212e+11
    ## 4: FLASH FLOOD 2.916149e+11
    ## 5:   HURRICANE 2.821438e+11
    ## 6:   TSTM WIND 1.576684e+11

Whereas, the most harmful event types on people are:

    # Total people involved
    head(damageToPeople)

    ##            EVTYPE injuries fatalities peopleInvolved
    ## 1:        TORNADO    20667       1511          22178
    ## 2: EXCESSIVE HEAT     6391       1797           8188
    ## 3:          FLOOD     6758        414           7172
    ## 4:      LIGHTNING     4141        651           4792
    ## 5:      TSTM WIND     3629        241           3870
    ## 6:    FLASH FLOOD     1674        887           2561

    ggplot(data = damageToPeople,
           aes(x = reorder(EVTYPE, -peopleInvolved), y = peopleInvolved)) +
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = 'Total number of people involved from 1996 by event type',
                 x = 'Event type', y = 'People')

![](PA2_files/figure-markdown_strict/unnamed-chunk-21-1.png)
