###############################################################################
                        # Preliminary script
###############################################################################
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl,destfile="Storm Data.bz2",method="curl") # it takes some minutes depending on your internet connection

########### Decompression ###########
install.packages("R.utils")
library(R.utils)
bunzip2("Storm Data.bz2", destname = "Storm DataDes", remove = FALSE)
data <- read.csv("Storm DataDES")
################################################################################
#Select relevant variables
library(dplyr)

SelectedData <- data %>% select(STATE__,BGN_DATE,EVTYPE,FATALITIES,INJURIES,
                                PROPDMG,PROPDMGEXP,CROPDMG, CROPDMGEXP)

######## Some transformation #############
# date


splitNames <- strsplit(data$BGN_DATE,"\\ ")

firstElement <- function(x){x[1]}

library(lubridate)
SelectedData$BGN_DATE <- year(mdy(sapply(splitNames,firstElement)))

##### Events (EVTYpe) ############


list <- split(SelectedData, SelectedData$BGN_DATE)
numEventsyears <- sapply(list, function(x){
                length(unique(x[[3]]))
                }
        )
numEventsyears

#1. i will split the data (from 1960 t0 1992 - and form 1993 to 2011)

SelectedData9311 <- filter(SelectedData, BGN_DATE > 1992)


# As we can see, from 1960 to 1992 there are  many EVTYPE
length(unique(SelectedData9311$EVTYPE))

sort(table(SelectedData9311$EVTYPE))


# 2. With the dataset from 1960 to 1992 I explore the events that caused at least 1 
# death or Injury

antidata <- SelectedData9311 %>% filter(INJURIES == 0 & FATALITIES == 0) 
unique(antidata$EVTYPE)

SelectedDataF1 <- SelectedData9311 %>% filter(INJURIES > 0 | FATALITIES > 0 |
                                                      PROPDMG > 0 | CROPDMG > 0) 


################################################################################
#### With amatch we can create a recategorization 

# To lower

SelectedDataF1$EVTYPE <- tolower(SelectedDataF1$EVTYPE)

length(unique(SelectedDataF1$EVTYPE)) 

# amatch
# here's an example of amatch
library("stringdist")


## but first we need to fix some names that are badly categorized by amatch


SelectedDataF1$EVTYPE <- gsub("tstm","thunderstorm",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("^wind","high winds",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("unseasonably warm and dry","heat",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("unseasonably warm","heat",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("^fog","dense fog",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("landslide","debris flow",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("landslide","debris flow",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("light ","winter ",SelectedDataF1$EVTYPE)
SelectedDataF1$EVTYPE <- gsub("(wet|dry) microburst|microburst","thunderstorm",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("moderate","",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("snow squalls","heavy snow",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("^smoke","dense smoke",SelectedDataF1$EVTYPE) 
SelectedDataF1 <- SelectedDataF1[!grepl("late|unseason(.*)|prolong|mudslide|
                                            black|black ice|wall|summary|(.*)record(.*)|
                                            abnormal|blowing|glaze|other
                                            |precipitation|unusual|dry",SelectedDataF1$EVTYPE),]


categories <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood",
                "Cold/Wind Chill", "Debris Flow",  "Dense Fog", "Dense Smoke","Drought",
                "Dust Devil", "Dust Storm","Excessive Heat","Extreme Cold/Wind Chill",
                "Flash Flood","Flood","Frost/Freeze","Funnel Cloud", "Freezing Fog", 
                "Hail","Heat" ,"Heavy Rain", "Heavy Snow" ,"High Surf" ,"High Wind",
                "Hurricane/Typhoon","Ice Storm","Lake-Effect Snow","Lakeshore Flood",
                "Lightning" ,"Marine Hail","Marine High Wind","Marine Strong Wind",
                "Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Surge/Tide",
                "Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression",
                "Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire", 
                "Winter Storm","Winter Weather")

categories <- tolower(categories)



################################################################################
grep("dry",SelectedDataF1$EVTYPE, value = TRUE)

sort(table(SelectedDataF1$EVTYPE))

f <- grep("snowfall",SelectedDataF1$EVTYPE)

SelectedDataF1$EVTYPE[f[1]]
d <- amatch(SelectedDataF1$EVTYPE[f[1]],categories,method = "jw", maxDist=20)
categories[d]
################################################################################






# recategorization

SelectedDataF1$TYPE <- sapply(SelectedDataF1$EVTYPE, function(x){
                i <- amatch(x,categories,method = "jw", maxDist=20)
                categories[i]
                }
        ) 
SelectedDataF1 <- SelectedDataF1 %>% relocate(TYPE, .after = EVTYPE)







# gsub

unique(grep("avalance", SelectedDataF1$EVTYPE))
SelectedDataF1$EVTYPE <- gsub("avalance","avalanche",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("(.*)blizzard(.*)","blizzard",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("coastal flood(.*)","coastal flood",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("^cold(.*)","cold/wind chill",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("(.*)fog(.*)","dense fog",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("(.*)drought(.*)","drought",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("(.*)excessive heat|extreme heat","excessive heat",SelectedDataF1$EVTYPE) 
SelectedDataF1$EVTYPE <- gsub("(.*)extreme cold(.*)|cold$","extreme cold/wind child",SelectedDataF1$EVTYPE) 
unique(grep("(.*)extreme cold(.*)|cold$",SelectedDataF1$EVTYPE, value= TRUE)) 

grep("extreme cold", SelectedDataF1$EVTYPE, value = TRUE)
SelectedDataF1$EVTYPE <- gsub("(.*)extreme cold(.*)","extreme cold/wind child",SelectedDataF1$EVTYPE) 
unique(grep("(.*)extreme cold(.*)",SelectedDataF1$EVTYPE, value= TRUE)) 

grep("storm surge/tide", SelectedDataF1$EVTYPE, value = TRUE)
SelectedDataF1$EVTYPE <- gsub("coastal(.*)storm","storm surge/tide",SelectedDataF1$EVTYPE) 
grep("coastal(.*)storm",SelectedDataF1$EVTYPE, value= TRUE) 





## Economic issue

unique(data$CROPDMG)
unique(data$PROPDMG)





