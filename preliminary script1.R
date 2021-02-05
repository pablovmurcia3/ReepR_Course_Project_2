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



################################################################################
SelectedData9311$EVTYPE <- tolower(SelectedData9311$EVTYPE)


# amatch
# here's an example of amatch
library("stringdist")


## but first we need to fix some names that are badly categorized by amatch


SelectedData9311$EVTYPE <- gsub("tstm","thunderstorm",SelectedData9311$EVTYPE) 
SelectedData9311$EVTYPE <- gsub("^wind","high winds",SelectedData9311$EVTYPE) 
SelectedData9311$EVTYPE <- gsub("unseasonably warm and dry","heat",SelectedData9311$EVTYPE) 
SelectedData9311$EVTYPE <- gsub("unseasonably warm","heat",SelectedData9311$EVTYPE) 
SelectedData9311$EVTYPE <- gsub("^fog","dense fog",SelectedData9311$EVTYPE) 
SelectedData9311$EVTYPE <- gsub("landslide","debris flow",SelectedData9311$EVTYPE) 
SelectedData9311$EVTYPE <- gsub("light ","winter ",SelectedData9311$EVTYPE) 
SelectedData9311$EVTYPE <- gsub("(wet|dry) microburst|microburst","thunderstorm",SelectedData9311$EVTYPE) 
SelectedData9311$EVTYPE <- gsub("moderate","",SelectedData9311$EVTYPE) 
SelectedData9311$EVTYPE <- gsub("snow squalls","heavy snow",SelectedData9311$EVTYPE) 
SelectedData9311$EVTYPE <- gsub("^smoke","dense smoke",SelectedData9311$EVTYPE) 


SelectedData9311 <- SelectedData9311[!grepl("late|unseason(.*)|prolong|mudslide|
                                            black|black ice|wall|summary|(.*)record(.*)|
                                            abnormal|blowing|glaze|other
                                            |precipitation|unusual|dry",SelectedData9311$EVTYPE),]


################################################################################
grep("urban/small stream",SelectedData9311$EVTYPE, value = TRUE)
sort(table(SelectedData9311$EVTYPE))


f <- grep("urban/small stream",SelectedData9311$EVTYPE)

SelectedData9311$EVTYPE[f[43]]
d <- amatch(SelectedData9311$EVTYPE[f[43]],categories,method = "jw", maxDist=20)
categories[d]
################################################################################
  
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


# recategorization

SelectedData9311$TYPE <- sapply(SelectedData9311$EVTYPE, function(x){
        i <- amatch(x,categories,method = "jw", maxDist=20)
        categories[i]
        }
) 

SelectedData9311 <- SelectedData9311 %>% relocate(TYPE, .after = EVTYPE)



################################################################################
sort(tapply(SelectedData9311$FATALITIES, SelectedData9311$TYPE, sum))
sort(tapply(SelectedDataF1$FATALITIES, SelectedDataF1$TYPE, mean))
################################################################################
# Better option -- take the SelectedData9311 dataset
# --- injuries -- boxplot 
# --- fatalities -- bar

################################################################################

SelectedData9311 <- SelectedData9311[!grepl("[+|?]", SelectedData9311$PROPDMGEXP),]
SelectedData9311 <- SelectedData9311[!grepl("[?]", SelectedData9311$CROPDMGEXP),]


SelectedData9311$SelectedData9311$CropDamage <- as.numeric(recode(SelectedData9311$CROPDMGEXP, 
                              B = "1000000000", M ="1000000" , K ="1000" ,
                              k = "1000", "0"= "10","2" = "10", "8" = "10", 
                              .default = "0"))

SelectedData9311$PROPDMGEXP <- as.numeric(recode(SelectedData9311$PROPDMGEXP,
                              B = "1000000000", M ="1000000",m = "1000000",
                              K ="1000" , H ="100",h ="100", "0"= "10","1"= "10",
                              "2" = "10", "3" = "10", "4" = "10", "5" = "10", 
                              "6" = "10", "7" = "10", "8" = "10", .default = "0"))

SelectedData9311$CropDamage <- SelectedData9311$CROPDMG*SelectedData9311$CROPDMGEXP
SelectedData9311$PropDamage <- SelectedData9311$PROPDMG*SelectedData9311$PROPDMGEXP

###############################################################################
sort(tapply(SelectedData9311$PropDamage, SelectedData9311$TYPE, sum))
sort(tapply(SelectedData9311$CropDamage, SelectedData9311$TYPE, sum))
###############################################################################

################################################################################
################################################################################


