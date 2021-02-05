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

######## Some transformations #############
# date

library(lubridate)
splitNames <- strsplit(data$BGN_DATE,"\\ ")
SelectedData$BGN_DATE <- year(mdy(sapply(splitNames,function(x){x[1]})))

##### Events (EVTYpe) ############


splityears <- split(SelectedData, SelectedData$BGN_DATE)
numEventsyears <- sapply(splityears, function(x){length(unique(x[[3]]))})
numEventsyears

#1. i will split the data (from 1960 t0 1992 - and form 1993 to 2011)

SelectedData9311 <- filter(SelectedData, BGN_DATE > 1992)

# As we can see, from 1960 to 1992 there are  many EVTYPE
length(unique(SelectedData9311$EVTYPE))

# But in the storm data documentation the official number of events is 48 


# To deal with this problem, I will use the amatch function of the stringdist 
# package. In order to optimize the utility of this function, first is needed to 
# make some corrections to the original events. 1) transform the to lower case letter
# 2) fix some names that are not well recognize by the amatch function
################################################################################
# 1 
SelectedData9311$EVTYPE <- tolower(SelectedData9311$EVTYPE)

library("stringdist")

# 2
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

# Now that the data set is ready..
  
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

# Economic part 

SelectedData9311 <- SelectedData9311[!grepl("[+|?]", SelectedData9311$PROPDMGEXP),]
SelectedData9311 <- SelectedData9311[!grepl("[?]", SelectedData9311$CROPDMGEXP),]

SelectedData9311$CROPDMGEXP <- as.numeric(recode(SelectedData9311$CROPDMGEXP, 
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
# 1 
# injuries/Fatalities  


layout(matrix(c(1,2,3,3),nrow=2,ncol = 2, byrow = TRUE))
top10injuries <- names(tail(sort(tapply(SelectedData9311$INJURIES, SelectedData9311$TYPE,
                                        sum)), n =10))
with(SelectedData9311 %>% filter(TYPE %in% top10injuries) %>% group_by(TYPE)
     %>% summarise(sum =sum(INJURIES))  %>% arrange(sum), barplot(height=sum,
                                                                  names=TYPE,
                                                                  horiz=T, 
                                                                  las=1))

top10fatalities <- names(tail(sort(tapply(SelectedData9311$FATALITIES, SelectedData9311$TYPE,
                                          sum)), n =10))
with(SelectedData9311 %>% filter(TYPE %in% top10fatalities) %>% group_by(TYPE)
     %>% summarise(sum =sum(FATALITIES))  %>% arrange(sum), barplot(height=sum,
                                                                  names=TYPE,
                                                                  horiz=T, 
                                                                  las=1))

least1injuries <- SelectedData9311 %>% filter(INJURIES > 0) 
least1injuries <- least1injuries %>% filter(TYPE %in% names(table(least1injuries$TYPE)[table(least1injuries$TYPE) > 1]))
top10injuriesM <- names(tail(sort(tapply(least1injuries$INJURIES, least1injuries$TYPE, 
                                         mean)), n =10))

forboxplot <- least1injuries %>% filter(TYPE %in% top10injuriesM) 
with(forboxplot, boxplot(INJURIES ~ TYPE,ylim =c(0,500),cex.axis =0.7))
means <- tapply(forboxplot$INJURIES, forboxplot$TYPE, mean)
points(means,col="red",pch=16)


##################################################################################
least1fatalities<- SelectedData9311 %>% filter(FATALITIES > 0) 
least1fatalities <- least1fatalities %>% filter(TYPE %in% names(table(least1fatalities$TYPE)[table(least1fatalities$TYPE) > 1]))
top10fatalitiesM <- names(tail(sort(tapply(least1fatalities$FATALITIES, least1fatalities$TYPE, 
                                         mean)), n =5))

with(least1injuries %>% filter(TYPE %in% top10fatalitiesM) , boxplot(FATALITIES ~ TYPE,
                                                                   ylim =c(0,120)))
#################################################################################

SelectedData9311$TotalDamage <- SelectedData9311$PropDamage + SelectedData9311$CropDamage

# Keep only the events that cause some economic damage 

someEconomicDamage <- SelectedData9311 %>% filter(TotalDamage > 0)

# sort by the 10 most destructive type events, according to the sum of the damages

top10economicS <- names(tail(sort(tapply(someEconomicDamage$TotalDamage,
                                        someEconomicDamage$TYPE, sum)),n =10))

someEconomicDamageS <- someEconomicDamage %>% filter(TYPE %in% top10economicS)

# Barplot sum

par(mfrow = c(1,1))

with(someEconomicDamageS  %>% group_by(TYPE) %>% summarise(sum =sum(TotalDamage))
     %>% arrange(desc(sum)), barplot(sum, names=TYPE,las=1, cex.names = 0.7))

# sort by the 10 most destructive type events, according to the mean of the damages

top10economicM <- names(tail(sort(tapply(someEconomicDamage$TotalDamage,
                                         someEconomicDamage$TYPE, mean)),n =10))

someEconomicDamageM <- someEconomicDamage %>% filter(TYPE %in% top10economicM)
unique(someEconomicDamageM$TYPE)

# Barplot sum

with(someEconomicDamageM, boxplot(TotalDamage ~ TYPE, ylim =c(0,500000000),cex.axis =0.7))
