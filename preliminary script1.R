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


SelectedData9311$TotalDamage <- SelectedData9311$PropDamage + SelectedData9311$CropDamage

###############################################################################
sort(tapply(SelectedData9311$PropDamage, SelectedData9311$TYPE, sum))
sort(tapply(SelectedData9311$CropDamage, SelectedData9311$TYPE, sum))
###############################################################################

################################################################################
################################################################################

# Get to tidy
install.packages("tidyverse")
library(tidyverse)

tidyData <- SelectedData9311 %>%  group_by(TYPE)  %>%  
  summarise(SumInjuries = sum(INJURIES), SumFatalities = sum(FATALITIES),
            SumTotalDamages = sum(TotalDamage))

###############################################################################


# Injuries
tidyInjuries <- arrange(tidyData,desc(SumInjuries))  %>% 
  mutate(SumInjuries = SumInjuries/100) %>%
  mutate(id = 1:nrow(tidyInjuries))
x <-c("23.3K", "6.8K", "6.7K", "6.2K", "5.2K", "2.5K", "2.1K", "1.7K", "1.6K", "1.6K")
length(x) <- nrow(tidyInjuries)
tidyInjuries <- cbind(x, tidyInjuries)
tidyInjuries$TYPE <- paste(tidyInjuries$TYPE, tidyInjuries$x)
tidyInjuries$TYPE <- gsub("NA","", tidyInjuries$TYPE)

# ----- ------------------------------------------- ---- #
label_data <- tidyInjuries
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar 
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


p <- ggplot(tidyInjuries, aes(x = as.factor(id), y = SumInjuries)) +
  geom_bar(stat="identity", fill=alpha("red", 0.3)) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     
  ) +
  coord_polar(start = 0) +
  geom_text(data=label_data, 
            aes(x=id, y=SumInjuries+10, label=TYPE, hjust=hjust), color="black", 
            fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle,
            inherit.aes = FALSE ) 

  

p

# Fatalities

tidyFatalities <- arrange(tidyData,desc(SumFatalities))  %>%
  mutate(SumFatalities = SumFatalities/100) %>% 
  mutate(id = 1:nrow(tidyInjuries))
x <-c("2M", "1.6K", "1.1K", "1K", "0.8K", "0.5K", "0.4K","0.4K","0.3K","0.3K") 
length(x) <- nrow(tidyFatalities)
tidyFatalities <- cbind(x, tidyFatalities)
tidyFatalities$TYPE <- paste(tidyFatalities$TYPE, tidyFatalities$x)
tidyFatalities$TYPE <- gsub("NA","", tidyFatalities$TYPE)

# ----- ------------------------------------------- ---- #
label_data <- tidyFatalities
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar 
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


p1 <- ggplot(tidyFatalities, aes(x = as.factor(id), y = SumFatalities)) +
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     
  ) +
  coord_polar(start = 0) +
  geom_text(data=label_data,
            aes(x=id, y=SumFatalities+10, label=TYPE, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=2.5, 
            angle= label_data$angle, inherit.aes = FALSE ) 



p1


install.packages("gridExtra")
library(gridExtra)
grid.arrange(p, p1, ncol=2)







#################################################################################

