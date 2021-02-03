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

#Select relevant variables
library(dplyr)

SelectedData <- data %>% select(REFNUM,STATE__,BGN_DATE,EVTYPE,LENGTH,WIDTH,MAG,
                                 FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG, 
                                 CROPDMGEXP,WFO)

######## Some transformation #############
# date
SelectedData$BGN_DATE[1]
SelectedData$BGN_DATE[902297]

f <- grepl("0:00:00", SelectedData$BGN_DATE)
length(f)

splitNames <- strsplit(data$BGN_DATE,"\\ ")
firstElement <- function(x){x[1]}

library(lubridate)
SelectedData$BGN_DATE <- year(mdy(sapply(splitNames,firstElement)))






##### Events (EVTYpe) ############
names(SelectedData)
unique(SelectedData$EVTYPE)
frecevents<-sort(table(data$EVTYPE))


table(filter(SelectedData,BGN_DATE == 1993)$EVTYPE)


# i will split the data (from 1960 t0 1992 - and form 1993 to 2011)

SelectedData6092 <- filter(SelectedData, BGN_DATE <= 1992)
SelectedData9311 <- filter(SelectedData, BGN_DATE > 1992)

# As we can see, from 1960 to 1992 there are only 3 EVTYPE

unique(SelectedData6092$EVTYPE)  #is it useful for the analysis? -- NOt!!!

# As we can see, from 1960 to 1992 there are  many EVTYPE
unique(SelectedData9311$EVTYPE)

sort(table(SelectedData9311$EVTYPE), decreasing = TRUE)[1:100]

# With the dataset from 1960 to 1992 I explore the events that caused at least 1 
# death or Injury

SelectedDataF1 <- SelectedData9311 %>% filter(INJURIES > 0 | FATALITIES > 0) 

unique(SelectedDataF1$EVTYPE)

