###############################################################################
                        # Preliminary script
###############################################################################
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl,destfile="Storm Data.bz2",method="curl") # it takes some minutes depending on your internet connection

# Some transformation 
# date
data <- read.csv("Storm Data.bz2")
str(data)
data$BGN_DATE[1]
data$BGN_DATE[902297]

f <- grepl("0:00:00", data$BGN_DATE)
length(f)

splitNames <- strsplit(data$BGN_DATE,"\\ ")
firstElement <- function(x){x[1]}
library(lubridate)
data$BGN_DATE <- mdy(sapply(splitNames,firstElement))
class(data$BGN_DATE)

hist(data$BGN_DATE, breaks = 10)


# Events (EVTYpe)
names(data)
data$EVTYPE
unique(data$EVTYPE)
frecevents<-sort(table(data$EVTYPE))

barplot(frecevents[975:985], cex.names = 0.5)


# Fatalities
data$FATALITIES
table(data$FATALITIES)

# 
str(data$PROPDMG)
