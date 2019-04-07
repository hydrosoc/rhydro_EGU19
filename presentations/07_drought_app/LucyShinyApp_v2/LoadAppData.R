# Load Shiny app data
# Lucy Barker (lucybar@ceh.ac.uk)
# Using R in Hydrology, EGU 2019
# Shiny demonstration
###############################
# install.packages("rgdal")
# install.packages("dplyr")
library(dplyr)
library(rgdal)

# load catch flow and rain data
lambournFlow <- read.csv("./Data/39019_gdf.csv",skip=20,header=FALSE)#,row.names=1)
colnames(lambournFlow) <- c("Date","39019")
lambournRain<- read.csv("./Data/39019_cdr.csv",skip=20,header=FALSE)[,1:2]#,row.names=1)
colnames(lambournRain) <- c("Date","39019")

colnFlow <- read.csv("./Data/39020_gdf.csv",skip=20,header=FALSE)#,row.names=1)
colnames(colnFlow) <- c("Date","39020")
colnRain<- read.csv("./Data/39020_cdr.csv",skip=20,header=FALSE)[,1:2]#,row.names=1)
colnames(colnRain) <- c("Date","39020")

thamesFlow <- read.csv("./Data/39001_gdf.csv",skip=20,header=FALSE)#,row.names=1)
colnames(thamesFlow) <- c("Date","39001")
thamesRain<- read.csv("./Data/39001_cdr.csv",skip=20,header=FALSE)[,1:2]#,row.names=1)
colnames(thamesRain) <- c("Date","39001")

# join flow data together in one data frame
catchFlow <- full_join(thamesFlow,lambournFlow,by="Date")
catchFlow <- full_join(catchFlow,colnFlow,by="Date")  
rownames(catchFlow) <- catchFlow$Date

catchRain <- full_join(thamesRain,lambournRain,colnRain,by="Date")
catchRain <- full_join(catchRain,colnRain,by="Date")  
rownames(catchRain) <- catchRain$Date

# create monthly time series of rain and flow
catchFlowMonthly <- catchFlow[,-1] # remove date coliumn 
catchFlowMonthly$Year <- format(as.Date(rownames(catchFlowMonthly),format="%Y-%m-%d"),format="%Y") 
catchFlowMonthly$Month <- format(as.Date(rownames(catchFlowMonthly),format="%Y-%m-%d"),format="%m") 
catchFlowMonthly$Day <- format(as.Date(rownames(catchFlowMonthly),format="%Y-%m-%d"),format="%d") 
catchFlowMonthly <- melt(catchFlowMonthly, id.vars=c("Year","Month","Day"), na.rm=FALSE)
catchFlowMonthly <- cast(catchFlowMonthly, Year + Month ~ variable, mean, na.rm=TRUE)

catchFlowMonthly$Date <- paste(catchFlowMonthly$Year,catchFlowMonthly$Month,"01",sep="-")
rownames(catchFlowMonthly) <- catchFlowMonthly$Date
catchFlowMonthly <- catchFlowMonthly[,-1:-2] # remove year and month cols

catchRainMonthly <- catchRain[,-1] # remove date column 
catchRainMonthly$Year <- format(as.Date(rownames(catchRainMonthly),format="%Y-%m-%d"),format="%Y") 
catchRainMonthly$Month <- format(as.Date(rownames(catchRainMonthly),format="%Y-%m-%d"),format="%m") 
catchRainMonthly$Day <- format(as.Date(rownames(catchRainMonthly),format="%Y-%m-%d"),format="%d") 
catchRainMonthly <- melt(catchRainMonthly, id.vars=c("Year","Month","Day"), na.rm=FALSE)
catchRainMonthly <- cast(catchRainMonthly, Year + Month ~ variable, mean, na.rm=TRUE)

catchRainMonthly$Date <- paste(catchRainMonthly$Year,catchRainMonthly$Month,"01",sep="-")
rownames(catchRainMonthly) <- catchRainMonthly$Date
catchRainMonthly <- catchRainMonthly[,-1:-2] # remove year and month cols

# load shapefile of catch areas
catchShp <- readOGR("./Data/CatchAreas.shp",layer="CatchAreas")
