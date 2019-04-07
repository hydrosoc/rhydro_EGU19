#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Install packages ##
# install.packages("shiny")
# install.packages("rgdal")
# install.packages("leaflet")
# install.packages("dygraphs")
# install.packages("dplyr")
# install.packages("SCI")
# install.packages("reshape)

## Load packages ##
library(shiny)
library(rgdal)
library(leaflet)
library(dygraphs)
library(dplyr)
library(SCI)
library(reshape)

# root folder
rootFolder <- "."

stns <-  c(39001,39019,39020)
stnNames <- c("39001 Thames at Kingston","39019 Lambourn at Shaw","39020 Coln at Bibury")
names(stnNames) <- stns

#load data
lambournFlow <- read.csv(paste0(rootFolder,"/Data/39019_gdf.csv"),skip=20,header=FALSE)#,row.names=1)
colnames(lambournFlow) <- c("Date","39019")
lambournRain<- read.csv(paste0(rootFolder,"/Data/39019_cdr.csv"),skip=20,header=FALSE)[,1:2]#,row.names=1)
colnames(lambournRain) <- c("Date","39019")
#lambournShp <- readOGR(paste0(rootFolder,"/Data/39019.shp"),layer="39019")

colnFlow <- read.csv(paste0(rootFolder,"/Data/39020_gdf.csv"),skip=20,header=FALSE)#,row.names=1)
colnames(colnFlow) <- c("Date","39020")
colnRain<- read.csv(paste0(rootFolder,"/Data/39020_cdr.csv"),skip=20,header=FALSE)[,1:2]#,row.names=1)
colnames(colnRain) <- c("Date","39020")
#colnShp <- readOGR(paste0(rootFolder,"/Data/39020.shp"),layer="39020")

thamesFlow <- read.csv(paste0(rootFolder,"/Data/39001_gdf.csv"),skip=20,header=FALSE)#,row.names=1)
colnames(thamesFlow) <- c("Date","39001")
thamesRain<- read.csv(paste0(rootFolder,"/Data/39001_cdr.csv"),skip=20,header=FALSE)[,1:2]#,row.names=1)
colnames(thamesRain) <- c("Date","39001")
#thamesShp <- readOGR(paste0(rootFolder,"/Data/39001.shp"),layer="39001")

catchShp <- readOGR(paste0(rootFolder,"/Data/CatchAreas.shp"),layer="CatchAreas")

catchFlow <- full_join(thamesFlow,lambournFlow,by="Date")
catchFlow <- full_join(catchFlow,colnFlow,by="Date")  
rownames(catchFlow) <- catchFlow$Date

catchRain <- full_join(thamesRain,lambournRain,colnRain,by="Date")
catchRain <- full_join(catchRain,colnRain,by="Date")  
rownames(catchRain) <- catchRain$Date

catchFlowMonthly <- catchFlow[,-1] # remove date coliumn 
catchFlowMonthly$Year <- format(as.Date(rownames(catchFlowMonthly),format="%Y-%m-%d"),format="%Y") 
catchFlowMonthly$Month <- format(as.Date(rownames(catchFlowMonthly),format="%Y-%m-%d"),format="%m") 
catchFlowMonthly$Day <- format(as.Date(rownames(catchFlowMonthly),format="%Y-%m-%d"),format="%d") 
catchFlowMonthly <- melt(catchFlowMonthly, id.vars=c("Year","Month","Day"), na.rm=FALSE)
catchFlowMonthly <- cast(catchFlowMonthly, Year + Month ~ variable, mean, na.rm=TRUE)

catchRainMonthly <- catchRain[,-1] # remove date coliumn 
catchRainMonthly$Year <- format(as.Date(rownames(catchRainMonthly),format="%Y-%m-%d"),format="%Y") 
catchRainMonthly$Month <- format(as.Date(rownames(catchRainMonthly),format="%Y-%m-%d"),format="%m") 
catchRainMonthly$Day <- format(as.Date(rownames(catchRainMonthly),format="%Y-%m-%d"),format="%d") 
catchRainMonthly <- melt(catchRainMonthly, id.vars=c("Year","Month","Day"), na.rm=FALSE)
catchRainMonthly <- cast(catchRainMonthly, Year + Month ~ variable, mean, na.rm=TRUE)


# Define UI for the application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring UK droughts"),
  br(),
  p("here is a Shiny app"),
  p("Tory MP Sir Oliver Letwin, who supports Ms Cooper's bill, said: This is a last-ditch attempt to prevent our country being exposed to the risks inherent in a no-deal exit. We realise this is difficult. But it is definitely worth trying.
     Ms Cooper said the UK was in a very dangerous situation and MPs have a responsibility to make sure we don't end up with a catastrophic no deal. Speaking to BBC Radio 4's World At One, she added: We have been attempting to squeeze into just a couple of days a process that really should have been happening for the last two years - a process of trying to build a consensus around the best way forward"),
  
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    fluidRow(
      p("Select a catchment to view  data on the Flow data tab"),
      selectInput(inputId = "Station", label="Select a catchment",
                  choices=stns,selected=stns[1],multiple = FALSE),
      #hr(),
      leafletOutput("catchMap"),
      p("This map has been plotted using",tags$code("leaflet"))
    )
  ),
  
  # Populate tabs
  mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("Intro",
                         br(),
                         p("Here we some information about the catchments:"),
                         fluidRow(
                           tableOutput("catchSummary"),
                           p("Find out more about the UK Benchmark Network (UKBN2) in ",tags$a("Harrigan et al., (2018)",href="http://hr.iwaponline.com/content/49/2/552",target="_blank")),
                           br(),
                           p("Use ",tags$code("html")," to format text and insert hyperlinks.")
                         )
                ),
                tabPanel("Obs Data",
                         br(),
                         p("Find out more about dygraphs for R here:",tags$a("https://rstudio.github.io/dygraphs/index.html",href="https://rstudio.github.io/dygraphs/index.html",target="_blank")),
                         fluidRow(
                           dygraphOutput('rainDygraph')
                         ),
                         fluidRow(
                           dygraphOutput('flowDygraph')
                         )),
                tabPanel("Drought indicators",
                         br(),
                         p("add some drought plots"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  selectedStation <- reactive({
    as.character(input$Station)
  })
  
   output$catchMap <- renderLeaflet({
     
     # define projections for UK: WGS84, British National Grid and Mercator
     wgs84 = '+proj=longlat +datum=WGS84'
     bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
     
     catchShp_wgs84 = spTransform(catchShp,CRS(wgs84))
     
     # set up the labels (print CatchID and NSE value)
     labels <- sprintf(
       "<strong> %s</strong> <br/> NRFA ref: %s <br/> UKBN2: %s <br/> Area (km2): %s <br/>Nested: %s <br/>" ,
       catchShp_wgs84$Name, catchShp_wgs84$Id, catchShp_wgs84$UKBN2, round(catchShp_wgs84$Area,2), catchShp_wgs84$Nested) %>% 
       lapply(htmltools::HTML)
     # labelsSelected <- sprintf(
     #   "<strong> %s</strong> <br/> NRFA ref: %s <br/> UKBN2: %s <br/> Area (km2): %s <br/>Nested: %s <br/>" ,
     #   catchShp_wgs84[catchShp_wgs84$Id==selectedStation(),]$Name, catchShp_wgs84[catchShp_wgs84$Id==selectedStation(),]$Id, catchShp_wgs84[catchShp_wgs84$Id==selectedStation(),]$UKBN2, round(catchShp_wgs84[catchShp_wgs84$Id==selectedStation(),]$Area,2), catchShp_wgs84[catchShp_wgs84$Id==selectedStation(),]$Nested) %>%
     #   lapply(htmltools::HTML)
     
     catchCols <- c('#cab2d6','#1f78b4','#b2df8a')
     
     pal <- colorFactor(
       palette = catchCols,
       domain = catchShp_wgs84$Id,levels=stns)
     
     leaflet() %>% 
       addTiles() %>% 
       addPolygons(data=catchShp_wgs84,
                   color="grey", weight=1,fillColor=~pal(stns),
                   stroke = TRUE, fillOpacity = 0.6,
                   highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = FALSE),
                   label=labels) #%>%
       # addPolygons(data=catchShp_wgs84[catchShp_wgs84$Id==selectedStation(),],
       #             stroke = TRUE,fillColor~pal(stns),color="#1ACFBC", weight=3,
       #             highlightOptions=highlightOptions(color="#1ACFBC",weight = 5, bringToFront = FALSE),
       #             label=labelsSelected)
   })
   
   catchTable <- as.data.frame(catchShp@data)
   colnames(catchTable) <- c("NRFA ref","Name","UKBN2","Area (km2)","Nested","ObsStart","Recon")
   output$catchSummary <- renderTable(catchTable,striped=TRUE,align="rlcrcrc",
                                      hover=TRUE)
   
   output$flowDygraph <- renderDygraph({
     
     # Run special dygraph functions
     dyUnzoom <-function(dygraph) {
       dyPlugin(
         dygraph = dygraph,
         name = "Unzoom",
         path = system.file("plugins/unzoom.js", package = "dygraphs")
       )
     }
     
     dyCrosshair <- function(dygraph,
                             direction = c("both", "horizontal", "vertical")) {
       dyPlugin(
         dygraph = dygraph,
         name = "Crosshair",
         path = system.file("plugins/crosshair.js",
                            package = "dygraphs"),
         options = list(direction = match.arg(direction))
       )
     }
     
     flow <- catchFlow[,selectedStation()]
     names(flow) <- catchFlow$Date
     flow <- as.data.frame(flow)
     rownames(flow) <- as.POSIXct(rownames(flow),format="%Y-%m-%d")
     #flow$Date <- as.POSIXct(rownames(flow),format="%Y-%m-%d")
     
     firstDay <- rownames(na.omit(flow))[1]
     lastDay <- rownames(flow)[length(rownames(flow))]
     
     flowDygraph <- dygraph(flow,main=as.character(stnNames[selectedStation()])) %>%
       dyOptions(colors="blue") %>%
       dyRangeSelector(dateWindow=c(firstDay,lastDay),height = 20, strokeColor = "yellow") %>%
       dyAxis("y", label = "Gauged Daily Flow (cumecs)") %>%
       dyUnzoom() %>%
       dyCrosshair(direction = "vertical")%>%
       dyLegend(show = "follow")
     
   })
   
   output$rainDygraph <- renderDygraph({
     
     # Run special dygraph functions
     dyUnzoom <-function(dygraph) {
       dyPlugin(
         dygraph = dygraph,
         name = "Unzoom",
         path = system.file("plugins/unzoom.js", package = "dygraphs")
       )
     }
     
     dyCrosshair <- function(dygraph,
                             direction = c("both", "horizontal", "vertical")) {
       dyPlugin(
         dygraph = dygraph,
         name = "Crosshair",
         path = system.file("plugins/crosshair.js",
                            package = "dygraphs"),
         options = list(direction = match.arg(direction))
       )
     }
     
     rain <- catchRain[,selectedStation()]
     names(rain) <- catchRain$Date
     rain <- as.data.frame(rain)
     rownames(rain) <- as.POSIXct(rownames(rain),format="%Y-%m-%d")
     #flow$Date <- as.POSIXct(rownames(flow),format="%Y-%m-%d")
     
     firstDay <- rownames(na.omit(rain))[1]
     lastDay <- rownames(rain)[length(rownames(rain))]
     
     flowDygraph <- dygraph(rain,main=as.character(stnNames[selectedStation()])) %>%
       dyOptions(colors="blue") %>%
       dyRangeSelector(dateWindow=c(firstDay,lastDay),height = 20, strokeColor = "yellow") %>%
       dyAxis("y", label = "Catchment Daily Rain (mm)") %>%
       dyUnzoom() %>%
       dyCrosshair(direction = "vertical") %>%
       dyLegend(show = "follow")
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

