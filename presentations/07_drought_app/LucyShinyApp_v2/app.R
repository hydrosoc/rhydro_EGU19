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
# install.packages("reshape")
# install.packages("plotly")
# install.packages("RColorBrewer")

## Load packages ##
library(shiny)
library(rgdal)
library(leaflet)
library(dygraphs)
library(dplyr)
library(SCI)
library(reshape)
library(plotly)
library(RColorBrewer)

# load app data
source("LoadAppData.R")

# set catchment numbers and names
stns <-  c(39001,39019,39020)
stnNames <- c("39001 Thames at Kingston","39019 Lambourn at Shaw","39020 Coln at Bibury")
names(stnNames) <- stns

# Define UI for the application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Using Shiny to view your data: an example"),
  br(),
  p("This demonstration Shiny app shows you how you can start to visualise your data without having to print out hundreds of image files. You can integrate lots of different data selectors and plot/map types to view and share your data."),
  
  # Sidebar with map and station selector
  sidebarPanel(
    fluidRow(
      # drop down selector
      selectInput(inputId = "Station", label="Select a catchment",
                  choices=stns,selected=stns[1],multiple = FALSE),
      hr(),
      # catch map in leaflet
      leafletOutput("catchMap"),
      p("This map has been plotted using",tags$code("leaflet"))
    )
  ),
  
  # Populate tabs
  mainPanel(
    tabsetPanel(type="tabs", # use tabs to split out content
                tabPanel("Intro", # tab 1
                         br(),
                         p("Here is some information about the catchments used in the demonstrator Shiny app:"),
                         fluidRow(
                           # a table with the catchment names and some metadat
                           tableOutput("catchSummary"),
                           p("Find out more about the UK Benchmark Network (UKBN2) in ",tags$a("Harrigan et al., (2018)",href="http://hr.iwaponline.com/content/49/2/552",target="_blank")),
                           br(),
                           p("Use ",tags$code("html")," to format text and insert hyperlinks."),
                           hr(),
                           p("All the data used here are from the UK National River Flow Archive (NRFA). You can search and download flow data, rainfall, catchment characteristics and shapefiles for over 1,500 gauging stations across the UK. ",tags$a("Search for data on the NRFA here",href="https://nrfa.ceh.ac.uk/data/search",target="_blank"))
                         )
                ),
                tabPanel("Observed Data", # tab 2
                         br(),
                         # radio button selectors to plot daily or monthly data 
                         radioButtons(inputId = "dataType", label="Select data type",
                                      choices=c("Daily","Monthly"),selected="Daily",inline=TRUE),
                         p("These interactive plots are made using ",tags$code("dygraphs"),". Find out more about dygraphs for R here:",tags$a("https://rstudio.github.io/dygraphs/index.html",href="https://rstudio.github.io/dygraphs/index.html",target="_blank")),
                         # interactive plot of catchment rainfall
                         fluidRow(
                           dygraphOutput('rainDygraph')
                         ),
                         # interactive plot of flow
                         fluidRow(
                           dygraphOutput('flowDygraph')
                         )),
                tabPanel("Standardised Drought Indicators", #tab 3
                         br(),
                         wellPanel(
                           fluidRow(
                             # radio button selector of drought indicator type
                             column(6,radioButtons(inputId = "sciType", label="Select Standardised Drought Indicator",
                                                   choices=c("SPI","SSI"),selected="SSI",inline=TRUE)),
                             # drop down selctor of accumulation period for indicator
                             column(6,selectInput(inputId = "Accum", label="Select a accumulation period:",
                                                  choices=c(1,3,6,9,12,18,24),multiple = FALSE))
                           )
                         ),
                         p("Here we are calculating Standardisd Drought Indicators (Standardised Precipitation Index (SPI) and the Standardised Streamflow Index (SSI)) on the fly using the ",tags$code("SCI")," package for the selected catchment, accumulation  period and indicator type. You can read from precalculated data files to speed things up - especially if you have large data sets. The plot below has been drawn in ",tags$code("ggplot")),
                         # static plot output of selected indictor
                         plotOutput("sciPlot"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # set reactive object functions selected in drop downs/radio buttons to select data for plots
  # catchment selected in side panel drop down
  selectedStation <- reactive({
    as.character(input$Station)
  })
  
  # selected data type (daily or monthly) rain or flows
  selectedData <- reactive({
    as.character(input$dataType)
  })
  
  # selected drought indicator (SPI or SSI)
  selectedSCI <- reactive({
    input$sciType
  })
  
  # selected indicator accumulation period
  selectedAccum <- reactive({
    as.numeric(input$Accum)
  })
  
  # plot catch map in side panel
   output$catchMap <- renderLeaflet({
     
     # define projections for UK: WGS84, British National Grid and Mercator
     wgs84 = '+proj=longlat +datum=WGS84'
     bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
     
     # make sure shapefile is corrct projection
     catchShp_wgs84 = spTransform(catchShp,CRS(wgs84))
     
     # set up the labels (print CatchID and NSE value)
     labels <- sprintf(
       "<strong> %s</strong> <br/> NRFA ref: %s <br/> UKBN2: %s <br/> Area (km2): %s <br/>Nested: %s <br/>" ,
       catchShp_wgs84$Name, catchShp_wgs84$Id, catchShp_wgs84$UKBN2, round(catchShp_wgs84$Area,2), catchShp_wgs84$Nested) %>% 
       lapply(htmltools::HTML)
    
     # create palette to colour catchments by
     catchCols <- brewer.pal(9,"YlGnBu")[3:9] 
     
     # define function to colour catchments by area
     pal <- colorBin(
       palette = catchCols,
       domain = catchShp_wgs84$Area,5)
     
     # draw map
     leaflet() %>% 
       addTiles() %>% 
       addPolygons(data=catchShp_wgs84,
                   color="grey", weight=1,fillColor=~pal(Area),
                   stroke = TRUE, fillOpacity = 0.6,
                   highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = FALSE),
                   label=labels)
   })
   
   # table of catchments on intro tab
   catchTable <- as.data.frame(catchShp@data)
   colnames(catchTable) <- c("NRFA ref","Name","UKBN2","Area (km2)","Nested","ObsStart","Recon")
   output$catchSummary <- renderTable(catchTable,striped=TRUE,align="rlcrcrc",
                                      hover=TRUE)
   
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
   
   # interactive plot of flow
   output$flowDygraph <- renderDygraph({
     
     # select daily or monthly flow
     if(selectedData()=="Daily"){
        flow <- catchFlow[,selectedStation()]
        names(flow) <- catchFlow$Date    
     }
     if(selectedData()=="Monthly"){
       flow <- catchFlowMonthly[,selectedStation()] 
       names(flow) <- catchFlowMonthly$Date    
     }
     # create dataframe
     flow <- as.data.frame(flow)
     rownames(flow) <- as.POSIXct(rownames(flow),format="%Y-%m-%d")
     
     # find start of period of record and elimate NAs
     firstDay <- rownames(na.omit(flow))[1]
     lastDay <- rownames(flow)[length(rownames(flow))]
     
     # make dygraph
     flowDygraph <- dygraph(flow,main=as.character(stnNames[selectedStation()]),group="catch") %>%
       dyOptions(colors="blue") %>%
       dyRangeSelector(dateWindow=c(firstDay,lastDay),height = 20, strokeColor = "yellow") %>%
       dyAxis("y", label = "Gauged Daily Flow (cumecs)",logscale="y") %>%
       dyUnzoom() %>%
       dyCrosshair(direction = "vertical")%>%
       dyLegend(show = "follow")
     
   })
   
   # interactive plot of rain
   output$rainDygraph <- renderDygraph({
     # select daily or monthly rain
     if(selectedData()=="Daily"){
       rain <- catchRain[,selectedStation()] 
       names(rain) <- catchRain$Date
     }
     if(selectedData()=="Monthly"){
       rain <- catchRainMonthly[,selectedStation()] 
       names(rain) <- catchRainMonthly$Date
     }
     # create dataframe
     rain <- as.data.frame(rain)
     rownames(rain) <- as.POSIXct(rownames(rain),format="%Y-%m-%d")

     # find start of period of record and elimate NAs
     firstDay <- rownames(na.omit(rain))[1]
     lastDay <- rownames(rain)[length(rownames(rain))]
     
     # make dygraph
     rainDygraph <- dygraph(rain,main=as.character(stnNames[selectedStation()]),group="catch") %>%
       dyOptions(colors="blue") %>%
       dyBarChart() %>%
       dyRangeSelector(dateWindow=c(firstDay,lastDay),height = 20, strokeColor = "yellow") %>%
       dyAxis("y", label = "Catchment Daily Rain (mm)") %>%
       dyUnzoom() %>%
       dyCrosshair(direction = "vertical") %>%
       dyLegend(show = "follow")
     
   })
   
   # make indictaor plot for indictor tab
   output$sciPlot <- renderPlot({
     
     # select data & calculate indictors
     if(selectedSCI()=="SPI"){
       sci.para <- fitSCI(catchRainMonthly[,selectedStation()],first.mon=1,time.scale=selectedAccum(),distr="gamma",p0=TRUE)
       sci <- transformSCI(catchRainMonthly[,selectedStation()],first.mon=1,obj=sci.para)
       names(sci) <- catchRainMonthly$Date
     }
     if(selectedSCI()=="SSI"){
       sci.para <- fitSCI(catchFlowMonthly[,selectedStation()],first.mon=1,time.scale=selectedAccum(),distr="gamma",p0=TRUE)
       sci <- transformSCI(catchFlowMonthly[,selectedStation()],first.mon=1,obj=sci.para)
       names(sci) <- catchFlowMonthly$Date
     }
     
     # create data.frame
     sci <- as.data.frame(sci)
     sci$Date <- rownames(sci)
     
     # select rows in the period of record (exluding months before data start)
     firstMonth <- na.omit(sci)$Date[1]
     lastMonth <- na.omit(sci)$Date[length(na.omit(sci)$Date)]
     sci <- sci[which(sci$Date==firstMonth):which(sci$Date==lastMonth),]
     
     # melt data
     sciMelt <- melt(sci)
     
     # set up x axis ticks and labs
     tick.lab.start <- round_any(as.numeric(unique(format(as.Date(firstMonth),format="%Y"))),10)
     tick.lab.end <- round_any(as.numeric(unique(format(as.Date(lastMonth),format="%Y"))),10)
     
     tick.loc <- paste(seq(from=tick.lab.start,to=tick.lab.end,by=10),"-01-01",sep="")
     tick.labs <- format(as.Date(tick.loc,format="%Y-%m-%d"),format="%Y")
     
     # make plot
     ggplot(sciMelt,aes(x=Date,y=value,group=variable))+
       geom_line()+
       scale_colour_manual(values=c("firebrick"))+
       scale_size_manual(values=c(0.75),guide=FALSE)+
       scale_y_continuous(limits = c(-3.5,3.5),breaks=c(-3,-2,-1.5,-1,0,1,1.5,2,3))+
       scale_x_discrete(breaks=tick.loc,labels=tick.labs) +
       theme(panel.grid.minor=element_line(colour = "grey80"),
             panel.grid.major=element_line(colour = "grey80"),
             panel.background = element_rect(fill = "white", colour = NA), 
             plot.background = element_rect(fill = "white", colour = NA),
             panel.border = element_rect(fill = NA, colour = "white"),
             axis.text = element_text(size=10),
             axis.title = element_text(size=12),
             legend.text = element_text(size=12),
             legend.title = element_text(size=14),
             plot.title = element_text(hjust = 0.5, size=14))+
       guides(colour=guide_legend(override.aes=list(size=4)))+
       labs(y=paste0(selectedSCI(),"-",selectedAccum()))+
       geom_hline(yintercept=c(0,-1,-1.5,-2),color="grey50",linetype="dashed")+
       labs(title=as.character(stnNames[selectedStation()]))
     
     ##ggplotly(aPlot) # this should make your plot interactive if you assign the ggplot(...) to aPlot
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

