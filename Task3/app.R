library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(dplyr)
library(highcharter)
library(tidyr)
library(msmtools)

# read data
no2_19 <- read.csv("spotlight_no2.csv")
ozone_19 <- read.csv("spotlight_ozone.csv")
ozone_19_df <- read.csv("ozone_19_df.csv")
# delete the first colume
ozone_19_df <- ozone_19_df[, -1]

# data preprocessing for the ozone_data used in the shiny
ozone_data2020_lxt <- read.csv("spotlight-covid-concentrations-data_ozone.csv")
year_seq <- seq.Date(from = as.Date("2020/03/01",format = "%Y/%m/%d"), by = "day", length.out = dim(ozone_data2020_lxt["year"])[1])
ozone_data2020_lxt["year"] <- year_seq
# ozone_data2020_lxt

ozone_data19_20_lxt <- read.csv("spotlight_ozone_10yrstats.csv")
ozone_data19_20_lxt$p90 <- round(ozone_data19_20_lxt$p90,4)
ozone_data19_20_lxt$average <- round(ozone_data19_20_lxt$average,4)
ozone_data19_20_lxt$p10 <- round(ozone_data19_20_lxt$p10,4)
ozone_data19_20_lxt$day <- year_seq
# ozone_data19_20_lxt


# build a date list in date format
startdate <- as.Date("2020/3/1")
enddate <- as.Date("2020/5/31")
ndays <- enddate - startdate + 1
tt <- ts(1:ndays, frequency =1, start =as.Date("2020/3/1"))
ss <- as.Date("2020/3/1")
dates <- seq(from=ss, by=1, length.out=ndays)

# to rename the dataset
icons <- list()
j <- 1
for (i in dates){
  icon <- as.character(as.Date(365, origin=dates[j]))
  icons[j] <- as.character(icon)
  j <- j + 1
}
names(ozone_19_df) <- c("latitude", "longitude",
                        "local_site_name", as.character(dates), as.character(icons))


# ui  
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  # show the time control line
  absolutePanel(top = 15, left = 70,
                sliderTextInput("date",
                                label = "Select a Date:",
                                choices = as.character(dates),
                                selected = "2020-03-01",
                                grid = FALSE,
                                # can show the change automaticly
                                animate=animationOptions(interval = 600, loop = FALSE))
  ),
  
  # show the ozone concentration plot which can be moved
  absolutePanel(id = "controls", class = "panel panel-default",
                top = 100, left = 5, width = 450, fixed=TRUE,
                draggable = TRUE, height = "auto",
                
                highchartOutput('Ozone_Concentration',width="100%")
  )
)

# server
server <- function(input, output, session) {
  # select the daily data
  filteredData <- reactive({
    data <- select(ozone_19_df, c("latitude", "longitude",
                                  "local_site_name",
                                  "con"=input$date,
                                  "icon"=as.character(as.Date(365, origin=input$date))))
    # delete the NA value in the selected date's data
    data <- na.omit(data)
    
  })
  
  # build a list to store the icons we will use later
  Iconslist <- iconList(
    minus1 = makeIcon(iconUrl="images/minus1.png", iconWidth = 25, iconHeight = 25,
                      shadowWidth = 10, shadowHeight = 10),
    minus2 = makeIcon(iconUrl="images/minus2.png", iconWidth = 25, iconHeight = 25,
                      shadowWidth = 10, shadowHeight = 10),
    plus1 = makeIcon(iconUrl="images/plus1.png", iconWidth = 25, iconHeight = 25,
                     shadowWidth = 10, shadowHeight = 10),
    plus2 = makeIcon(iconUrl="images/plus2.png", iconWidth = 25, iconHeight = 25,
                     shadowWidth = 10, shadowHeight = 10)
  )
  
  # use leaflet show the ozone concentration map
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addMarkers(~longitude, ~latitude, 
                 icon = ~Iconslist[icon],
                 label = ~local_site_name, 
                 labelOptions = labelOptions(textsize = "12px"))
  })
  
  output$Ozone_Concentration <- renderHighchart({
    highchart() %>% 
      # title
      hc_title(text = "Ozone 8-hour Concentration",
               margin = 1,
               style = list(color = "black")) %>%
      # set up line form
      hc_chart(type = 'line') %>%
      hc_yAxis(title = list(text = "Concentration (ppm)"),
               # add the line of the national standard
               plotLines = list(
                 list(
                   label = list(text = "Most Recent National Standard", align="right"),
                   dashstyle = "Dash",
                   value = 0.07,
                   width = 1,
                   color = "grey"
                 )
               )) %>%
      # every x is year
      hc_xAxis(categories = ozone_data2020_lxt$year) %>% 
      
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      # append the series of data with the arearange
      hc_add_series(name = "range of 10-90 Percentile (2020)",
                    data = list_parse2(data.frame(ozone_data19_20_lxt$p10,ozone_data19_20_lxt$p90)), 
                    type = "arearange",
                    color= "grey",
                    showInLegend = T) %>%
      
      hc_add_series(name = "range of 10-90 Percentile (2010-2019)",
                    data = list_parse2(data.frame(ozone_data2020_lxt$p10,ozone_data2020_lxt$p90)),
                    type = "arearange",
                    color = "green",
                    showInLegend = T) %>%
      # add line
      hc_add_series(name = "Average (2020)",data = ozone_data2020_lxt$avg, color='yellow',marker = list(symbol = 'circle')) %>%
      
      hc_add_series(name = "Average (2010-2019)",data = ozone_data19_20_lxt$average,color='black',marker = list(symbol = 'triangle')) %>%
      
      # optional setting
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        stacking = "normal",
        enableMouseTracking = TRUE)) %>%
      # The text when we select
      hc_tooltip(table = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>'," <b>{series.name}:</b> {point.y} ppm"),
                 headerFormat = '<span style="font-size: 13px"><b>Day:</b> {point.key}</span>',
                 crosshairs = TRUE, 
                 backgroundColor = "#FCFFC5",
                 shared = TRUE, 
                 borderWidth = 8)
  })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addMarkers(~longitude, ~latitude, 
                 icon = ~Iconslist[icon], 
                 label = ~local_site_name, 
                 labelOptions = labelOptions(textsize = "12px"))
  })
}

shinyApp(ui, server)