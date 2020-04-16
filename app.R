library(shiny)
library(leaflet)
library(sp)
library(rgdal)
library(htmltools)
library(DT)
library(ggplot2)
library(shinydashboard)

path <- "C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/"
setwd(path)

####################################### ADD ALL CSV FILES + POPUPS ###################################################

################## 1: GROCERY #########################
GS <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/GS .csv")

GS$y <- as.numeric(GS$y)
GS$x <- as.numeric(GS$x)

GS.SP <- SpatialPointsDataFrame(GS[,c(7,8)], GS[,-c(7,8)])

GS$popup <- paste("<p><h2>", GS$name,"</p></h2>",
                  "<p>", GS$sector,"</p>",
                  "<p>", GS$address,"</p>",
                  "<p>", GS$phone,"</p>",
                  "<p>Hours: ", GS$hours,"</p>",
                  "<p>Snap benefits? ", GS$snap,"</p>",
                  "<p>WIC benefits? ", GS$wic,"</p>")

benefits <- c(GS$snap, GS$wic)


################## 2: FARMERS #########################
FM <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/FM.csv")

FM$y <- as.numeric(FM$y)
FM$x <- as.numeric(FM$x)

FM.SP <- SpatialPointsDataFrame(FM[,c(7,8)], FM[,-c(7,8)])

FM$popup <- paste("<p><h2>", FM$name,"</p></h2>",
                  "<p>", FM$sector,"</p>",
                  "<p>", FM$address,"</p>",
                  "<p>", FM$phone,"</p>",
                  "<p>Snap benefits? ", FM$snap,"</p>",
                  "<p>WIC benefits? ", FM$wic,"</p>",
                  "<p>Senior vouchers? ", FM$fm_senior,"</p>")


################## 3: LIBRARIES #######################
LIB <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/LIB.csv")

LIB$y <- as.numeric(LIB$y)
LIB$x <- as.numeric(LIB$x)

LIB.SP <- SpatialPointsDataFrame(LIB[,c(7,8)], LIB[,-c(7,8)])

LIB$popup <- paste("<p><h2>", LIB$name,"</p></h2>",
                   "<p>", LIB$sector,"</p>",
                   "<p>", LIB$address,"</p>",
                   "<p>", LIB$phone,"</p>",
                   "<p>", LIB$hours,"</p>",
                   "<p>Services: ", LIB$re_lib_serves,"</p>")


################## 4: EXT OFFICES ######################
EO <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/EO.csv")

EO$y <- as.numeric(EO$y)
EO$x <- as.numeric(EO$x)

EO.SP <- SpatialPointsDataFrame(EO[,c(7,8)], EO[,-c(7,8)])

EO$popup <- paste("<p><h2>", EO$name,"</p></h2>",
                  "<p>", EO$sector,"</p>",
                  "<p>", EO$address,"</p>",
                  "<p>", EO$phone,"</p>",
                  "<p>", EO$hours,"</p>")


################## 5: RECYCLE #########################
RE <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/RE.csv")

RE$y <- as.numeric(RE$y)
RE$x <- as.numeric(RE$x)

RE.SP <- SpatialPointsDataFrame(RE[,c(7,8)], RE[,-c(7,8)])

RE$popup <- paste("<p><h2>", RE$name,"</p></h2>",
                  "<p>", RE$sector,"</p>",
                  "<p>", RE$address,"</p>",
                  "<p>", RE$phone,"</p>",
                  "<p>Services: ", RE$re_lib_serves,"</p>",
                  "<p>Materials that can be recycled: ", RE$re_mat,"</p>",
                  "<p>Facility type: ", RE$re_type,"</p>")


################## 6: SOCIAL SERVICES ##################
SS <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/SS.csv")

SS$y <- as.numeric(SS$y)
SS$x <- as.numeric(SS$x)

SS.SP <- SpatialPointsDataFrame(SS[,c(7,8)], SS[,-c(7,8)])

SS$popup <- paste("<p><h2>", SS$name,"</p></h2>",
                  "<p>", SS$sector,"</p>",
                  "<p>", SS$address,"</p>",
                  "<p>", SS$phone,"</p>",
                  "<p>", SS$hours,"</p>")


#################### COUNTIES POLYGONS ######################
counties <- readOGR("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/polygons/1EKY_counties.shp")


####################################################### UI ###########################################################
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(titleWidth = 400, title = "Controls"),
  dashboardSidebar(width = 400
                 # checkboxGroupInput("checkGroup", label = h3("Recycleables"), 
                  #                    choices = list("Glass" = RE$GL, "Aluminum" = RE$AL, "Plastics" = RE$PL),
                   #                   selected = 0)
                   ),
  dashboardBody(
  #  fluidRow(box(width = 12, leafletOutput(outputId = "map"))),
   # fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table"))),
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    leafletOutput("map")
  )
)


####################################################### SERVER ###########################################################
server <- function(input, output) {
  
  # check: https://stackoverflow.com/questions/38932230/reactive-mapping-after-checkboxgroupinput 
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -83.5, lat = 37.6, zoom = 8.5)  %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addPolygons(data = counties,
                  color = "green",
                  weight = 1,
                  fillOpacity = .1,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "green",
                    fillOpacity = .3)) %>% 
   #   addMarkers(data = FM,
    #             lng = ~x, lat = ~y, 
     #            label = lapply(FM$popup, HTML),
    #             group = "farmer",
     #            clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
    #  addMarkers(data = EO,
     #            lng = ~x, lat = ~y, 
      #           label = lapply(EO$popup, HTML),
       #          group = "ext",
        #         clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
      #addMarkers(data = SS,
       #          lng = ~x, lat = ~y, 
        #         label = lapply(SS$popup, HTML),
         #        group = "social",
          #       clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
      #addMarkers(data = LIB,
       #          lng = ~x, lat = ~y, 
        #         label = lapply(LIB$popup, HTML),
         #        group = "library",
          #       clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
      #addMarkers(data = GS,
       #          lng = ~x, lat = ~y, 
        #         label = lapply(GS$popup, HTML),
         #        group = "grocery",
          #       clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
      addMarkers(data = RE,
                 lng = ~x, lat = ~y, 
                 label = lapply(RE$popup, HTML),
                 group = "recycle",
                 clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
    addLayersControl(baseGroups = c("Esri.WorldImagery", "Toner"),
                     overlayGroups = c("recycle"),
                     options = layersControlOptions(collapsed = TRUE))
  })
}
  
 
################################################################################################################
shinyApp(ui = ui, server = server)
