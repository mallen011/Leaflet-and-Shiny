library(shiny)
library(leaflet)
library(sp)
library(rgdal)
library(htmltools)
library(DT)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
path <- "C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/"
setwd(path)

####################################### ADD ALL CSV FILES + POPUPS ###################################################
All <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/All.csv")

################## 0: FP ##############################
FP <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/FP.csv")

FP$y <- as.numeric(FP$y)
FP$x <- as.numeric(FP$x)

FP.SP <- SpatialPointsDataFrame(FP[,c(7,8)], FP[,-c(7,8)])

FP$popup <- paste("<p><h2>", FP$name,"</p></h2>",
                  "<p>", FP$sector,"</p>",
                  "<p>", FP$address,"</p>",
                  "<p>", FP$phone,"</p>",
                  "<p>Hours: ", FP$hours,"</p>",
                  "<p>Snap benefits? ", FP$snap,"</p>",
                  "<p>WIC benefits? ", FP$wic,"</p>",
                  "<p>", FP$website, "</p>")
FP_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/conveniencestore.png")


FP_snap <-read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/FP_snap.csv")
FP_snap$y <- as.numeric(FP_snap$y)
FP_snap$x <- as.numeric(FP_snap$x)
FP_snap.SP <- SpatialPointsDataFrame(FP_snap[,c(7,8)], FP_snap[,-c(7,8)])
FP_snap$popup <- paste("<p><h2>", FP_snap$name,"</p></h2>",
                      "<p>", FP_snap$address,"</p>",
                      "<p>", FP_snap$phone,"</p>",
                      "<p>Hours: ", FP_snap$hours,"</p>",
                      "<p>", FP_snap$website, "</p>")

FP_snap_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/fruits.png")

FP_wic <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/FP_wic.csv")
FP_wic$y <- as.numeric(FP_wic$y)
FP_wic$x <- as.numeric(FP_wic$x)
FP_wic.SP <- SpatialPointsDataFrame(FP_wic[,c(7,8)], FP_wic[,-c(7,8)])
FP_wic$popup <- paste("<p><h2>", FP_wic$name,"</p></h2>",
                  "<p>", FP_wic$address,"</p>",
                  "<p>", FP_wic$phone,"</p>",
                  "<p>Hours: ", FP_wic$hours,"</p>",
                  "<p>", FP_wic$website, "</p>")

FP_wic_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/grocery.png")

FP_fm_senior <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/FP_fm_senior.csv")
FP_fm_senior$y <- as.numeric(FP_fm_senior$y)
FP_fm_senior$x <- as.numeric(FP_fm_senior$x)
FP_fm_senior.SP <- SpatialPointsDataFrame(FP_fm_senior[,c(7,8)], FP_fm_senior[,-c(7,8)])
FP_fm_senior$popup <- paste("<p><h2>", FP_fm_senior$name,"</p></h2>",
                      "<p>", FP_fm_senior$address,"</p>",
                      "<p>", FP_fm_senior$phone,"</p>",
                      "<p>Hours: ", FP_fm_senior$hours,"</p>",
                      "<p>", FP_fm_senior$website, "</p>")

FP_fm_senior_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/farmstand.png")

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
                  "<p>WIC benefits? ", GS$wic,"</p>",
                  "<p>", GS$website, "</p>")

GS_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/supermarket.png")

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
                  "<p>Senior vouchers? ", FM$fm_senior,"</p>",
                  "<p>", FM$website, "</p>")

FM_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/farmstand.png")


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
                   "<p>Services: ", LIB$re_lib_serves,"</p>",
                   "<p>", LIB$website, "</p>")

LIB_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/library.png")
  
LIB_free_wifi <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/LIB_free_wifi.csv")
LIB_free_wifi$y <- as.numeric(LIB_free_wifi$y)
LIB_free_wifi$x <- as.numeric(LIB_free_wifi$x)
LIB_free_wifi.SP <- SpatialPointsDataFrame(LIB_free_wifi[,c(7,8)], LIB_free_wifi[,-c(7,8)])

LIB_free_wifi$popup <- paste("<p><h2>", LIB_free_wifi$name,"</p></h2>",
                   "<p>", LIB_free_wifi$sector,"</p>",
                   "<p>", LIB_free_wifi$address,"</p>",
                   "<p>", LIB_free_wifi$phone,"</p>",
                   "<p>", LIB_free_wifi$hours,"</p>",
                   "<p>Services: ", LIB_free_wifi$re_lib_serves,"</p>",
                   "<p>", LIB_free_wifi$website, "</p>")
LIB_free_wifi_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/wi-fi-2.png")

LIB_com_access <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/LIB_com_access.csv")
LIB_com_access$y <- as.numeric(LIB_com_access$y)
LIB_com_access$x <- as.numeric(LIB_com_access$x)
LIB_com_access.SP <- SpatialPointsDataFrame(LIB_com_access[,c(7,8)], LIB_com_access[,-c(7,8)])

LIB_com_access$popup <- paste("<p><h2>", LIB_com_access$name,"</p></h2>",
                   "<p>", LIB_com_access$sector,"</p>",
                   "<p>", LIB_com_access$address,"</p>",
                   "<p>", LIB_com_access$phone,"</p>",
                   "<p>", LIB_com_access$hours,"</p>",
                   "<p>Services: ", LIB_com_access$re_lib_serves,"</p>",
                   "<p>", LIB_com_access$website, "</p>")
LIB_com_access_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/computers.png")

LIB_bookmobile <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/LIB_bookmobile.csv")
LIB_bookmobile$y <- as.numeric(LIB_bookmobile$y)
LIB_bookmobile$x <- as.numeric(LIB_bookmobile$x)
LIB_bookmobile.SP <- SpatialPointsDataFrame(LIB_bookmobile[,c(7,8)], LIB_bookmobile[,-c(7,8)])

LIB_bookmobile$popup <- paste("<p><h2>", LIB_bookmobile$name,"</p></h2>",
                   "<p>", LIB_bookmobile$sector,"</p>",
                 "<p>", LIB_bookmobile$address,"</p>",
                   "<p>", LIB_bookmobile$phone,"</p>",
                   "<p>", LIB_bookmobile$hours,"</p>",
                   "<p>Services: ", LIB_bookmobile$re_lib_serves,"</p>",
                   "<p>", LIB_bookmobile$website, "</p>")
LIB_bookmobile_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/truck3.png")

LIB_meeting_rooms <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/LIB_meeting_rooms.csv")
LIB_meeting_rooms$y <- as.numeric(LIB_meeting_rooms$y)
LIB_meeting_rooms$x <- as.numeric(LIB_meeting_rooms$x)
LIB_meeting_rooms.SP <- SpatialPointsDataFrame(LIB_meeting_rooms[,c(7,8)], LIB_meeting_rooms[,-c(7,8)])

LIB_meeting_rooms$popup <- paste("<p><h2>", LIB_meeting_rooms$name,"</p></h2>",
                   "<p>", LIB_meeting_rooms$sector,"</p>",
                   "<p>", LIB_meeting_rooms$address,"</p>",
                   "<p>", LIB_meeting_rooms$phone,"</p>",
                   "<p>", LIB_meeting_rooms$hours,"</p>",
                   "<p>Services: ", LIB_meeting_rooms$re_lib_serves,"</p>",
                   "<p>", LIB_meeting_rooms$website, "</p>")
LIB_meeting_rooms_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/glazer.png")

LIB_e_audio <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/LIB_e_audio.csv")
LIB_e_audio$y <- as.numeric(LIB_e_audio$y)
LIB_e_audio$x <- as.numeric(LIB_e_audio$x)
LIB_e_audio.SP <- SpatialPointsDataFrame(LIB_e_audio[,c(7,8)], LIB_e_audio[,-c(7,8)])

LIB_e_audio$popup <- paste("<p><h2>", LIB_e_audio$name,"</p></h2>",
                   "<p>", LIB_e_audio$sector,"</p>",
                   "<p>", LIB_e_audio$address,"</p>",
                   "<p>", LIB_e_audio$phone,"</p>",
                   "<p>", LIB_e_audio$hours,"</p>",
                   "<p>Services: ", LIB_e_audio$re_lib_serves,"</p>",
                   "<p>", LIB_e_audio$website, "</p>")
LIB_e_audio_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/audio.png")

LIB_test_prep <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/LIB_test_prep.csv")
LIB_test_prep$y <- as.numeric(LIB_test_prep$y)
LIB_test_prep$x <- as.numeric(LIB_test_prep$x)
LIB_test_prep.SP <- SpatialPointsDataFrame(LIB_test_prep[,c(7,8)], LIB_test_prep[,-c(7,8)])

LIB_test_prep$popup <- paste("<p><h2>", LIB_test_prep$name,"</p></h2>",
                   "<p>", LIB_test_prep$sector,"</p>",
                   "<p>", LIB_test_prep$address,"</p>",
                   "<p>", LIB_test_prep$phone,"</p>",
                   "<p>", LIB_test_prep$hours,"</p>",
                   "<p>Services: ", LIB_test_prep$re_lib_serves,"</p>",
                   "<p>", LIB_test_prep$website, "</p>")
LIB_test_prep_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/test-2.png")

################## 4: EXT OFFICES ######################
EO <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/EO.csv")

EO$y <- as.numeric(EO$y)
EO$x <- as.numeric(EO$x)

EO.SP <- SpatialPointsDataFrame(EO[,c(7,8)], EO[,-c(7,8)])

EO$popup <- paste("<p><h2>", EO$name,"</p></h2>",
                  "<p>", EO$sector,"</p>",
                  "<p>", EO$address,"</p>",
                  "<p>", EO$phone,"</p>",
                  "<p>", EO$hours,"</p>",
                  "<p>", EO$website,"</p>")

EO_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/office-building.png")

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
                  "<p>Facility type: ", RE$re_type,"</p>",
                  "<p>", RE$website,"</p>")
#Glass <- RE[RE$GL=="Yes", ]
#Aluminum <- RE[RE$AL=="Yes", ]
#Plastic <- RE[RE$PL=="Yes", ]

RE_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/recycle.png")

################## 6: SOCIAL SERVICES ##################
SS <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/SS .csv")

SS$y <- as.numeric(SS$y)
SS$x <- as.numeric(SS$x)

SS.SP <- SpatialPointsDataFrame(SS[,c(8,9)], SS[,-c(8,9)])

SS$popup <- paste("<p><h2>", SS$name,"</p></h2>",
                  "<p>", SS$sector,"</p>",
                  "<p>", SS$address,"</p>",
                  "<p>", SS$phone,"</p>",
                  "<p>", SS$hours,"</p>",
                  "<p>", SS$website,"</p>")

SS_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/communitycentre.png")

SS_families <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/SS_families.csv")

SS_families$y <- as.numeric(SS_families$y)
SS_families$x <- as.numeric(SS_families$x)

SS_families.SP <- SpatialPointsDataFrame(SS_families[,c(8,9)], SS_families[,-c(8,9)])

SS_families$popup <- paste("<p><h2>", SS_families$name,"</p></h2>",
                  "<p>", SS_families$sector,"</p>",
                  "<p>", SS_families$address,"</p>",
                  "<p>", SS_families$phone,"</p>",
                  "<p>", SS_families$hours,"</p>",
                  "<p>", SS_families$website,"</p>")
SS_families_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/family.png")

SS_community <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/SS_community.csv")

SS_community$y <- as.numeric(SS_community$y)
SS_community$x <- as.numeric(SS_community$x)

SS_community.SP <- SpatialPointsDataFrame(SS_community[,c(8,9)], SS_community[,-c(8,9)])

SS_community$popup <- paste("<p><h2>", SS_community$name,"</p></h2>",
                  "<p>", SS_community$sector,"</p>",
                  "<p>", SS_community$address,"</p>",
                  "<p>", SS_community$phone,"</p>",
                  "<p>", SS_community$hours,"</p>",
                  "<p>", SS_community$website,"</p>")
SS_community_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/group-2.png")


SS_careers <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/SS_careers.csv")

SS_careers$y <- as.numeric(SS_careers$y)
SS_careers$x <- as.numeric(SS_careers$x)

SS_careers.SP <- SpatialPointsDataFrame(SS_careers[,c(8,9)], SS_careers[,-c(8,9)])

SS_careers$popup <- paste("<p><h2>", SS_careers$name,"</p></h2>",
                  "<p>", SS_careers$sector,"</p>",
                  "<p>", SS_careers$address,"</p>",
                  "<p>", SS_careers$phone,"</p>",
                  "<p>", SS_careers$hours,"</p>",
                  "<p>", SS_careers$website,"</p>")

SS_careers_icon <- makeIcon("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/icons/findajob.png")

#################### COUNTIES POLYGONS ######################
counties <- readOGR("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/polygons/1EKY_counties.shp")
counties1 <- read.csv("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/csv/counties.csv")
counties1$y <- as.numeric(counties1$y)
counties1$x <- as.numeric(counties1$x)
counties1.SP <- SpatialPointsDataFrame(counties1[,c(3,4)], counties1[,-c(3,4)])

####################################################### UI ###########################################################
ui <- dashboardPage(
  dashboardHeader(title = "Community Resources of Eastern Kentucky", titleWidth = 600),
  dashboardSidebar(
        sidebarMenu(menuItem("EKY Resources", tabName = "EKY_Resources"),
                   menuItem("Fresh Produce", tabName = "fresh_produce"),
                   menuItem("Recycling Centers", tabName = "recycling_centers"),
                   menuItem("Social Services", tabName = "social_services"),
                   menuItem("Libraries", tabName = "libraries")
                   )
),
  
  dashboardBody(
    tags$head(
      includeCSS("C:/Users/Clown Baby/Desktop/GIS/Shiny Leaflet Map/shiny_style.css"),
      includeCSS("https://fonts.googleapis.com/css?family=Lora&display=swap"),
      includeCSS("https://fonts.googleapis.com/css2?family=Roboto:wght@300&display=swap")
),


tabItems( 

############## Map: EKY RESOURCES ALL ################
tabItem(tabName = "EKY_Resources",
      leafletOutput("map", height = 900),

      absolutePanel(id = "controls1", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", height = "auto",
               selectInput(inputId = "county", label = "Select your county", choices = unique(All$county)),
               selectInput(inputId = "sector", label = "Select service type", choices = NULL),
               selectInput(inputId = "name", label = "Name", choices = NULL),
                   actionButton("zoom2location","Take me there!"),
                   selected = 1),
),

############## Map2: FRESH PRODUCE ################
tabItem(tabName = "fresh_produce",
              
        leafletOutput("map2", height = 900),
        
        absolutePanel(id = "controls2", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
              width = 330, height = "auto",
              selectInput(inputId = "county0", label = "Select your county", choices = unique(counties1$CNTY_NAME)),
              actionButton(inputId = "zoom2location2","Take me there!")
)
),

############## Map3:RECYCLING CENTERS ################
tabItem(tabName = "recycling_centers",
        
    leafletOutput("map3", height = 900),

    absolutePanel(id = "controls3", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
              width = 330, height = "auto",
              selectInput("select1", "Recycling Options",c("All Locations","Glass", "Aluminum" ,"Plastic", "Ferrous metals", "Non-ferrous metals",
                                                           "Cardboard", "Mixed office paper", "Sorted office paper", "Cans", "Newspapers", "Electronics",
                                                           "Used oil", "Mixed rigid plastics", "Car batteries", "Tires", "Antifreeze/ Ethol glycol")), 
              uiOutput("select2"),
              actionButton("zoom2location00","Take me there!"))
),

############## Map4: SOCIAL SERVICES ################
tabItem(tabName = "social_services",

     leafletOutput("map4", height = 900),

     absolutePanel(id = "controls4", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                   height = "auto",
                        selectInput(inputId = "county3", label = "Select your county", choices = unique(counties1$CNTY_NAME)),
                        actionButton(inputId = "zoom2location3","Take me there!")
)
),

############## Map6: LIBRARIES ################
tabItem(tabName = "libraries",

      leafletOutput("map6", height = 900),

      absolutePanel(id = "controls5", fixed = TRUE, draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    height = "auto",
                        selectInput(inputId = "county4", label = "Select your county", choices = unique(counties1$CNTY_NAME)),
                        actionButton(inputId = "zoom2location4","Take me there!")
)
)
)
)
)


####################################################### SERVER ###########################################################
server <- function(session, input, output) {

  ###################### observe eky_resources tab filter all layers #########################
  
   observe({
   updateSelectInput(session, "sector", "sector", 
                     choices = unique(All$sector[All$county==input$county]))
 })
  
   observe({
    updateSelectInput(session, "name", "name", 
                      choices = unique(All$name[All$county==input$county & All$sector==input$sector]))
  })
 
###################### output menu -- default tab on EKY_Resources #########################
   
   output$menu <- renderMenu({
     dashboardSidebarMenu(
       menuItem("EKY Resources", tabName="EKY_resources"))
       menuItem("Fresh Produce", tabName = "fresh_produce")
       menuItem("Recycling Centers", tabName = "recycling_centers")
       menuItem("Social Services", tabName = "social_services")
       menuItem("Extension Offices", tabName = "extension_offices")
       menuItem("Libraries", tabName = "libraries")
})
   isolate({updateTabItems(session, "tabs", "eky_resources")})
  

###################### Output map EKY_Resources #########################
   groups <- c("<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/farmstand.png' height='40' width='40'> Farmer Markets", 
               "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/supermarket.png' height='40' width='40'>Grocery Stores", 
               "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/office-building.png' height='40' width='40'>Cooperative Extension Offices",
               "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/communitycentre.png' height='40' width='40'>Social Services", 
               "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/library.png' height='40' width='40'>Libraries",
               "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/recycle.png' height='40' width='40'>Recycling Centers")
   
    output$map <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -83.5, lat = 37.6, zoom = 8.5)  %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addPolygons(data = counties,
                  color = "#339966",
                  weight = 1,
                  fillOpacity = .1,
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#339966",
                    fillOpacity = .2)) %>% 
      addMarkers(data = FM,
                 lng = ~x, lat = ~y, 
                 popup = lapply(FM$popup, HTML),
                 icon = FM_icon,
                 group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/farmstand.png' height='40' width='40'> Farmer Markets",
                 clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
      addMarkers(data = EO,
                 lng = ~x, lat = ~y, 
                 popup = lapply(EO$popup, HTML),
                 icon = EO_icon,
                 group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/office-building.png' height='40' width='40'>Cooperative Extension Offices",
                 clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
      addMarkers(data = SS,
                 lng = ~x, lat = ~y, 
                 popup = lapply(SS$popup, HTML),
                 icon = SS_icon,
                 group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/communitycentre.png' height='40' width='40'>Social Services",
                 clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
      addMarkers(data = LIB,
                 lng = ~x, lat = ~y, 
                 popup = lapply(LIB$popup, HTML),
                 icon = LIB_icon,
                 group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/library.png' height='40' width='40'>Libraries",
                 clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
      addMarkers(data = GS,
                 lng = ~x, lat = ~y, 
                 popup = lapply(GS$popup, HTML),
                 icon = GS_icon,
                 group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/supermarket.png' height='40' width='40'>Grocery Stores",
                 clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
    addMarkers(data = RE,
                 lng = RE$x, lat = RE$y, 
                 popup = lapply(RE$popup, HTML),
                 icon = RE_icon,
                 group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/recycle.png' height='40' width='40'>Recycling Centers",
                 clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
    addLayersControl("bottomright", baseGroups = c("Esri.WorldImagery", "Toner"),
                     overlayGroups = groups,
                     options = layersControlOptions(collapsed = FALSE)) 
  })

###################### Output map2 - Fresh Produce #########################
 
   output$map2 <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -83.5, lat = 37.6, zoom = 8.5)  %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addPolygons(data = counties,
                  color = "#339966",
                  weight = 1,
                  fillOpacity = .1,
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#339966",
                    fillOpacity = .2)) %>% 
      addMarkers(data = FP,
                 lng = ~x, lat = ~y, 
                 popup = lapply(FP$popup, HTML),
                 icon = FP_icon,
                 group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/conveniencestore.png' height='40' width='40'>All stores & markets selling fresh produce",
                 clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = FP_snap,
                  lng = FP_snap$x, lat = FP_snap$y,
                  popup = lapply(FP_snap$popup, HTML),
                  group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/fruits.png' height='40' width='40'>Stores & markets accepting SNAP/EBT benefits",
                  icon = FP_snap_icon,
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = FP_wic,
                  lng = FP_wic$x, lat = FP_wic$y,
                  popup = lapply(FP_wic$popup, HTML),
                  group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/grocery.png' height='40' width='40'>Stores & markets accepting WIC benefits",
                  icon = FP_wic_icon,
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = FP_fm_senior,
                  lng = FP_fm_senior$x, lat = FP_fm_senior$y,
                  popup = lapply(FP_fm_senior$popup, HTML),
                  group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/farmstand.png' height='40' width='40'>Farmers' markets accepting senior vouchers",
                  icon = FP_fm_senior_icon,
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                function(cluster) {
                                  return new L.DivIcon({
                                    html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                      className: 'marker-cluster'
                                    });
                                   }"))) %>% 
       
      addLayersControl("bottomright", baseGroups = c("Esri.WorldImagery", "Toner"),
                       overlayGroups = c("<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/conveniencestore.png' height='40' width='40'>All stores & markets selling fresh produce",
                                         "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/fruits.png' height='40' width='40'>Stores & markets accepting SNAP/EBT benefits",
                                         "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/grocery.png' height='40' width='40'>Stores & markets accepting WIC benefits",
                                         "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/farmstand.png' height='40' width='40'>Farmers' markets accepting senior vouchers"
                                        ),
                      options = layersControlOptions(collapsed = FALSE)
                ) 
})
 
     observeEvent(input$zoom2location2, {
       req(input$county0)
       srow2 <- counties1[counties1$CNTY_NAME == input$county0,]
       isolate({leafletProxy("map2")   %>%
           setView(
             lng = srow2$x,
             lat = srow2$y,
             zoom = 12)
       })
     })
   
###################### Output map3 - Recycling centers #########################    
     output$select2 <- renderUI({
       if(input$select1 == "All Locations"){
         selectInput("select2", "All Locations",RE$name)
       }else if(input$select1 == "Glass"){
         selectInput("select2", "Accepts Glass",RE$name[as.character(RE$GL) %in% "Yes"])
       }else if(input$select1 == "Aluminum"){
         selectInput("select2", "Accepts Aluminum",RE$name[as.character(RE$AL) %in% "Yes"])
       }else if(input$select1 == "Plastic"){
         selectInput("select2", "Accepts Plastic",RE$name[as.character(RE$PL) %in% "Yes"])
       }else if(input$select1 == "Ferrous metals"){
         selectInput("select2", "Accepts Ferrous metals",RE$name[as.character(RE$FE) %in% "Yes"])
       }else if(input$select1 == "Non-ferrous metals"){
         selectInput("select2", "Accepts Non-ferrous metals",RE$name[as.character(RE$NONFE) %in% "Yes"]) 
       }else if(input$select1 == "Cardboard"){
         selectInput("select2", "Accepts cardboard",RE$name[as.character(RE$OCC) %in% "Yes"]) 
       }else if(input$select1 == "Mixed office paper"){
       selectInput("select2", "Accepts mixed office paper",RE$name[as.character(RE$OPMIX) %in% "Yes"]) 
       }else if(input$select1 == "Sorted office paper"){
         selectInput("select2", "Accepts sorted office paper",RE$name[as.character(RE$OPSPORT) %in% "Yes"]) 
       }else if(input$select1 == "Cans"){
         selectInput("select2", "Accepts cans",RE$name[as.character(RE$ST.CANS) %in% "Yes"]) 
       }else if(input$select1 == "Electronics"){
         selectInput("select2", "Accepts Electronics",RE$name[as.character(RE$ESCRAP) %in% "Yes"]) 
       }else if(input$select1 == "Used oil"){
         selectInput("select2", "Accepts used oil",RE$name[as.character(RE$UO) %in% "Yes"])
       }else if(input$select1 == "Mixed rigid plastics"){
         selectInput("select2", "Accepts mixed rigid plastics",RE$name[as.character(RE$MRP) %in% "Yes"])
       }else if(input$select1 == "Car batteries"){
         selectInput("select2", "Accepts car batteries",RE$name[as.character(RE$CARBAT) %in% "Yes"]) 
       }else if(input$select1 == "Tires"){
         selectInput("select2", "Accepts tires",RE$name[as.character(RE$RUB) %in% "Yes"]) 
       }else if(input$select1 == "Antifreeze/ Ethol glycol"){
         selectInput("select2", "Accepts antifreeze and ethol glycol",RE$name[as.character(RE$EGC) %in% "Yes"]) 
       }
     })

######### map3 output ############ 
     
      output$map3 <- renderLeaflet({
      leaflet() %>% 
      setView(lng = -83.5, lat = 37.6, zoom = 8.5)  %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addPolygons(data = counties,
                  color = "#339966",
                  weight = 1,
                  fillOpacity = .1,
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#339966",
                    fillOpacity = .2))%>% 
      addMarkers(data = RE,
                 lng = RE$x, lat = RE$y, 
                 popup = lapply(RE$popup, HTML),
                 group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/recycle.png' height='40' width='40'>Recycling centers",
                 icon = RE_icon,
                 clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                function(cluster) {
                                  return new L.DivIcon({
                                    html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                      className: 'marker-cluster'
                                    });
                                   }"))) %>% 
      addLayersControl("bottomright", baseGroups = c("Esri.WorldImagery", "Toner"), 
                       overlayGroups = c("<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/recycle.png' height='40' width='40'>Recycling centers"),
                       options = layersControlOptions(collapsed = FALSE))

  })

      observeEvent(input$zoom2location00, {
        srow0 <- RE[RE$name == input$select2,]
        leafletProxy("map3")   %>%
          setView(
            lng = srow0$x,
            lat = srow0$y,
            zoom = 16.25  )
      })
###################### Output map4 - Social Services #########################
 
   output$map4 <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -83.5, lat = 37.6, zoom = 8.5)  %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addPolygons(data = counties,
                  color = "#339966",
                  weight = 1,
                  fillOpacity = .1,
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#339966",
                    fillOpacity = .2))%>% 
      addMarkers(data = SS,
                 lng = ~x, lat = ~y, 
                 popup = lapply(SS$popup, HTML),
                 group = "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/communitycentre.png' height='40' width='40'>All social services",
                 clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = SS_families,
                  lng = SS_families$x, lat = SS_families$y, 
                  popup = lapply(SS_families$popup, HTML),
                  icon = SS_families_icon,
                  group = "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/family.png' height='40' width='40'>Family Services",
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>%
       addMarkers(data = SS_community,
                  lng = SS_community$x, lat = SS_community$y, 
                  popup = lapply(SS_community$popup, HTML),
                  icon = SS_community_icon,
                  group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/group-2.png' height='40' width='40'>Community Services",
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>%
       addMarkers(data = SS_careers,
                  lng = SS_careers$x, lat = SS_careers$y, 
                  popup = lapply(SS_careers$popup, HTML),
                  icon = SS_careers_icon,
                  group = "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/findajob.png' height='40' width='40'>Career Services",
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>%
      addLayersControl("bottomright", baseGroups = c("Esri.WorldImagery", "Toner"),
                       overlayGroups = c("<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/communitycentre.png' height='40' width='40'>All social services",
                                         "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/family.png' height='40' width='40'>Family Services",
                                         "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/group-2.png' height='40' width='40'>Community Services",
                                         "<img src='http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/findajob.png' height='40' width='40'>Career Services"), 
                                          options = layersControlOptions(collapsed = FALSE))
  })
   
   ####### map4 SS ######
   observeEvent(input$zoom2location3, {
     req(input$county3)
    srow3 <- counties1[counties1$CNTY_NAME == input$county3,]
     leafletProxy("map4")   %>%
      setView(
       lng = srow3$x,
      lat = srow3$y,
       zoom = 12)
   })  


###################### Output map6 - Libraries ######################### 

   output$map6 <- renderLeaflet({
     leaflet() %>% 
       setView(lng = -83.5, lat = 37.6, zoom = 8.5)  %>% 
       addProviderTiles("Esri.WorldImagery") %>% 
       addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
       addProviderTiles("Esri.WorldImagery") %>% 
       addPolygons(data = counties,
                   color = "#339966",
                   weight = 1,
                   fillOpacity = .1,
                   highlight = highlightOptions(
                     weight = 2,
                     color = "#339966",
                     fillOpacity = .2))%>% 
       addMarkers(data = LIB,
                  lng = ~x, lat = ~y, 
                  popup = lapply(LIB$popup, HTML),
                  icon = LIB_icon,
                  group = "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/library.png' height='40' width='40'>All libraries",
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                         function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = LIB_bookmobile,
                  lng = LIB_bookmobile$x, lat = LIB_bookmobile$y, 
                  popup = lapply(LIB_bookmobile$popup, HTML),
                  icon = LIB_bookmobile_icon,
                  group = "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/truck3.png' height='40' width='40'>Bookmobile and public outreach services",
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = LIB_free_wifi,
                  lng = LIB_free_wifi$x, lat = LIB_free_wifi$y, 
                  popup = lapply(LIB_free_wifi$popup, HTML),
                  icon = LIB_free_wifi_icon,
                  group = "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/wi-fi-2.png' height='40' width='40'>Free Wifi",
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = LIB_com_access,
                  lng = LIB_com_access$x, lat = LIB_com_access$y, 
                  popup = lapply(LIB_com_access$popup, HTML),
                  icon = LIB_com_access_icon,
                  group = "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/computers.png' height='40' width='40'>Open computer access",
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                          }"))   ) %>% 
       addMarkers(data = LIB_meeting_rooms,
                  lng = LIB_meeting_rooms$x, lat = LIB_meeting_rooms$y, 
                  icon = LIB_meeting_rooms_icon,
                  popup = lapply(LIB_meeting_rooms$popup, HTML),
                  group = "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/glazer.png' height='40' width='40'>Meeting rooms available",
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = LIB_e_audio,
                  lng = LIB_e_audio$x, lat = LIB_e_audio$y, 
                  popup = lapply(LIB_e_audio$popup, HTML),
                  icon = LIB_e_audio_icon,
                  group = "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/audio.png' height='40' width='40'>Ebooks and audio books available",
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = LIB_test_prep,
                  lng = LIB_test_prep$x, lat = LIB_test_prep$y, 
                  popup = lapply(LIB_test_prep$popup, HTML),
                  icon = LIB_test_prep_icon,
                  group = "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/test-2.png' height='40' width='40'>Test preparation resources",
                  clusterOptions = markerClusterOptions(iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addLayersControl("bottomright", baseGroups = c("Esri.WorldImagery", "Toner"),
                        overlayGroups =c("<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/library.png' height='40' width='40'>All libraries",
                                         "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/truck3.png' height='40' width='40'>Bookmobile and public outreach services",
                                         "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/wi-fi-2.png' height='40' width='40'>Free Wifi",
                                         "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/computers.png' height='40' width='40'>Open computer access",
                                         "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/glazer.png' height='40' width='40'>Meeting rooms available",
                                         "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/audio.png' height='40' width='40'>Ebooks and audio books available", 
                                         "<img src= 'http://127.0.0.1:8887/Shiny%20Leaflet%20Map/icons/test-2.png' height='40' width='40'>Test preparation resources"),
                        options = layersControlOptions(collapsed = FALSE))
   })  

   observeEvent(input$zoom2location4, {
     req(input$county4)
     srow4 <- counties1[counties1$CNTY_NAME == input$county4,]
     leafletProxy("map6")   %>%
       setView(
         lng = srow4$x,
         lat = srow4$y,
         zoom = 12)
   })
   
###################### Observe Zoom2location button EKY_resources#########################     
   
     observeEvent(input$zoom2location, {
     srow <- All[All$name == input$name,]
     leafletProxy("map")   %>%
       setView(
               lat = srow$y,
               lng = srow$x,
         zoom = 16.25)
   })
}

################################3### broken functions/ experimental functions #####################################


################ functions for recycling #############
# observe({
#  y <- All$county[All$sector == input$sector]
# updateSelectInput(session, "name", "name", choices = y)

#})

#  output$select3 <- renderUI({
#   if(input$select1 == "All Locations"){
#    selectInput("select2", "Any Services",RE$re_lib_serves)
#  }else if(input$select1 == "Glass"){
#   selectInput("select2", "Accepts Glass",RE$name[as.character(RE$GL) %in% "Yes"])
#  }else if(input$select1 == "Aluminum"){
#   selectInput("select2", "Accepts Aluminum",RE$name[as.character(RE$AL) %in% "Yes"])
#  }else if(input$select1 == "Plastic"){
#   selectInput("select2", "Accepts Plastic",RE$name[as.character(RE$PL) %in% "Yes"])
#  }
# })



#  filtered_data <- reactive({
#    if(input$RE_check["Glass" == Glass, ])
#})


############## zoom to location experimenting ###############
# observeEvent(input$zoom2location, {
#  srow <- RE[RE$name == input$select2,]
# leafletProxy("map")   %>%
#  setView(
#   lng = srow$x,
#  lat = srow$y,
# zoom = 16.25  )
#})
#} 


############## proxy map for observe function in recycling map ############
#observe(
# leafletProxy("map", data = filtered_data) %>% 
#        clearMarkers() %>% 
#       addMarkers()
#)

#proxy <- leafletProxy("map")

#observe(input$RE_check{ 
#    proxy %>% 
# clearGroup("recycle")
# addMarkers(data = Glass,
#            lat = ~y, lng = ~x,
#            group = "glass")
#})

#observe({ 
#  if(input$Glass == TRUE){
#   proxy %>% showGroup("glass") }
#  else { proxy %>% 
#     hideGroup("glass")
#    } 
# })


# observeEvent({
# x <- input$RE_check
#  if (is.null(x))
#   x <- character(0)

#  if(x == "Glass"){
#   leafletProxy("map") %>% 
#    clearMarkers() %>% 
#   addMarkers(data = Glass,
#             lat = ~y,
#            lng = ~x
#    )

#}
#})

#################### add to output map for recycle
#   addMarkers(data = RE[RE$GL=="Yes", ],
#               lng = ~x, lat = ~y, 
#              label = lapply(Glass$popup, HTML),
#            group = "glass",
#            clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 

##################################################################################################################################
shinyApp(ui = ui, server = server)
