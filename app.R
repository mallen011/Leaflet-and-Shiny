library(shiny)
library(leaflet)
library(sp)
library(rgdal)
library(htmltools)
library(DT)
library(ggplot2)
library(shinydashboard)
library(shinythemes)


###### SET PATH HERE #####

####################################### ADD ALL CSV FILES + POPUPS ###################################################
All <- read.csv("csv/All.csv")

################## 0: FP ##############################
FP <- read.csv("csv/FP.csv")

FP$y <- as.numeric(FP$y)
FP$x <- as.numeric(FP$x)

FP.SP <- SpatialPointsDataFrame(FP[,c(7,8)], FP[,-c(7,8)])

FP$popup <- paste("<p><h2>", FP$name,"</p></h2>",
                  FP$website, "<br />",
                  "<h3>Address:</h3>", FP$address,"<br />",
                  "<h3>Phone number:</h3>", FP$phone,"<br />",
                  "<h3>Hours:</h3> ", FP$hours,"<br />",
                  "<h3>SNAP/EBT benefits?</h3> ", FP$snap,"<br />",
                  "<h3>WIC benefits?</h3> ", FP$wic)
FP_icon <- makeIcon("icons/conveniencestore.png")


FP_snap <-read.csv("csv/FP_snap.csv")
FP_snap$y <- as.numeric(FP_snap$y)
FP_snap$x <- as.numeric(FP_snap$x)
FP_snap.SP <- SpatialPointsDataFrame(FP_snap[,c(7,8)], FP_snap[,-c(7,8)])
FP_snap$popup <- paste("<p><h2>", FP_snap$name,"</p></h2>",
                       FP_snap$website,"<br />",
                       "<h3>Address:</h3>", FP_snap$address,"<br />",
                       "<h3>Phone number:</h3>", FP_snap$phone,"<br />",
                      "<h3>Hours:</h3> ", FP_snap$hours)

FP_snap_icon <- makeIcon("icons/fruits.png")

FP_wic <- read.csv("csv/FP_wic.csv")
FP_wic$y <- as.numeric(FP_wic$y)
FP_wic$x <- as.numeric(FP_wic$x)
FP_wic.SP <- SpatialPointsDataFrame(FP_wic[,c(7,8)], FP_wic[,-c(7,8)])
FP_wic$popup <- paste("<p><h2>", FP_wic$name,"</p></h2>",
                        FP_wic$website, "<br />",
                      "<h3>Address:</h3>", FP_wic$address,"<br />",
                      "<h3>Phone number:</h3>", FP_wic$phone,"<br />",
                  "<h3>Hours:</h3> ", FP_wic$hours)

FP_wic_icon <- makeIcon("icons/grocery.png")

FP_fm_senior <- read.csv("csv/FP_fm_senior.csv")
FP_fm_senior$y <- as.numeric(FP_fm_senior$y)
FP_fm_senior$x <- as.numeric(FP_fm_senior$x)
FP_fm_senior.SP <- SpatialPointsDataFrame(FP_fm_senior[,c(7,8)], FP_fm_senior[,-c(7,8)])
FP_fm_senior$popup <- paste("<p><h2>", FP_fm_senior$name,"</p></h2>",
                              FP_fm_senior$website, "<br />",
                            "<h3>Address: </h3>", FP_fm_senior$address,"<br />",
                            "<h3>Phone number: </h3>", FP_fm_senior$phone,"<br />",
                      "<h3>Hours:</h3> ", FP_fm_senior$hours)

FP_fm_senior_icon <- makeIcon("icons/farmstand.png")

################## 1: GROCERY #########################
GS <- read.csv("csv/GS .csv")

GS$y <- as.numeric(GS$y)
GS$x <- as.numeric(GS$x)

GS.SP <- SpatialPointsDataFrame(GS[,c(7,8)], GS[,-c(7,8)])

GS$popup <- paste("<p><h2>", GS$name,"</p></h2>",
                   GS$website,"<br />",
                  "<h3>Address:</h3>", GS$address,"<br />",
                  "<h3>Phone number:</h3>", GS$phone,"<br />",
                  "<h3>Hours:</h3> ", GS$hours,"<br />",
                  "<h3>SNAP/EBT benefits? </h3>", GS$snap,"<br />",
                  "<h3>WIC benefits?</h3> ", GS$wic)

GS_icon <- makeIcon("icons/supermarket.png")

################## 2: FARMERS #########################
FM <- read.csv("csv/FM.csv")

FM$y <- as.numeric(FM$y)
FM$x <- as.numeric(FM$x)

FM.SP <- SpatialPointsDataFrame(FM[,c(7,8)], FM[,-c(7,8)])

FM$popup <- paste("<p><h2>", FM$name,"</p></h2>",
                  FM$website,"<br />",
                  "<h3>Address:</h3>", FM$address,"<br />",
                  "<h3>Phone number:</h3>", FM$phone,"<br />",
                  "<h3>SNAP/EBT benefits?</h3> ", FM$snap,"<br />",
                  "<h3>WIC benefits?</h3> ", FM$wic,"<br />",
                  "<h3>Senior vouchers?</h3> ", FM$fm_senior
                  )

FM_icon <- makeIcon("icons/farmstand.png")


################## 3: LIBRARIES #######################
LIB <- read.csv("csv/LIB.csv")

LIB$y <- as.numeric(LIB$y)
LIB$x <- as.numeric(LIB$x)

LIB.SP <- SpatialPointsDataFrame(LIB[,c(7,8)], LIB[,-c(7,8)])

LIB$popup <- paste("<p><h2>", LIB$name,"</p></h2>",
                   LIB$website,"<br />",
                   "<h3>Address:</h3>", LIB$address,"<br />",
                   "<h3>Phone number:</h3>", LIB$phone,"<br />",
                   "<h3>Hours: </h3>", LIB$hours,"<br />",
                   "<h3>Services:</h3> ", LIB$re_lib_serves)

LIB_icon <- makeIcon("icons/library.png")
  
LIB_free_wifi <- read.csv("csv/LIB_free_wifi.csv")
LIB_free_wifi$y <- as.numeric(LIB_free_wifi$y)
LIB_free_wifi$x <- as.numeric(LIB_free_wifi$x)
LIB_free_wifi.SP <- SpatialPointsDataFrame(LIB_free_wifi[,c(7,8)], LIB_free_wifi[,-c(7,8)])

LIB_free_wifi$popup <- paste("<p><h2>", LIB_free_wifi$name,"</p></h2>",
                    LIB_free_wifi$website,"<br />",
                    "<h3>Address:</h3>", LIB_free_wifi$address,"<br />",
                    "<h3>Phone number:</h3>", LIB_free_wifi$phone,"<br />",
                   "<h3>Hours: </h3>", LIB_free_wifi$hours,"<br />",
                   "</h3>Services:</h3> ", LIB_free_wifi$re_lib_serves)
LIB_free_wifi_icon <- makeIcon("icons/wi-fi-2.png")

LIB_com_access<- read.csv("csv/LIB_com_access.csv")
LIB_com_access$y <- as.numeric(LIB_com_access$y)
LIB_com_access$x <- as.numeric(LIB_com_access$x)
LIB_com_access.SP <- SpatialPointsDataFrame(LIB_com_access[,c(7,8)], LIB_com_access[,-c(7,8)])

LIB_com_access$popup <- paste("<p><h2>", LIB_com_access$name,"</p></h2>",
                    LIB_com_access$website,"<br />",
                   "<h3>Address:</h3>", LIB_com_access$address,"<br />",
                   "<h3>Phone number:</h3>", LIB_com_access$phone,"<br />",
                   "<h3>Hours: </h3>", LIB_com_access$hours,"<br />",
                   "<h3>Services: </h3> ", LIB_com_access$re_lib_serves)
LIB_com_access_icon <- makeIcon("icons/computers.png")

LIB_bookmobile <- read.csv("csv/LIB_bookmobile.csv")
LIB_bookmobile$y <- as.numeric(LIB_bookmobile$y)
LIB_bookmobile$x <- as.numeric(LIB_bookmobile$x)
LIB_bookmobile.SP <- SpatialPointsDataFrame(LIB_bookmobile[,c(7,8)], LIB_bookmobile[,-c(7,8)])

LIB_bookmobile$popup <- paste("<p><h2>", LIB_bookmobile$name,"</p></h2>",
                   LIB_bookmobile$website,"<br />",
                   "<h3>Address: </h3>", LIB_bookmobile$address, "<br />",
                   "<h3>Phone number: </h3>", LIB_bookmobile$phone,"<br />",
                   "<h3>Hours: </h3>", LIB_bookmobile$hours,"<br />",
                   "<h3>Services: </h3> ", LIB_bookmobile$re_lib_serves)
LIB_bookmobile_icon <- makeIcon("icons/truck3.png")

LIB_meeting_rooms <- read.csv("csv/LIB_meeting_rooms.csv")
LIB_meeting_rooms$y <- as.numeric(LIB_meeting_rooms$y)
LIB_meeting_rooms$x <- as.numeric(LIB_meeting_rooms$x)
LIB_meeting_rooms.SP <- SpatialPointsDataFrame(LIB_meeting_rooms[,c(7,8)], LIB_meeting_rooms[,-c(7,8)])

LIB_meeting_rooms$popup <- paste("<p><h2>", LIB_meeting_rooms$name,"</p></h2>",
                      LIB_meeting_rooms$website,"<br />",
                   "<h3>Address: </h3>", LIB_meeting_rooms$address,"<br />",
                   "<h3>Phone number: </h3>", LIB_meeting_rooms$phone,"<br />",
                   "<h3>Hours: </h3>", LIB_meeting_rooms$hours,"<br />",
                   "<h3>Services: </h3>", LIB_meeting_rooms$re_lib_serves)
LIB_meeting_rooms_icon <- makeIcon("icons/glazer.png")

LIB_e_audio <- read.csv("csv/LIB_e_audio.csv")
LIB_e_audio$y <- as.numeric(LIB_e_audio$y)
LIB_e_audio$x <- as.numeric(LIB_e_audio$x)
LIB_e_audio.SP <- SpatialPointsDataFrame(LIB_e_audio[,c(7,8)], LIB_e_audio[,-c(7,8)])

LIB_e_audio$popup <- paste("<p><h2>", LIB_e_audio$name,"</p></h2>",
                  LIB_e_audio$website,"<br />",
                  "<h3>Address: </h3>", LIB_e_audio$address,"<br />",
                  "<h3>Phone number: </h3>", LIB_e_audio$phone,"<br />",
                  "<h3>Hours: </h3> ", LIB_e_audio$hours,"<br />",
                  "<h3>Services: </h3> ", LIB_e_audio$re_lib_serves)
LIB_e_audio_icon <- makeIcon("icons/audio.png")

LIB_test_prep <- read.csv("csv/LIB_test_prep.csv")
LIB_test_prep$y <- as.numeric(LIB_test_prep$y)
LIB_test_prep$x <- as.numeric(LIB_test_prep$x)
LIB_test_prep.SP <- SpatialPointsDataFrame(LIB_test_prep[,c(7,8)], LIB_test_prep[,-c(7,8)])

LIB_test_prep$popup <- paste("<p><h2>", LIB_test_prep$name,"</p></h2>",
                   LIB_test_prep$website,"<br />",
                   "<h3>Address: </h3>", LIB_test_prep$address,"<br />",
                   "<h3>Phone number: </h3>", LIB_test_prep$phone,"<br />",
                   "<h3>Hours: </h3>", LIB_test_prep$hours,"<br />",
                   "<h3>Services:</h3> ", LIB_test_prep$re_lib_serves)
LIB_test_prep_icon <- makeIcon("icons/test-2.png")

################## 4: EXT OFFICES ######################
EO <- read.csv("csv/EO.csv")

EO$y <- as.numeric(EO$y)
EO$x <- as.numeric(EO$x)

EO.SP <- SpatialPointsDataFrame(EO[,c(7,8)], EO[,-c(7,8)])

EO$popup <- paste("<p><h2>", EO$name,"</p></h2>",
                  EO$website,"<br />",
                  "<h3>Address: </h3>", EO$address,"<br />",
                  "<h3>Phone number: </h3>", EO$phone,"<br />",
                  "<h3>Hours:</h3> ", EO$hours)

EO_icon <- makeIcon("icons/office-building.png")

################## 5: RECYCLE #########################
RE <- read.csv("csv/RE.csv")

RE$y <- as.numeric(RE$y)
RE$x <- as.numeric(RE$x)

RE.SP <- SpatialPointsDataFrame(RE[,c(7,8)], RE[,-c(7,8)])

RE$popup <- paste("<p><h2>", RE$name,"</p></h2>",
                    RE$website, "<br />",
                  "<h3>Address: </h3>", RE$address,"<br />",
                  "<h3>Phone number: </h3>", RE$phone,"<br />",
                  "<h3>Services:</h3> ", RE$re_lib_serves,"<br />",
                  "<h3>Materials that can be recycled: </h3> ", RE$re_mat,"<br />",
                  "<h3>Facility type:</h3> ", RE$re_type)
#Glass <- RE[RE$GL=="Yes", ]
#Aluminum <- RE[RE$AL=="Yes", ]
#Plastic <- RE[RE$PL=="Yes", ]

RE_icon <- makeIcon("icons/recycle.png")

################## 6: SOCIAL SERVICES ##################
SS <- read.csv("csv/SS .csv")

SS$y <- as.numeric(SS$y)
SS$x <- as.numeric(SS$x)

SS.SP <- SpatialPointsDataFrame(SS[,c(8,9)], SS[,-c(8,9)])

SS$popup <- paste("<p><h2>", SS$name,"</p></h2>",
                  SS$website,"<br />",
                  "<h3>Address: </h3>", SS$address,"<br />",
                  "<h3>Phone number: </h3>", SS$phone,"<br />",
                  "<h3>Hours:</h3> ", SS$hours)

SS_icon <- makeIcon("icons/communitycentre.png")

SS_families <- read.csv("csv/SS_families.csv")

SS_families$y <- as.numeric(SS_families$y)
SS_families$x <- as.numeric(SS_families$x)

SS_families.SP <- SpatialPointsDataFrame(SS_families[,c(8,9)], SS_families[,-c(8,9)])

SS_families$popup <- paste("<p><h2>", SS_families$name,"</p></h2>",
                    SS_families$website,"<br />",
                  "<h3>Address: </h3>", SS_families$address,"<br />",
                  "<h3>Phone number: </h3>", SS_families$phone,"<br />",
                  "<h3>Hours:</h3> ", SS_families$hours)
SS_families_icon <- makeIcon("icons/family.png")

SS_community <- read.csv("csv/SS_community.csv")

SS_community$y <- as.numeric(SS_community$y)
SS_community$x <- as.numeric(SS_community$x)

SS_community.SP <- SpatialPointsDataFrame(SS_community[,c(8,9)], SS_community[,-c(8,9)])

SS_community$popup <- paste("<p><h2>", SS_community$name,"</p></h2>",
                    SS_community$website,"<br />",
                  "<h3>Address: </h3>", SS_community$address,"<br />",
                  "<h3>Phone number: </h3>", SS_community$phone,"<br />",
                  "<h3>Hours: </h3>", SS_community$hours)
SS_community_icon <- makeIcon("icons/group-2.png")


SS_careers <- read.csv("csv/SS_careers.csv")

SS_careers$y <- as.numeric(SS_careers$y)
SS_careers$x <- as.numeric(SS_careers$x)

SS_careers.SP <- SpatialPointsDataFrame(SS_careers[,c(8,9)], SS_careers[,-c(8,9)])

SS_careers$popup <- paste("<p><h2>", SS_careers$name,"</p></h2>",
                  SS_careers$website,"<br />",
                  "<h3>Address: </h3>", SS_careers$address,"<br />",
                  "<h3>Phone number: </h3>", SS_careers$phone,"<br />",
                  "<h3>Hours: </h3>", SS_careers$hours)

SS_careers_icon <- makeIcon("icons/findajob.png")

#################### COUNTIES POLYGONS ######################
counties <- readOGR("polygons/1EKY_counties.shp")
counties1 <- read.csv("csv/counties.csv")
counties1$y <- as.numeric(counties1$y)
counties1$x <- as.numeric(counties1$x)
counties1.SP <- SpatialPointsDataFrame(counties1[,c(3,4)], counties1[,-c(3,4)])

####################################################### UI ###########################################################
ui <- dashboardPage(
  dashboardHeader(title = "Community Resources of Eastern Kentucky", titleWidth = 600, tags$li(class = "dropdown",
                                                                                               actionButton("code","Click here to download the mapping code!",
                                                                                                                                onclick ="window.open('https://github.com/mallen011/Leaflet-and-Shiny', '_blank')"))),
  dashboardSidebar(
        sidebarMenu(menuItem("All Resources", tabName = "EKY_Resources"),
                   menuItem("Fresh Produce", tabName = "fresh_produce"),
                   menuItem("Recycling Centers", tabName = "recycling_centers"),
                   menuItem("Social Services", tabName = "social_services"),
                   menuItem("Libraries", tabName = "libraries"),
                   box( width = 200, status = "primary", solidHeader = TRUE, title = "Reporting",
                   HTML("Almost all data presented <br />
                                on these maps are personally- <br />
                                collected, making them <br />
                                vulnerable to error. If you find <br />
                                a mistake, please click <a href='https://docs.google.com/forms/d/e/1FAIpQLSeWATA-cUeOKw5QWAG_Ba4q9sH_T5yLahIZ9NeVaanWM8Ns-w/viewform?usp=sf_link'><button type='button'>here</button> </a><br/ >
                                to report it. Thank you!")))
                   ),

  dashboardBody(
    tags$head(
      includeCSS("shiny_style.css"),
      includeCSS("https://fonts.googleapis.com/css?family=Lora&display=swap"),
      includeCSS("https://fonts.googleapis.com/css2?family=Roboto:wght@300&display=swap")
),


tabItems( 

############## Map: EKY RESOURCES ALL ################
tabItem(tabName = "EKY_Resources",
      leafletOutput("map", height = 700),

      absolutePanel(id = "controls1", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 22, bottom = "auto", height = "auto",
               selectInput(inputId = "county", label = "Select your county", choices = (All$county)),
               selectInput(inputId = "sector", label = "Select service type", choices = NULL),
               selectInput(inputId = "name", label = "Name", choices = NULL),
                   actionButton("zoom2location","Take me there!"),
                   selected = 1),
),

############## Map2: FRESH PRODUCE ################
tabItem(tabName = "fresh_produce",
              
        leafletOutput("map2", height = 700),
        
        absolutePanel(id = "controls2", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 22, bottom = "auto",
              width = 330, height = "auto",
              selectInput(inputId = "county0", label = "Select your county", choices = unique(counties1$CNTY_NAME)),
              actionButton(inputId = "zoom2location2","Take me there!")
)
),

############## Map3:RECYCLING CENTERS ################
tabItem(tabName = "recycling_centers",
        
    leafletOutput("map3", height = 700),

    absolutePanel(id = "controls3", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 22, bottom = "auto",
              width = 330, height = "auto",
              selectInput("select1", "Recycling Options",c("All Locations","Glass", "Aluminum" ,"Plastic", "Ferrous metals", "Non-ferrous metals",
                                                           "Cardboard", "Mixed office paper", "Sorted office paper", "Cans", "Newspapers", "Electronics",
                                                           "Used oil", "Mixed rigid plastics", "Car batteries", "Tires", "Antifreeze/ Ethol glycol")), 
              uiOutput("select2"),
              actionButton("zoom2location00","Take me there!"))
),

############## Map4: SOCIAL SERVICES ################
tabItem(tabName = "social_services",

     leafletOutput("map4", height = 700),

     absolutePanel(id = "controls4", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 22, bottom = "auto",
                   height = "auto",
                        selectInput(inputId = "county3", label = "Select your county", choices = unique(counties1$CNTY_NAME)),
                        actionButton(inputId = "zoom2location3","Take me there!")
)
),

############## Map6: LIBRARIES ################
tabItem(tabName = "libraries",

      leafletOutput("map6", height = 700),

      absolutePanel(id = "controls5", fixed = TRUE, draggable = TRUE, top = 60, left = "auto", right = 22, bottom = "auto",
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
  
###################### observe eky_resources tab filter all layers #########################
   
   observe({
     updateSelectInput(session, "sector", "Select service type", 
                       choices = (All$sector[All$county==input$county]))
   })
   
   observe({
     updateSelectInput(session, "name", "Name", 
                       choices = (All$name[All$county==input$county & All$sector==input$sector]))
   })
   
   
###################### Output map EKY_Resources #########################
   groups <- c("<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/farmstand.png'> Farmer Markets", 
               "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/supermarket.png'>Grocery Stores", 
               "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/office-building.png'>Cooperative Extension Offices",
               "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/communitycentre.png'>Social Services", 
               "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/library.png'>Libraries",
               "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/recycle.png'>Recycling Centers")
   
   
    output$map <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -83.5, lat = 37.6, zoom = 8)  %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
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
                 group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/farmstand.png'> Farmer Markets",
                 clusterOptions = markerClusterOptions(
                   showCoverageOnHover = FALSE,
                   iconCreateFunction = JS("
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
                 group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/office-building.png'>Cooperative Extension Offices",
                 clusterOptions = markerClusterOptions(
                   showCoverageOnHover = FALSE,
                   iconCreateFunction = JS("
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
                 group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/communitycentre.png'>Social Services",
                 clusterOptions = markerClusterOptions(
                   showCoverageOnHover = FALSE,
                   iconCreateFunction = JS("
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
                 group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/library.png'>Libraries",
                 clusterOptions = markerClusterOptions(
                   showCoverageOnHover = FALSE,
                   iconCreateFunction = JS("
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
                 group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/supermarket.png'>Grocery Stores",
                 clusterOptions = markerClusterOptions(
                   showCoverageOnHover = FALSE,
                   iconCreateFunction = JS("
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
                 group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/recycle.png'>Recycling Centers",
                 clusterOptions = markerClusterOptions(
                   showCoverageOnHover = FALSE,
                   iconCreateFunction = JS("
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

    
###################### Observe Zoom2location button EKY_resources#########################     
    
    observeEvent(input$zoom2location, {
      srow <- All[All$name == input$name,]
      leafletProxy("map")   %>%
        setView(
          lat = srow$y,
          lng = srow$x,
          zoom = 16.25)
    })


###################### Output map2 - Fresh Produce #########################
 
   output$map2 <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -83.5, lat = 37.6, zoom = 8)  %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
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
                 group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/conveniencestore.png'>All stores & markets selling fresh produce",
                 clusterOptions = markerClusterOptions(
                   showCoverageOnHover = FALSE,
                   iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = FP_snap,
                  lng = FP_snap$x, lat = FP_snap$y,
                  popup = lapply(FP_snap$popup, HTML),
                  group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/fruits.png'>Stores & markets accepting SNAP/EBT benefits",
                  icon = FP_snap_icon,
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = FP_wic,
                  lng = FP_wic$x, lat = FP_wic$y,
                  popup = lapply(FP_wic$popup, HTML),
                  group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/grocery.png'>Stores & markets accepting WIC benefits",
                  icon = FP_wic_icon,
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addMarkers(data = FP_fm_senior,
                  lng = FP_fm_senior$x, lat = FP_fm_senior$y,
                  popup = lapply(FP_fm_senior$popup, HTML),
                  group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/farmstand.png'>Farmers' markets accepting senior vouchers",
                  icon = FP_fm_senior_icon,
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
                                function(cluster) {
                                  return new L.DivIcon({
                                    html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                      className: 'marker-cluster'
                                    });
                                   }"))) %>% 
       
      addLayersControl("bottomright", baseGroups = c("Esri.WorldImagery", "Toner"),
                       overlayGroups = c("<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/conveniencestore.png'>All stores & markets selling fresh produce",
                                         "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/fruits.png'>Stores & markets accepting SNAP/EBT benefits",
                                         "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/grocery.png'>Stores & markets accepting WIC benefits",
                                         "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/farmstand.png'>Farmers' markets accepting senior vouchers"
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
      setView(lng = -83.5, lat = 37.6, zoom = 8)  %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
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
                 group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/recycle.png'>Recycling centers",
                 icon = RE_icon,
                 clusterOptions = markerClusterOptions(
                   showCoverageOnHover = FALSE,
                   iconCreateFunction = JS("
                                function(cluster) {
                                  return new L.DivIcon({
                                    html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                      className: 'marker-cluster'
                                    });
                                   }"))) %>% 
      addLayersControl("bottomright", baseGroups = c("Esri.WorldImagery", "Toner"), 
                       overlayGroups = c("<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/recycle.png'>Recycling centers"),
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
      setView(lng = -83.5, lat = 37.6, zoom = 8)  %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
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
                 icon = SS_icon,
                 group = "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/communitycentre.png'>All social services",
                 clusterOptions = markerClusterOptions(
                   showCoverageOnHover = FALSE,
                   iconCreateFunction = JS("
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
                  group = "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/family.png'>Family Services",
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
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
                  group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/group-2.png'>Community Services",
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
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
                  group = "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/findajob.png'>Career Services",
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>%
      addLayersControl("bottomright", baseGroups = c("Esri.WorldImagery", "Toner"),
                       overlayGroups = c("<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/communitycentre.png'>All social services",
                                         "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/family.png'>Family Services",
                                         "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/group-2.png'>Community Services",
                                         "<img src='https://experiential-binder.000webhostapp.com/EKY_Resources/findajob.png'>Career Services"), 
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
       setView(lng = -83.5, lat = 37.6, zoom = 8)  %>% 
       addProviderTiles("Esri.WorldImagery") %>% 
       addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
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
                  group = "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/library.png'>All libraries",
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
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
                  group = "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/truck3.png'>Bookmobile and outreach services",
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
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
                  group = "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/wi-fi-2.png'>Free Wifi",
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
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
                  group = "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/computers.png'>Open computer access",
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
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
                  group = "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/glazer.png'>Meeting rooms available",
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
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
                  group = "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/audio.png'>Ebooks and audio books available",
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
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
                  group = "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/test-2.png'>Test preparation resources",
                  clusterOptions = markerClusterOptions(
                    showCoverageOnHover = FALSE,
                    iconCreateFunction = JS("
                                          function(cluster) {
                                          return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>% 
       addLayersControl("bottomright", baseGroups = c("Esri.WorldImagery", "Toner"),
                        overlayGroups =c("<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/library.png'>All libraries",
                                         "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/truck3.png'>Bookmobile and outreach services",
                                         "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/wi-fi-2.png'>Free Wifi",
                                         "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/computers.png'>Open computer access",
                                         "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/glazer.png'>Meeting rooms available",
                                         "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/audio.png'>Ebooks and audio books available", 
                                         "<img src= 'https://experiential-binder.000webhostapp.com/EKY_Resources/test-2.png'>Test preparation resources"),
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
   

}


##################################################################################################################################
shinyApp(ui = ui, server = server)
