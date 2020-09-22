library(shiny)
library(shinydashboard)
#library(shinyWidgets)
library(DT)
#library(httr)
library(readr)
library(leaflet)
library(mapview)

MyTab <-  read_delim("AllCodedDataW_forEviAtlas.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)



ui <- dashboardPage(
                    
          dashboardHeader(title = "Systematic Map",
                                    titleWidth = 400),
                    
          dashboardSidebar(disable = T),
                    
          
          dashboardBody(
                
            column(width=12,
                             
                    box(width = NULL,
                        leafletOutput('theMap')),
                   
                    DTOutput('filtered')
                             
                             
                      ))
                    
)


server <- function(input, output, session) {
  
  output$filtered <- renderDT(
    MyTab, selection = 'single', options = list(scrollX = TRUE))
  
  output$theMap <- renderLeaflet({
      dat <- MyTab[ , ]  # add subsetts here. Shorten author list and title.
      dat2 <- sp::SpatialPointsDataFrame(coords = dat[,c("coordinates_E","coordinates_N")], 
                     data = dat,
                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      
      theMap <- mapview::mapview(dat2,
                                 layer.name = "Evidence Point",
                                 map.types = c("Esri.WorldShadedRelief",
                                               "Esri.WorldImagery"),
                                 cex = 5, lwd = 0,
                                 alpha.regions = 0.5,
                                 col.regions = "blue",
                                 popup = leafpop::popupTable(dat2, 
                                                    row.numbers = F,
                                                    zcol = c("author_list",
                                                                   "year",
                                                                   "journal"))
                                 
                                 #label = dat2$title
                                 )
                               
      theMap@map
      
    })
    
    
  
 
  
}

shinyApp(ui = ui, server = server)

