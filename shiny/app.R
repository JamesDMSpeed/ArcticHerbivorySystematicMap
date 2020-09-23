library(shiny)
library(shinydashboard)
library(shinyjs)
#library(shinyWidgets)
library(DT)
#library(httr)
library(readr)
library(leaflet)
library(mapview)
library(dplyr)
library(ggplot2)
library(raster)
library(sp)
#library(eply)

MyTab <-  read_delim("AllCodedDataW_forEviAtlas.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)


# Add shortened versions of author lists
MyTab$author_list2 <- substr(MyTab$author_list, start = 1, stop = 20)
MyTab$author_list2 <-  ifelse(nchar(MyTab$author_list)>20, 
                              paste0(MyTab$author_list2, " [...]"),
                              MyTab$author_list2)



ui <- dashboardPage(
                    
  
  
          dashboardHeader(title = "Arctic Herbivory Systematic Map",
                                    titleWidth = 400),
                    
          dashboardSidebar(disable = T),

                    
          
          dashboardBody(
           
            #div(class="outer",
                tags$head(
                  # Include our custom CSS
                  includeCSS("styles.css")
                  ),
          
          #  column(width=12,
                             
                    box(width = NULL,
                        leafletOutput('theMap')),
                   
                absolutePanel(id = "controls", 
                              class = "panel panel-default", 
                              fixed = TRUE,
                              draggable = TRUE, 
                              top = 80, left = "auto", right = 50, bottom = "auto",
                              width = 230, height = "auto",
                              
                   # Changing the colour of the slider from blue to orange
                   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: orange} .irs-from, .irs-to, .irs-single {background: orange }")),   
                   
                   
                                       
               sliderInput("year", 
                           label = h5("Publication year"), 
                           min = 1945, #min(MyTab$year), 
                           max = 2019, #min(MyTab$year), 
                           value = c(1945, 2019),
                           sep="", ticks=F, width = "100%"),
               
               box(title = "Country", 
                   width = NULL, collapsible = T, collapsed = T,
                               #tabPanel("Page 2",    
                               checkboxGroupInput(inputId = "country", 
                                                  label = "Country",
                                                  choices = unique(MyTab$country),
                                                  selected = unique(MyTab$country),
                                                  width = NULL)),
               box(title = "Language", 
                   width = NULL, collapsible = T, collapsed = T,
                   checkboxGroupInput(inputId = "language", 
                                      label = "Language",
                                      choices = unique(MyTab$language),
                                      selected = unique(MyTab$language),
                                      width = NULL)),
               
               box(title = "Herbivore type", 
                   width = NULL, collapsible = T, collapsed = T,
                   checkboxGroupInput(inputId = "herbivore", 
                                      label = "Herbivore type",
                                      choices = unique(MyTab$herbivore_type),
                                      selected = unique(MyTab$herbivore_type),
                                      width = NULL))
                            
                            
                   ), # abs.panel1
             #  ) # column1

     # ), # div
    
     box(title = "Figures", 
         width = NULL, collapsible = T, collapsed = F,
          tabBox(width = NULL, id = 'figs', selected = NULL,
                 tabPanel('Count cases',
                          h5("Output is reactive to the filtering in the above map"),
                          radioButtons(inputId = "uni", 
                                       label = "",
                                       choices = c("country", 
                                                   "study_design",
                                                   "study_method",
                                                   "extent_of_spatial_scale",
                                                   "temporal_resolution",
                                                   "measured_response_variable",
                                                   "herbivore_type",
                                                   "herbivory_season",
                                                   "effect_type",
                                                   "management_herbivore",
                                                   "conservation_herbivore",
                                                   "management"
                                                   )),
                          plotOutput('univ')),     
                 tabPanel('Trends',
                          h5("Output is reactive to the filtering in the above map"),
                          radioButtons(inputId = "uni2", 
                                       label = "",
                                       choices = c("year")), 
                          plotOutput('trends')),
                 tabPanel('Pairwise Plots',
                          h5("Coming soon....")
                          )    
                 
          )
         ),
      box(title = "Lookup table", 
          width = NULL, collapsible = T, collapsed = T,
     verbatimTextOutput('featurePrint'),
     selectInput('feature', 'Input FeatureID', 1:nrow(MyTab), multiple=FALSE, selectize=TRUE),
     tableOutput('oneFeature'))
         
                 
                
              ) # body



                             
                             
 
 
) # page


server <- function(input, output, session) {
  
  
  output$oneFeature <- renderTable(t(MyTab[input$feature,]), rownames = TRUE)
  
  # this needs to be made an interactive element
  
  datR <- reactive({
    MyTab[
      dplyr::between(MyTab$year, input$year[1], input$year[2]) &
        MyTab$country %in% input$country &
        MyTab$language %in% input$language &
        MyTab$herbivore_type %in% input$herbivore
      ,]
    
  })
  
  output$theMap <- renderLeaflet({
      
      datM <- datR()
    
      # Convert to spatial...
      dat2 <- sp::SpatialPointsDataFrame(coords = datM[,c("coordinates_E","coordinates_N")], 
                     data = datM,
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
                                                    zcol = c("author_list2",
                                                                   "year",
                                                                   "journal")),
                                 legend=F
                                 )
                               
      theMap@map
      
    })
    
  output$featurePrint <- renderPrint(input$feature)
  
  
  output$univ <- renderPlot({
    datF <- datR()
    ggplot(data = datF, aes_string(x=input$uni))+
      geom_bar()+
      coord_flip()+
      theme_bw()+
      theme(text = element_text(size = 20))
  })
  
  
  output$trends <- renderPlot({
    
    ggplot(data = datR(), aes_string(x=input$uni2))+
      geom_histogram()+
      theme_bw()+
      theme(text = element_text(size = 20))
  })
}

shinyApp(ui = ui, server = server)

