# LIBRARY ####
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


# GET DATA ####
MyTab <-  read_delim("AllCodedDataW_forEviAtlas.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)


# Add shortened versions of author lists
MyTab$author_list2 <- substr(MyTab$author_list, start = 1, stop = 20)
MyTab$author_list2 <-  ifelse(nchar(MyTab$author_list)>20, 
                              paste0(MyTab$author_list2, " [...]"),
                              MyTab$author_list2)


# UI ####
ui <- dashboardPage(
                    
  
  
          dashboardHeader(title = "Arctic Herbivory Systematic Map",
                                    titleWidth = 400),
                    
          dashboardSidebar(disable = T),

                    
          
          dashboardBody(
           
# Include custom CSS to add padding and auto-opacity to filtering-box
    tags$head(
      includeCSS("styles.css")
      ),
          

# THE MAP #####                              
    box(width = NULL,
            leafletOutput('theMap')),
                 
# ABS. PANEL   ####
    absolutePanel(id = "controls", 
                  class = "panel panel-default", 
                  fixed = F,
                  draggable = TRUE, 
                  top = 110, left = "auto", right = 50, bottom = "auto",
                  width = 300, height = "auto",
                              
                   # Changing the colour of the slider from blue to orange
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: orange} .irs-from, .irs-to, .irs-single {background: orange }")),   
                   
# CHOOSE YEARS ####                  
         sliderInput("year", 
                     label = h5("Publication year"), 
                     min = 1940, #min(MyTab$year), 
                     max = 2020, #min(MyTab$year), 
                     value = c(1945, 2019),
                     step=1, sep="", ticks=F, width = "100%",
                     animate=F
                     ),
               
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

     
# FIGURES ####
     box(title = "Figures", 
         width = NULL, collapsible = T, collapsed = T,
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
      
     
# LOOKUP TABLE ####     
     box(title = "Lookup table", 
          width = NULL, collapsible = T, collapsed = T,
      #verbatimTextOutput('featurePrint'),
     selectInput('feature', 'Type the FeatureID of the record you wan to show', 
                 1:nrow(MyTab), multiple=FALSE, selectize=TRUE),
     tableOutput('oneFeature')),

# RESPONSIVE TABLE ####
box(title = "Responsive table", 
    width = NULL, collapsible = T, collapsed = T,
    h5("This table is responsive to the filters applied in the map"),
    DTOutput('responsiveTable')),

h4("Press the download button to download a speadsheet copy of raw data:"),
downloadButton("downloadData", "Download")

         
                 
                
              ) # body



                             
                             
 
 
) # page


# SERVER ####
server <- function(input, output, session) {

  
# LOOKUP TABLE
  oneFeatureTab <- reactive({
    Value <- t(MyTab[input$feature,])
    featureID <- rownames(Value)
    temp <- as.data.frame(cbind(featureID, Value))
    colnames(temp) <- c("featureID", "Value")
    temp
  })
  
  output$oneFeature <- renderTable(
    oneFeatureTab(), rownames = FALSE
    )
  

# FILTERED DATASET ####
  datR <- reactive({
    MyTab[
      dplyr::between(MyTab$year, input$year[1], input$year[2]) &
        MyTab$country %in% input$country &
        MyTab$language %in% input$language &
        MyTab$herbivore_type %in% input$herbivore
      ,]
    
  })
  
  
# THE MAP ####
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
    
  
# RESPONSIVE TABLE ####
  #tempTable <- reactive({
  #  t <- as.data.frame(datR())
  #  t$comment <- substr(t$comment, start = 1, stop = 30)
  #  t$comment <- ifelse(nchar(t$comment)>30, 
  #         paste0(t$comment, " [...]"),
  #         MyTab$author_list2) 
  #})
  output$responsiveTable <- 
    renderDataTable({
      DT::datatable(datR(), 
        options = list(
          scrollX = TRUE,
          columnDefs = list(list(
         targets = "_all",
           render = JS(
             "function(data, type, row, meta) {",
             "return type === 'display' && data != null && data.length > 30 ?",
           "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                "}")
         ))),
      class = "display")
})
  
  output$univ <- renderPlot({
    datF <- datR()
    ggplot(data = datF, aes_string(x=input$uni))+
      geom_bar()+
      coord_flip()+
      theme_bw()+
      theme(text = element_text(size = 20))
  })
  

# TRENDS ####
  output$trends <- renderPlot({
    
    ggplot(data = datR(), aes_string(x=input$uni2))+
      geom_histogram()+
      theme_bw()+
      theme(text = element_text(size = 20))
  })
  
# DOWNLOAD ####
  output$downloadData <- downloadHandler(
    filename = function() {"ArcticHerbivorySystematicMap.csv"},
    content = function(file) {
      write.csv( MyTab, file,  row.names = FALSE)}
  )

}
shinyApp(ui = ui, server = server)

