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


# Get Data ----------------------------------------------------------------

MyTab <-  read_delim("AllCodedDataW_forEviAtlas.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)


# Add shortened versions of author lists
MyTab$author_list2 <- substr(MyTab$author_list, start = 1, stop = 20)
MyTab$author_list2 <-  ifelse(nchar(MyTab$author_list)>20, 
                              paste0(MyTab$author_list2, " [...]"),
                              MyTab$author_list2)



# UI ----------------------------------------------------------------------

ui <- dashboardPage(
                    
  
  
          dashboardHeader(title = "Arctic Herbivory Systematic Map",
                                    titleWidth = 400),
                    
          dashboardSidebar(disable = T),

                    
          
          dashboardBody(
           
# Include custom CSS to add padding and auto-opacity to filtering-box
    tags$head(
      includeCSS("styles.css")
      ),
          


# The Map -----------------------------------------------------------------

    box(width = NULL,
            leafletOutput('theMap')),
                 
# Abs.Panel ---------------------------------------------------------------

    absolutePanel(id = "controls", 
                  class = "panel panel-default", 
                  fixed = F,
                  draggable = TRUE, 
                  top = 100, left = 50, right = 'auto', bottom = "auto",
                  width = 370, height = "auto",
                              
                   # Changing the colour of the slider from blue to orange
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: orange} .irs-from, .irs-to, .irs-single {background: orange }")),   


# . Year and Colour ---------------------------------------------------------


        fluidRow(
          column(width = 7,
         sliderInput("year", 
                     label = h5("Publication year"), 
                     min = 1940, #min(MyTab$year), 
                     max = 2020, #min(MyTab$year), 
                     value = c(1945, 2019),
                     step=1, sep="", ticks=F, width = "100%",
                     animate=F
                     )),
         column(width=5,
       selectInput("colour", h5("Choose colouring variable"), 
                    choices = c("country", "language", "herbivore_type", 
                                "effect_type", "experimental_design", 
                                "study_method", "study_design"),
            selected = "country"))),

      
      
      
# . Filters -----------------------------------------------------------------


       fluidRow(
                   

# .. Country ---------------------------------------------------------------


  tabBox(width = NULL, id = 'filters', selected = "Language",
              tabPanel('Country',
               column(width = 6,
       checkboxGroupInput(inputId = "country", 
                                      label = NULL,
                                     choices = unique(MyTab$country)[1:5],
                                       selected = unique(MyTab$country)[1:5],
                                                  width = NULL)),
       column(width = 6,
              checkboxGroupInput(inputId = "country2", 
                                 label = NULL,
                                 choices = unique(MyTab$country)[6:10],
                                 selected = unique(MyTab$country)[6:10],
                                 width = NULL))) ,
            

# .. Language --------------------------------------------------------------


       tabPanel('Language',
                   checkboxGroupInput(inputId = "language", 
                                      label = NULL,
                                      choices = unique(MyTab$language),
                                      selected = unique(MyTab$language),
                                      width = NULL)),
               

# .. Herbivore -------------------------------------------------------------

             
       tabPanel('Herbivore',
                column(width=6,  
                 checkboxGroupInput(inputId = "herbivore", 
                                      label = NULL,
                                      choices = unique(MyTab$herbivore_type)[1:5],
                                      selected = unique(MyTab$herbivore_type)[1:5],
                                      width = NULL)),
                column(width=6,  
                       checkboxGroupInput(inputId = "herbivore2", 
                                          label = NULL,
                                          choices = unique(MyTab$herbivore_type)[6:10],
                                          selected = unique(MyTab$herbivore_type)[6:10],
                                          width = NULL))),


# .. study design ----------------------------------------------------------

  tabPanel('Type',
           checkboxGroupInput(
             inputId = 'studydesign',
             label=NULL,
             choices = unique(MyTab$study_design),
             selected = unique(MyTab$study_design),
             width=NULL
           )),


# .. study method ----------------------------------------------------------

  tabPanel('Method',
      column(width=6,  
           checkboxGroupInput(
             inputId = 'studymethod',
             label=NULL,
             choices = unique(MyTab$study_method)[1:4],
             selected = unique(MyTab$study_method)[1:4],
             width=NULL
           )),
      column(width = 6,
             checkboxGroupInput(
               inputId = 'studymethod2',
               label=NULL,
               choices = unique(MyTab$study_method)[5:7],
               selected = unique(MyTab$study_method)[5:7],
               width=NULL))),

# .. ex. design ------------------------------------------------------------

  tabPanel('Design',
           checkboxGroupInput(
             inputId = 'expdesign',
             label=NULL,
             choices = unique(MyTab$experimental_design),
             selected = unique(MyTab$experimental_design),
             width=NULL
           ))


  )# TabBox
 ) # fluid row
), # abs.panel1




# Remaining datapoints ----------------------------------------------------


absolutePanel(id = "remaining", 
              class = "panel panel-default", 
              fixed = F,
              draggable = TRUE, 
              top = 420, left = 'auto', right = 40, bottom = "auto",
              width = 150, height = "auto",
              verbatimTextOutput('remaining')),

# Lower Tabbox ------------------------------------------------------------

     
tabBox(width = NULL, id = 'additionals',


# . count cases -----------------------------------------------------------

          
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
     

# . trends ----------------------------------------------------------------

     
         tabPanel('Trends',
                  h5("Output is reactive to the filtering in the above map"),
                  radioButtons(inputId = "uni2", 
                               label = "",
                               choices = c("year")), 
                  plotOutput('trends')),


# . pairwise plots --------------------------------------------------------

     
     #    tabPanel('Pairwise Plots',
     #             h5("Coming soon....")),



# . lookup table ----------------------------------------------------------

         tabPanel('Lookup table',
                  selectInput('feature', 'Type the evidence point ID of the record you wan to show. Get the unique ID by first clicking on the point on the map.', 
                choices = MyTab$evidence_point_ID, multiple=FALSE, selectize=TRUE),
                  tableOutput('oneFeature')         
                  ),


# . responsive table ------------------------------------------------------

         
         tabPanel('Responsive table',
                  h5("This table is responsive to the filters applied in the map"),
                  DTOutput('responsiveTable'))
         
         ),  # End panel box


# Download ----------------------------------------------------------------



h4("Press the download button to download a speadsheet copy of raw data:"),
downloadButton("downloadData", "Download")

                
) # body

 
 
) # page





# SERVER ------------------------------------------------------------------



server <- function(input, output, session){ 

# LOOKUP TABLE -------------------------------------------

  oneFeatureTab <- reactive({
    Value <- t(MyTab[MyTab$evidence_point_ID==input$feature,])
    Variable <- rownames(Value)
    temp <- as.data.frame(cbind(Variable, Value))
    colnames(temp) <- c("Variable", "Value")
    temp
  })
  
  output$oneFeature <- renderTable(
    oneFeatureTab(), rownames = FALSE
    )
  

# FILTERED DATASET -------------------------------------
# First, I need a combine the two inputs for country, herbivore type and studymethod:

country        <-   reactive({c(input$country,        input$country2)})  
herbivore_type <-   reactive({c(input$herbivore, input$herbivore2)})   
studymethod    <-   reactive({c(input$studymethod,    input$studymethod2)})   
  


  datR <- reactive({
    MyTab[
      dplyr::between(MyTab$year, input$year[1], input$year[2]) &
        MyTab$country %in% country() &
        MyTab$language %in% input$language &
        MyTab$herbivore_type %in% herbivore_type() &
        MyTab$study_design %in% input$studydesign &
        MyTab$study_method %in% studymethod() &
        MyTab$experimental_design %in% input$expdesign
      ,]
    
  })
  
  

# Remaining datapoints ----------------------------------------------------
  output$remaining <- renderText(paste("Datapoints: ", nrow(datR())))

  
# THE MAP ####
  output$theMap <- renderLeaflet({
      
      
    
      # Convert to spatial...
      dat2 <- sp::SpatialPointsDataFrame(coords = datR()[,c("coordinates_E","coordinates_N")], 
                     data = datR(),
                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      
      theMap <- mapview::mapview(dat2,
                                 layer.name = "Evidence Point",
                                 map.types = c("Esri.WorldShadedRelief",
                                               "Esri.WorldImagery"),
                                 cex = 5, 
                                 #lwd = 0,
                                 alpha.regions = 0.5,
                                 #col.regions = "blue",
                                 zcol = input$colour,
                                 popup = leafpop::popupTable(dat2, 
                                                    row.numbers = F, feature.id = F,
                                                    zcol = c("evidence_point_ID",
                                                             "author_list2",
                                                             "year",
                                                             "journal",
                                                             "locality",
                                                             "elevation",
                                                             "study_design",
                                                             "experimental_design",
                                                             "herbivore_type",
                                                             "study_method",
                                                             "effect_type")),
                                 legend=T
                                 )
                               
      theMap@map
      
    })
    
  
# Responsive Table ####
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
  
  
# Counts ####
  output$univ <- renderPlot({
    datF <- datR()
    ggplot(data = datF, aes_string(x=input$uni))+
      geom_bar()+
      coord_flip()+
      theme_bw()+
      theme(text = element_text(size = 20))
  })
  
  
# pairwise ####
#output$univ <- renderPlot({
#  
#  ggplot(data = datR(), aes_string(x=input$myX, y=input$myY))+
#    ???
#    theme_bw()+
#    theme(text = element_text(size = 20))
#})
  

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

