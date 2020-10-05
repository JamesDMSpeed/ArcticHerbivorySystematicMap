# LIBRARY ####
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(readr)
library(leaflet)
library(mapview)
library(dplyr)
library(ggplot2)
library(sp)


# Get Data ----------------------------------------------------------------

MyTab <- read_csv("AllCodedData_withGIScontext.csv")

# Add shortened versions of author lists, but first, change wierd column name
if(!"author_list" %in% names(MyTab)) colnames(MyTab)[2] <- "author_list"
MyTab$author_list <- as.character(MyTab$author_list)
MyTab$language <- as.character(MyTab$language)
MyTab$author_list2 <- substr(MyTab$author_list, start = 1, stop = 20)
MyTab$author_list2 <-  ifelse(nchar(MyTab$author_list)>20, 
                              paste0(MyTab$author_list2, " [...]"),
                              MyTab$author_list2)
# Fix alternate spelling
MyTab$language[MyTab$language == "English"] <- "english"
# Make a colums whihc is later used in the filter by species function
MyTab$incl <- NULL
# Scale species richness whihc is currently as proportions
MyTab$ArcticHerbivore_Species.richness <- MyTab$ArcticHerbivore_Species.richness*70
MyTab[,c("bio1", 
         "bio2",
         "bio5",
         "bio6",
         "bio7",
         "bio8",
         "bio9",
         "bio10",
         "bio11")] <- MyTab[,c("bio1", 
                               "bio2",
                               "bio5",
                               "bio6",
                               "bio7",
                               "bio8",
                               "bio9",
                               "bio10",
                               "bio11")]/10

MyTab <- rename(MyTab,
                "Annual_Mean_Temperature" = bio1,
                "Mean_Diurnal_Range" = bio2,
                "Isothermality" = bio3,
                "Temperature_Seasonality" = bio4,
                "Max_Temperature_of_Warmest_Month" = bio5,
                "Min_Temperature_of_Coldest_Month" = bio6,
                "Temperature_Annual_Range" = bio7,
                "Mean_Temperature_of_Wettest_Quarter" = bio8,
                "Mean_Temperature_of_Driest_Quarter" = bio9,
                "Mean_Temperature_of_Warmest_Quarter" = bio10,
                "Mean_Temperature_of_Coldest_Quarter" = bio11,
                "Annual_Precipitation" = bio12,
                "Precipitation_of_Wettest_Month" = bio13,
                "Precipitation_of_Driest_Month" = bio14,
                "Precipitation_Seasonality" = bio15,
                "Precipitation_of_Wettest_Quarter" = bio16,
                "Precipitation_of_Driest_Quarter" = bio17,
                "Precipitation_of_Warmest_Quarter" = bio18,
                "Precipitation_of_Coldest_Quarter" = bio19)



# Get total species list --------------------------------------------------
species  <- paste(MyTab$herbivore_identity, collapse = ",")
species2 <-     unlist(strsplit(species, ","))
species3 <- gsub(" ", "", species2, fixed = T)
species4 <- unique(species3)
rm(species, species2, species3)


# Climate space data ------------------------------------------------------

# Import dataset with the ranges of environmental variables within the study region
RangeofEcoContexts <- read_csv("RangeofEcoContexts.csv")
# remove row and id columns
range <- select(RangeofEcoContexts,-X1, -ID)
# remove empty rows
range <- range[rowSums(is.na(range)) != ncol(range),]
rm(RangeofEcoContexts)
# scale variables
range[,c("bio1", 
         "bio2",
         "bio5",
         "bio6",
         "bio7",
         "bio8",
         "bio9",
         "bio10",
         "bio11")] <- range[,c("bio1", 
                               "bio2",
                               "bio5",
                               "bio6",
                               "bio7",
                               "bio8",
                               "bio9",
                               "bio10",
                               "bio11")]/10
# Rename columns
range <- rename(range,
                "Annual_Mean_Temperature" = bio1,
                "Mean_Diurnal_Range" = bio2,
                "Isothermality" = bio3,
                "Temperature_Seasonality" = bio4,
                "Max_Temperature_of_Warmest_Month" = bio5,
                "Min_Temperature_of_Coldest_Month" = bio6,
                "Temperature_Annual_Range" = bio7,
                "Mean_Temperature_of_Wettest_Quarter" = bio8,
                "Mean_Temperature_of_Driest_Quarter" = bio9,
                "Mean_Temperature_of_Warmest_Quarter" = bio10,
                "Mean_Temperature_of_Coldest_Quarter" = bio11,
                "Annual_Precipitation" = bio12,
                "Precipitation_of_Wettest_Month" = bio13,
                "Precipitation_of_Driest_Month" = bio14,
                "Precipitation_Seasonality" = bio15,
                "Precipitation_of_Wettest_Quarter" = bio16,
                "Precipitation_of_Driest_Quarter" = bio17,
                "Precipitation_of_Warmest_Quarter" = bio18,
                "Precipitation_of_Coldest_Quarter" = bio19)
range$ArcticHerbivore_Species.richness <- range$ArcticHerbivore_Species.richness*70


# List environmental and ecological variables -----------------------------
EEvars <- c(
  "ArcticHerbivore_Species.richness",                         
  "ArcticHerbivore_Phylogenetic.diversity",                    
  "ArcticHerbivore_Functional.diversity",
  "Annual_Mean_Temperature",
  "Mean_Diurnal_Range",
  "Isothermality",
  "Temperature_Seasonality",
  "Max_Temperature_of_Warmest_Month",
  "Min_Temperature_of_Coldest_Month",
  "Temperature_Annual_Range",
  "Mean_Temperature_of_Wettest_Quarter",
  "Mean_Temperature_of_Driest_Quarter",
  "Mean_Temperature_of_Warmest_Quarter",
  "Mean_Temperature_of_Coldest_Quarter",
  "Annual_Precipitation",
  "Precipitation_of_Wettest_Month",
  "Precipitation_of_Driest_Month",
  "Precipitation_Seasonality",
  "Precipitation_of_Wettest_Quarter",
  "Precipitation_of_Driest_Quarter",
  "Precipitation_of_Warmest_Quarter",
  "Precipitation_of_Coldest_Quarter")
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
                  width = 300, height = "auto",
                              
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
       pickerInput(inputId = "colour", 
                   label = "Colour by:", 
                   choices = c("country", "language", "herbivore_type", 
                                "effect_type", "experimental_design", 
                                "study_method", "study_design"),
                   selected = "country"))),

      
      
      
# . Filters -----------------------------------------------------------------


      # fluidRow(
  column(width = 6,
# .. Country ---------------------------------------------------------------
         
         pickerInput(
           inputId = "country",
           choices = unique(MyTab$country),
           selected = unique(MyTab$country),
           multiple = TRUE,
           width = "100%",
           options = list(
             `actions-box` = TRUE, `selected-text-format`= "static", title = "Country")),
         
                   
# .. Species ---------------------------------------------------------------

pickerInput(
  inputId = "species",
  choices = sort(species4),
  selected = sort(species4),
  multiple = TRUE,
  width = "100%",
  options = list(
    `actions-box` = TRUE, `selected-text-format`= "static", title = "Species")),



# .. Language --------------------------------------------------------------


      pickerInput(inputId = "language", 
                  choices = unique(MyTab$language),
                  selected = unique(MyTab$language),
                  multiple = TRUE,
                  width = "100%",
                  options = list(
                    `actions-box` = TRUE, 
                    `selected-text-format`= "static",
                    title = "Language")
                  ),
               

# .. Herbivore -------------------------------------------------------------

             
       pickerInput(inputId = "herbivore", 
               choices = unique(MyTab$herbivore_type),
               selected = unique(MyTab$herbivore_type),
               multiple = TRUE,
               width = "100%",
               options = list(
               `actions-box` = TRUE, 
               `selected-text-format`= "static",
               title = "Herbivore")
               )

  ),
column(width = 6,
                


# .. study design ----------------------------------------------------------

  pickerInput(inputId = 'studydesign',
             choices = unique(MyTab$study_design),
             selected = unique(MyTab$study_design),
             multiple = TRUE,
             width = "100%",
             options = list(
             `actions-box` = TRUE, 
             `selected-text-format`= "static",
             title = "Type")
             ),



# .. study method ----------------------------------------------------------

  pickerInput(
             inputId = 'studymethod',
             choices = unique(MyTab$study_method),
             selected = unique(MyTab$study_method),
             multiple = TRUE,
             width = "100%",
             options = list(
             `actions-box` = TRUE, 
             `selected-text-format`= "static",
             title = "Method")
              ),
      
# .. ex. design ------------------------------------------------------------

  pickerInput(
             inputId = 'expdesign',
             choices = unique(MyTab$experimental_design),
             selected = unique(MyTab$experimental_design),
             multiple = TRUE,
             width = "100%",
             options = list(
               `actions-box` = TRUE, 
               `selected-text-format`= "static",
               title = "Design")
           ),

# Remaining datapoints ----------------------------------------------------

verbatimTextOutput('remaining')


 ), # column

), # abs.panel1



#verbatimTextOutput("keepers"),




# Lower Tabbox ------------------------------------------------------------

     
tabBox(width = NULL, id = 'additionals',


# . count cases -----------------------------------------------------------

          
  tabPanel('Count cases',
    h5("Output is reactive to the filtering in the above map"),
           selectInput(inputId = "uni", 
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
                             "management"),
                selected = c("country"),
                selectize = TRUE),
                plotOutput('univ')),  
     

# . trends ----------------------------------------------------------------

     
         tabPanel('Temporal trends',
                  h5("Output is reactive to the filtering in the above map"),
                 # radioButtons(inputId = "uni2", 
                 #              label = "",
                 #              choices = c("year")), 
                  plotOutput('trends')),


# . pairwise plots --------------------------------------------------------

     
  tabPanel('Pairwise Plots',
      fluidRow(h5("The blue circles are the evidence points 
                  identified in the systematic review. The darker 
                  opaque points show all possible values for each 
                  variable inside the study region (the Arctic circle)")),
      fluidRow(
           column(width = 3,
           pickerInput(
             inputId = "var1",
             label = "X variable", 
             choices = EEvars,
             selected = EEvars[4]
           )),
           column(width = 3,
           pickerInput(
             inputId = "var2",
             label = "Y variable", 
             choices = EEvars,
             selected = EEvars[15]
           )),
           column(width = 3,
           pickerInput(
             inputId = "var3",
             label = "Size variable", 
             choices = c("NULL", EEvars),
             selected = EEvars[1]
           ))),
           #column(width=3,
           #pickerInput(
           #  inputId = "var4",
           #  label = "Colour", 
           #  choices = c("country", "language", "herbivore_type", 
           #                        "effect_type", "experimental_design", 
           #                        "study_method", "study_design")
           #  ))),
      fluidRow(     
          plotOutput('space')),
      fluidRow(
          h5("For information about the climatic variables, 
             go to: https://www.worldclim.org/data/bioclim.html"))),



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




    
 
  datR <- reactive({
    for(i in 1:nrow(MyTab)){
      MyTab$incl[i] <- any(input$species %in% gsub(" ", "", unlist(strsplit(MyTab$herbivore_identity[i], ","))))
      
    }
   
      
    MyTab[
        MyTab$incl == TRUE &
        dplyr::between(MyTab$year, input$year[1], input$year[2]) &
        MyTab$country %in% input$country &
        MyTab$language %in% input$language &
        MyTab$herbivore_type %in% input$herbivore &
        MyTab$study_design %in% input$studydesign &
        MyTab$study_method %in% input$studymethod &
        MyTab$experimental_design %in% input$expdesign
      ,]
    
  })
  

# Remaining datapoints ----------------------------------------------------
  output$remaining <- renderText(nrow(datR()))

 
  
# THE MAP ####
  output$theMap <- renderLeaflet({
      
      
    # Convert to spatial...
    dat2 <- sp::SpatialPointsDataFrame(coords = datR()[,c("coordinates_E","coordinates_N")], 
                                       data = datR(),
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))# +ellps=WGS84 +towgs84=0,0,0"))
    
     m <- mapview::mapview(dat2,
                  layer.name = "Evidence Point",
                  map.types = c("Esri.WorldShadedRelief","Esri.WorldImagery"),
                  cex = 5,
                  alpha.regions = 0.5,
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
                  legend=T)
                  
     m@map
     
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
output$space <- renderPlot({
  
  ggplot(data = range, aes_string(x = input$var1, y = input$var2))+
    geom_point(alpha=5/10, size=2)+
    theme_bw()+
   geom_point(data = MyTab, aes_string(size = input$var3), 
                                       #colour = input$var4, fill=input$var4),
              shape=21, colour = "blue", fill = "blue", stroke = 1, alpha=0.3)
})

# TRENDS ####
  output$trends <- renderPlot({
    
    ggplot(data = datR(), aes(x=year))+
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

