# LIBRARY ####
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(readr)
library(leaflet)
library(mapview)
library(plyr)
library(dplyr)
library(ggplot2)
library(sp)
library(stringr)
library(data.table)
library(rlang)
library(sf)
library(RColorBrewer)
#

#Note: If map fails to load need to install older version of mapview
#library(devtools)
#install_version('mapview',version='2.7.8')
#install_version('rlang',version='0.4.9')

# Get Data ----------------------------------------------------------------

MyTab <- fread("AllCodedData_withGIScontext.csv")
#MyTab <- fread("shiny/AllCodedData_withGIScontext.csv")

# Add shortened versions of author lists, but first, change weird column name
if(!"author_list" %in% names(MyTab)) colnames(MyTab)[2] <- "author_list"
MyTab$author_list <- as.character(MyTab$author_list)
MyTab$language <- as.character(MyTab$language)
MyTab$author_list2 <- substr(MyTab$author_list, start = 1, stop = 20)
MyTab$author_list2 <-  ifelse(nchar(MyTab$author_list)>20, 
                              paste0(MyTab$author_list2, " [...]"),
                              MyTab$author_list2)
#names(MyTab)[names(MyTab)=='GPW']<-'HumanPopulationDensity'
#names(MyTab)[names(MyTab)=='Footprint']<-'Human.footprint'

# Fix alternate spelling
MyTab$language[MyTab$language == "English"] <- "english"
# Make a colums whihc is later used in the filter by species function
MyTab$incl <- NULL
# Scale species richness whihc is currently as proportions
MyTab$ArcticHerbivore_Species.richness <- MyTab$ArcticHerbivore_Species.richness*69
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

MyTab <- dplyr::rename(MyTab,
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
                "Precipitation_of_Coldest_Quarter" = bio19,
                "Distance_to_Coast"=distance_from_coast,
                "Distance_from_treeline"=north_of_treeline,
                "Growing_season"=CurrentGrowingSeasonLength,
                "Extent_of_recent_growing_season_change"=GrowingSeasonLength.trend,
                "Extent_of_recent_warming"=Temperature.anomaly,
                "Extent_of_recent_greening"=NDVI.trend,
                "Productivity"=Current.NDVI,
                "Human_population_density"=GPW,
                "Human_footprint"=Footprint,
                )

MyTab <- dplyr::rename(MyTab, "Elevation" = elevation_DEM)

# Make some variables continous even though they contain categories
r <- c("not available",  "not relevant",  "not reported")
MyTab$last_year_of_study <- MyTab$year_end
MyTab$last_year_of_study[MyTab$last_year_of_study %in% r] <- NA
MyTab$last_year_of_study <- as.numeric(MyTab$last_year_of_study)

MyTab$first_year_of_study <- MyTab$year_start
MyTab$first_year_of_study[MyTab$first_year_of_study %in% r] <- NA
MyTab$first_year_of_study <- as.numeric(MyTab$first_year_of_study)

# Reduce the number of levels in some variables

MyTab$extent_of_spatial_scale <- as.factor(MyTab$extent_of_spatial_scale)

MyTab$extent_of_spatial_scale <- plyr::revalue(MyTab$extent_of_spatial_scale,
         c("not reported"="not reported or nor relevant",
           "not relevant"="not reported or nor relevant"#,
         #  "from 100x100 km to 1000x1000 km (including 1000x1000 km)" = "from 100x100 km to 1000x1000 km",
          # "from 10x10 km to 100x100 km (including 100x100 km)" = "from 10x10 km to 100x100 km",
          # "from 1x1 km to 10x10 km (including 10x10 km)" = "from 1x1 km to 10x10 km"
           ))
MyTab$extent_of_spatial_scale <- factor(MyTab$extent_of_spatial_scale, 
                                        levels = c("1x1 km or less", 
                                                   "from 1x1 km to 10x10 km",
                                                   "from 100x100 km to 1000x1000 km",
                                                   "larger than 1000x1000 km",
                                                   "not reported or nor relevant"))


exttemp<-(as.numeric(as.character(MyTab$extent_of_temporal_scale)))
MyTab$extent_of_temporal_scale[exttemp==1]<-'1 year'
MyTab$extent_of_temporal_scale[exttemp>1  & exttemp<=5]<-'2-5 years'
MyTab$extent_of_temporal_scale[exttemp>5  & exttemp<=10]<-'5-10 years'
MyTab$extent_of_temporal_scale[exttemp>10 & exttemp<=20]<-'11-20 years'
MyTab$extent_of_temporal_scale[exttemp>20 & exttemp<=50]<-'21-50 years'
MyTab$extent_of_temporal_scale[exttemp>50 & exttemp<=100]<-'51-100 years'
MyTab$extent_of_temporal_scale[exttemp>100 ]<-'Over 100 years'
MyTab$extent_of_temporal_scale<-factor(MyTab$extent_of_temporal_scale,
                                          levels= c('1 year','2-5 years','5-10 years','11-20 years','21-50 years','51-100 years','Over 100 years'),
                                          ordered=T)

#Blank column for colouring points the same
MyTab$none<-rep('Evidence point',times=nrow(MyTab))

# Get total species list --------------------------------------------------
species  <- paste(MyTab$herbivore_identity, collapse = ",")
species2 <-     unlist(strsplit(species, ","))
species3 <- gsub(" ", "", species2, fixed = T)
species4 <- unique(species3)
rm(species, species2, species3)


# Climate space data ------------------------------------------------------

# Import dataset with the ranges of environmental variables within the study region
RangeofEcoContexts <- fread("RangeofEcoContexts.csv")
#RangeofEcoContexts <- fread("shiny/RangeofEcoContexts.csv")
# remove row and id columns
range <- select(RangeofEcoContexts,-V1)
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
range <- dplyr::rename(range,
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
                "Precipitation_of_Coldest_Quarter" = bio19,
                "Distance_to_Coast"=DistancetoCoast,
                "Distance_from_treeline"=NorthofTreeline,
                "Growing_season"=CurrentGrowingSeasonLength,
                "Extent_of_recent_growing_season_change"=GrowingSeasonLength.trend,
                "Extent_of_recent_warming"=Temperature.anomaly,
                "Extent_of_recent_greening"=NDVI.trend,
                "Productivity"=Current.NDVI,
                "Human_population_density"=HumanPopulationDensity,
                "Human_footprint"=Human.footprint)

range$ArcticHerbivore_Species.richness <- range$ArcticHerbivore_Species.richness*69


# List environmental and ecological variables -----------------------------

EEvars <- c(
  "Annual_Mean_Temperature",
  "Annual_Precipitation",
  "ArcticHerbivore_Functional.diversity",
  "ArcticHerbivore_Phylogenetic.diversity",
  "ArcticHerbivore_Species.richness",
  "Distance_to_Coast",
  "Distance_from_treeline",
  "Elevation",
  "Extent_of_recent_greening",
  "Extent_of_recent_growing_season_change",
  "Extent_of_recent_warming",
  "Growing_season",
  "Human_footprint",
  "Human_population_density",
  "Isothermality",
  "Max_Temperature_of_Warmest_Month",
  "Mean_Diurnal_Range",
  "Mean_Temperature_of_Coldest_Quarter",
  "Mean_Temperature_of_Driest_Quarter",
  "Mean_Temperature_of_Warmest_Quarter",
  "Mean_Temperature_of_Wettest_Quarter",
  "Min_Temperature_of_Coldest_Month",
  "Precipitation_of_Coldest_Quarter",
  "Precipitation_of_Driest_Month",
  "Precipitation_of_Driest_Quarter",
  "Precipitation_of_Warmest_Quarter",
  "Precipitation_of_Wettest_Month",
  "Precipitation_of_Wettest_Quarter",
  "Precipitation_Seasonality",
  "Productivity",
  "Temperature_Annual_Range",
  "Temperature_Seasonality"
  )

varA <- c("additional_exposures",
          "conservation_herbivore",
          "country", 
          "effect_type",
          "experimental_design",
          "exposure_quantification",
          "extent_of_spatial_scale",
          "extent_of_temporal_scale",
          "herbivory_season",
          "herbivore_type",
          "management",
          "management_herbivore",
          "measured_response_variable",
          "none",
          "permafrost",
          "redundancy",
          "soil_type",
          "study_design",
          "study_method",
          "spatial_resolution_recorded",
          "spatial_resolution_reported",
          "Subzone",
          "temporal_resolution"
)

# those without background data
#EEvars2 <- c("bbb" )

# Soil type
soil <- fread("SoilLegend.csv")
# make a new column (removing the period behind the name)
MyTab$soil_type <- soil$SoilType[match(MyTab$soil_type., soil$Letter)]

#Set factor levels for subzone
MyTab$Subzone<-as.factor(MyTab$Subzone)
#levels(MyTab$Subzone)<-c('A','B','C','D','E','Non-Arctic')#,'Non-Arctic')

#Factor levels for permafrost
#MyTab$permafrost[is.na(MyTab$permafrost)]<-'None'
MyTab$permafrost<-as.factor(MyTab$permafrost)
#levels(MyTab$permafrost)<-c('Continuous','Discontinuous','IsolatedPatches','Sporadic','None')
levels(MyTab$permafrost)<-c('IsolatedPatches','Sporadic','Discontinuous','Continuous')#,'None')
MyTab$permafrost<-factor(MyTab$permafrost,ordered=T,levels=c('IsolatedPatches','Sporadic','Discontinuous','Continuous'))

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

    shinydashboard::box(width = NULL,
            leafletOutput('theMap')),
                 
# Abs.Panel ---------------------------------------------------------------

    absolutePanel(id = "controls", 
                  class = "panel panel-default", 
                  fixed = F,
                  draggable = TRUE, 
                  top = 80, left = 70, right = 'auto', bottom = "auto",
                  width = 300, height = "auto",
                              
                   # Changing the colour of the slider from blue to orange
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: orange} .irs-from, .irs-to, .irs-single {background: orange }")),   

     
# . Year and Colour ---------------------------------------------------------


        fluidRow(
          column(width = 7,
                 br(),
                 switchInput(
                   inputId = "filteron",
                   label = "Filters", 
                   value = TRUE,
                   labelWidth = "80px")
         ),
         column(width=5,
       pickerInput(inputId = "colour", 
                   label = "Colour by:", 
                   choices =varA,
                   selected = "herbivore_type",
                   options = list(size=10)))),

      
      
      
# . Filters -----------------------------------------------------------------

sliderInput("year", 
            label = h5("Publication year"), 
            min = 1940, #min(MyTab$year), 
            max = 2020, #min(MyTab$year), 
            value = c(1945, 2019),
            step=1, sep="", ticks=F, width = "100%",
            animate=F
),

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
             `actions-box` = TRUE, `selected-text-format`= "static", title = "Country",size=6)),
         
                   
# .. Species ---------------------------------------------------------------

pickerInput(
  inputId = "species",
  choices = sort(species4),
  selected = sort(species4),
  multiple = TRUE,
  width = "100%",
  options = list(
    `actions-box` = TRUE, `selected-text-format`= "static", title = "Species",size=6)),



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


 ) # column

), # abs.panel1


# Description text and Active Filters ####
h5("This is an interactive user interface of the systematic map of herbivory in the Arctic.",tags$br(),
   "Above, the spatial distribution of evidence points is shown. These can be filtered or coloured by a range of variables.",tags$br(),
   "Below, individual variables can be summarised, or pairs of variables can be plotted against one another. The full data set can be downloaded below.",tags$br(),
   "The systematic map is published as Soininen et al. XXXX", tags$a(href="https://environmentalevidencejournal.biomedcentral.com/","Environmental Evidence"), "- click to download here [will be added]",tags$br(),
   "The systematic map protocol is published as Soininen et al. 2018 Environmental Evidence and can be downloaded", tags$a(href="https://environmentalevidencejournal.biomedcentral.com/track/pdf/10.1186/s13750-018-0135-1.pdf", "here"),tags$br(),
   "For further information contact", tags$a(href="mailto:James.Speed@ntnu.no", "James Speed"), "or",tags$a(href="mailto:Eeva.Soininen@uit.no",'Eeva Soininen'),tags$br(),
   "",tags$br(),
  "Click to see which filters are active: "),
uiOutput('activeFilters'), br(),



# Lower Tabbox ------------------------------------------------------------


tabBox(width = NULL, id = 'additionals',


# . count cases -----------------------------------------------------------

          
  tabPanel('Count cases',
      h5("This figure is responsive to the filters applied above. When filters are applied, the unfiltered data is shown as light gray bars.", tags$br(),
         "Download Appendix below for full description of variables and coding"),
           selectInput(inputId = "uni", 
                label = "Select variable",
                choices = varA,
                selected = c("country"),
                selectize = TRUE),
                plotOutput('univ')),  
     

# . trends ----------------------------------------------------------------

     
         tabPanel('Univariate continuous variables',
                  h5("This figure is responsive to the filters applied above", tags$br(),
                     "Download Appendix below for full description of variables and coding"),
                  
                  pickerInput(
                    inputId = "cont",
                    label = "Select variable",
                    choices = c("year",
                                "coordinates_N",
                                "coordinates_E",
                                "first_year_of_study",
                                "last_year_of_study",
                                EEvars)),
                  plotOutput('trends')),
          


# . pairwise plots --------------------------------------------------------

     
  tabPanel('Pairwise Plots',
      fluidRow(h5("The coloured circles are the evidence points 
                  identified in the systematic review. The grey 
                  background points show all possible values for each 
                  variable inside the study region", tags$br(),
                  "Download Appendix below for full description of variables and coding")),
      fluidRow(
           column(width = 2,
           pickerInput(
             inputId = "var1",
             label = "X variable", 
             choices = EEvars,
             selected = "Annual_Mean_Temperature",
             options = list(size=10)
           )),
           column(width = 2,
           pickerInput(
             inputId = "var2",
             label = "Y variable", 
             choices = EEvars,#[!EEvars %in% EEvars2],  # avoid weird error by removing 4 variables that don't have background points
             selected = "Annual_Precipitation",
             options = list(size=10)
           )),
           column(width = 2,
           pickerInput(
             inputId = "var3",
             label = "Size variable", 
             choices = c("NULL" = 3, EEvars),
             selected = "ArcticHerbivore_Species.richness",
             options = list(size=10)
           )),
           column(width=2,
           pickerInput(
             inputId = "var4",
             label = "Colour",
             options = list(size=10),
             choices = c("NULL", varA) #"extent_of_spatial_scale", "study_design", "experimental_design")
             )),
           column(width=3,
                  sliderInput('alpha', "Transparency of background points",
                              min=0, max = 1, step = 0.1, value = 0.2)
                  #switchInput(
                  #  inputId = "var5",
                  #  label = "Background points", 
                  #  value = TRUE,
                  #  labelWidth = "80px")
           )),
      fluidRow(     
          plotOutput('space')),
      fluidRow(
          h5("For information on the variables and coding please see Appendix 3" ,tags$br(),
          "For information about the climatic variables, 
             go to: https://www.worldclim.org/data/bioclim.html"))),



# . lookup table ----------------------------------------------------------

         tabPanel('Lookup table',
                  selectInput('feature', 'Type the evidence point ID of the record you want to show. Get the unique ID by first clicking on the point on the map.', 
                choices = MyTab$evidence_point_ID, multiple=FALSE, selectize=TRUE),
                  tableOutput('oneFeature')         
                  ),


# . responsive table ------------------------------------------------------

         
         tabPanel('Responsive table',
                  h5("This figure is responsive to the filters applied above"),
                  
                  DTOutput('responsiveTable'))
         
         ),  # End panel box


# Download ----------------------------------------------------------------



h4("Press the download button to download a speadsheet copy of the raw data:"),
downloadButton("downloadData", "DownloadData"),
h4("Press the download button to download the description of variables and coding:"),
downloadButton("downloadAppendix", "DownloadAppendix")

                
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
      
    ifelse(input$filteron == TRUE, dat <- datR(), dat <- MyTab)
    # Convert to spatial...
    dat2 <- sp::SpatialPointsDataFrame(coords = dat[,c("coordinates_E","coordinates_N")], 
                                       data = dat,
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))# +ellps=WGS84 +towgs84=0,0,0"))
    
    dat2 <- sf::st_as_sf(dat2)
    dat2 <- st_jitter(dat2, factor = 0.00001)
   
    #colpoints = brewer.pal(12,"Set3")
    pal = mapviewPalette("mapviewVectorColors")
    
   # mapviewOptions(vector.palette = brewer.pal(12,"Set3"),
  #                 na.color = grey(0.8),
   #                layers.control.pos = "topright") 
    
     m <- mapview::mapview(dat2,
                  layer.name = "Evidence Point",
                  map.types = c("Stamen.Terrain","Esri.WorldImagery"),
                  cex = 5,
                  alpha.regions = 0.8,
                  zcol = input$colour,
               #   na.color=grey(0.8,alpha=0.8),
                  popup = leafpop::popupTable(dat2, 
                                              row.numbers = F, feature.id = F,
                                              zcol = c("evidence_point_ID",
                                                       "author_list2",
                                                       "year",
                                                       "journal",
                                                       "locality",
                                                       "study_design",
                                                       "experimental_design",
                                                       "herbivore_type",
                                                       "study_method",
                                                       "effect_type")),
                 #col.regions=colpoints,
                 col.regions=pal,
                 color=pal,
                                legend=T)
                  
     m@map
     #addLegend(m@map,position='right',pal=colpoints,values=input$colour)
    })
    
  
# Responsive Table ####
  output$responsiveTable <- 
    renderDataTable({
      ifelse(input$filteron == TRUE, dat <- datR(), dat <- MyTab)
      DT::datatable(dat, 
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
   ifelse(input$filteron == TRUE, dat <- datR(), dat <- MyTab)
    ggplot(data = dat, aes_string(x=input$uni))+
      geom_bar()+
      geom_bar(data = MyTab, alpha =0.3)+
      coord_flip()+
      theme_bw()+
      theme(text = element_text(size = 20))
  })
  
  
# pairwise ####
output$space <- renderPlot({
  
  
  ifelse(input$filteron == TRUE, dat <- datR(), dat <- MyTab)
  
  
  myG <- ggplot(data = dat, 
                aes_string(x = input$var1, y = input$var2, size = input$var3))+    
                  theme_bw()
  # remove size legend if size = NULL
  if(input$var3=="3"){
    myG <- myG + guides(size = FALSE)
  }
  
  if(input$var4!="NULL"){
    myG <- myG+    geom_point(aes_string(fill=input$var4),
               shape=21,  stroke = 1, alpha=0.5, colour="black")+
           guides(fill = guide_legend(override.aes = list(size = 5)))+
           scale_color_viridis_d(option = "B", 
                                 direction = -1,
                                 aesthetics = c("fill"),
                                 begin=0.4)} else{
    myG <- myG+    geom_point(shape=21,  fill="blue", 
                      stroke = 1, alpha=0.5, colour="black")
                          }
  # add background points is var 1 and 2 are not GPW, Footprint ...                         
#  if(!c(input$var1, input$var2) %in% EEvars2 ){
    myG <- myG +geom_point(data = range, aes_string(x = input$var1, y = input$var2),
                           alpha=input$alpha, size=2)  #}
  
 
  myG
           })

# TRENDS ####
  output$trends <- renderPlot({
    ifelse(input$filteron == TRUE, dat <- datR(), dat <- MyTab)
    ggplot(data = dat, aes_string(x=input$cont))+
      geom_histogram()+
      geom_histogram(data = MyTab, alpha = 0.3)+
      theme_bw()+
      theme(text = element_text(size = 20))
  })
  

  
  
# DOWNLOAD ####
  output$downloadData <- downloadHandler(
    filename = function() {"ArcticHerbivorySystematicMap.csv"},
    content = function(file) {
      write.csv( MyTab, file,  row.names = FALSE)}
  )
  
  output$downloadAppendix<-downloadHandler(
    filename="Appendix 3.xlsx",
    content= function(file) {
      file.copy("www/Appendix 3_Coding template, including descriptions of coded variables.xlsx", file)}
      )
  
  output$downloadProtocol<-downloadHandler(
    filename='Soininen_et_al._2016_Protocol.pdf',
    content=function(file){
      file.copy('www/Soininen_2018_Protocol.pdf')
    }
  )

# Active filters --------------------------------------------------------------
  output$years <- renderPrint({
    t <- input$year
    #t <- strsplit(t, "\\s+")
    #t <- noquote(unlist(t))
    
    t
  })
  
  output$countries <- renderPrint({
    t <- input$country
    if(length(t) == length(unique(MyTab$country))) {t <- "All"} else{
    #t <- gsub(pattern="\\[",replacement = "",t)
    #t <- stringr::str_replace(t, " \\[.*\\]", "")
    t <- strsplit(t, "\\s+")
    t <- noquote(unlist(t))
    }
    t
  })
   
 
  output$sp <- renderPrint({
    t <- input$species
    if(length(t) == length(species4)) {t <- "All"} else{
      t <- strsplit(t, "\\s+")
      t <- noquote(unlist(t))
    }
    
    t
  })

  output$la <- renderPrint({
    t <- input$language
    if(length(t) == length(unique(MyTab$language))) {t <- "All"} else{
      t <- strsplit(t, "\\s+")
      t <- noquote(unlist(t))
    }
    t
  })
 
   output$la <- renderPrint({
    t <- input$language
    if(length(t) == length(unique(MyTab$language))) {t <- "All"} else{
      t <- strsplit(t, "\\s+")
      t <- noquote(unlist(t))
    }
    t
  })
   
   output$he <- renderPrint({
     t <- input$herbivore
     if(length(t) == length(unique(MyTab$herbivore_type))) {t <- "All"} else{
       t <- strsplit(t, "\\s+")
       t <- noquote(unlist(t))
     }
     t
   })
   
   output$ty <- renderPrint({
     t <- input$studydesign
     if(length(t) == length(unique(MyTab$study_design))) {t <- "All"} else{
       t <- strsplit(t, "\\s+")
       t <- noquote(unlist(t))
     }
     t
   })
   output$me <- renderPrint({
     t <- input$studymethod
     if(length(t) == length(unique(MyTab$study_method))) {t <- "All"} else{
       t <- strsplit(t, "\\s+")
       t <- noquote(unlist(t))
     }
     t
   })
   output$ex <- renderPrint({
     t <- input$expdesign
     if(length(t) == length(unique(MyTab$experimental_design))) {t <- "All"} else{
       t <- strsplit(t, "\\s+")
       t <- noquote(unlist(t))
     }
     t
   })
   
output$activeFilters <- renderUI({
    dropdownButton(
      h3("Active Filters"),
      h4("Years: "), textOutput('years'),
      h4("Countries: "), textOutput('countries'),
      h4("Species: "), textOutput('sp'),
      h4("Language: "), textOutput('la'),
      h4("Herbivore types: "), textOutput('he'),
      h4("Study design types: "), textOutput('ty'),
      h4("Study method: "), textOutput('me'),
      h4("Experimental design: "), textOutput('ex'),
      circle = TRUE, status = "info",
      icon = icon("list"), width = "300px",
      tooltip = tooltipOptions(title = "Click to see which filters are active")
    )
  })
  
}
shinyApp(ui = ui, server = server)

