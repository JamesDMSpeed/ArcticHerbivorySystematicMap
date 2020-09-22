library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rdrop2)
library(stringr)
library(DT)
library(httr)
library(readr)


MyTab <- read_delim("AllCodedData.txt", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

 


ui <- dashboardPage(title = "Systematic Map",
                    
          dashboardHeader(title = "Systematic Map",
                                    titleWidth = 400),
                    
          dashboardSidebar(disable = T),
                    
          
          dashboardBody(
                
            column(width=12,
                             
                    DTOutput('filtered')
                             
                             
                      ))
                    
)


server <- function(input, output, session) {
  output$filtered <- renderDT(
    MyTab, selection = 'single'
  )
  
 
  
}

shinyApp(ui = ui, server = server)

