#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

### calls r script files
source("functions.R")

### loads data
load("data/sample.Rdata")

### imports libraries
library(shiny)

r <- getOption("repos")
r["CRAN"] <-"https://cloud.r-project.org/"
options(repos=r)

# install.packages("pacman")

# pacman::p_load(tidyverse,readxl,haven,sjlabelled,kable,kableExtra)

#### shiny app starts here ###

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Illustrative example"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "left",
                  
        sidebarPanel(
          
          # actor input
          selectInput("actor",
                      label="Choose an actor:",
                      c("Keanu Reeves",
                        "Alec Baldwin",
                        "Arnold Schwarzenegger",
                        "Timothee Chalamet",
                        "Anamaria Marinca"),
                      multiple = TRUE),
          
          # stereotype input  
          selectInput("stereotype",
                        label="Stereotype dimension:",
                        c("Warmth women" = "wom_warm",
                                  "Competence women" = "wom_comp",
                                  "Warmth men" = "men_warm",
                                  "Competence men" = "men_comp"))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          h3("Output displayed here",),
          # start tabset Panel
          tabsetPanel(
            
            # tab 1
            tabPanel("Actors",
                     
                     tags$p(HTML(paste("A table is generated based on the actors chosen on the side panel..", sep = "")) ),        
                     
                     DT::dataTableOutput("act") ),
            
            # tab 2
            tabPanel("Stereotypes",
                     
                     tags$p(HTML(paste("A plote is generated based on the variable chosen on the side panel..", sep = "")) ),
                     
                     plotOutput("st") )
            
          ) # close tabset Panel
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ####### -- imports and prepares data from here 
  
  # reactive object
  # data from Stanciu et al. 2017
  
  tempdf <- reactive({
    
    choice=input$stereotype
    
    dfex %>% 
      sjlabelled::remove_all_labels() %>% 
      pivot_longer(contains("warm") | contains("comp")) %>% 
      filter(name %in% choice)
  
  })
  
  # reactive object
  # meta data movies
  movietmp<- reactive({
    dfmv<-readxl::read_excel("mat/movies.xlsx",1) %>% 
      filter(Actor %in% input$actor)
    
  })
  
    #### -- generates output objects from here
  
  # generate ggplot
  plottmp<- reactive({
    
    ## ggplot code
    (input$plot_type == "ggplot2")
    
    ggplot(tempdf(), aes(x=factor(gen),y=value)) +
      labs(title=paste0("Evaluation based on ", input$stereotype), 
           x="Gender",
           y=paste0("Stereotype of ", input$stereotype)) +
      geom_boxplot() + 
      theme_light()
  })
  
  ##### -- code for output from here
  
  # render plot for user 
  output$st <- renderPlot({
   
    plottmp()
  })
  
  output$act <- DT::renderDataTable({
    
    movietmp()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
