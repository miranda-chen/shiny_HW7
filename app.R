####GEOL590 Class###
### In-class Oct 23 - Intro to Shiny Part II ###

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

#set slider range to the range of carats in the dataset
min.mpg <- min(mtcars$mpg)
max.mpg <- max(mtcars$mpg)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("mtcars dataset viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("mpg.adjuster",
                  "Miles per gallon",
                  min = min.mpg,
                  max = max.mpg,
                  value = c(min.mpg, max.mpg)),
      submitButton(text = "Go!") #can make it a range, two things to adjust, not just single value
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mtcars_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #d_filt is a function
  
  d_filt <- reactive({
    # Filter the mtcars plot so that it only contains the specific range of miles per gallon
    low.mpg <- input$mpg.adjuster[1]
    high.mpg <- input$mpg.adjuster[2]
    
    mtcars %>%
      filter(mpg >= low.mpg) %>%
      filter(mpg <= high.mpg)
    
  })
  
  output$mtcars_plot <- renderPlot({
    ggplot(d_filt(), mapping = aes_string(x = "cyl", y = "hp", color = "vs")) + 
      geom_point()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

