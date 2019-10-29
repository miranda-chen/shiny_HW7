####GEOL590 Class###
### Homework 8 Shiny Application Practice ###

library(shiny)
library(tidyverse)

#set slider range to the range of miles per gallon in the dataset
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
                  value = c(min.mpg, max.mpg)), #can make it a range, two things to adjust, not just single value
                  submitButton(text = "Go!") #button to GO and generate a plot
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mtcars_plot")
    )
  )
)

# Define server logic required to draw a plot reactive to the slider bar
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

