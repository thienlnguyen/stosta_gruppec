#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(shiny)
library(readr)

breast_cancer_data <- read_delim("breast_cancer_data.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE,show_col_types = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Breast Cancer"),

   

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("sample",
                        "Stichprobengröße",
                        min = 1,
                        max = nrow(breast_cancer_data),
                        value = 30),
            selectInput("var", 
                        label = "Choose a variable to display",
                        choices = c("Alter", 
                                    "Größe des Tumors",
                                    "Anzahl der Lymphknoten"),
                        selected = "Alter")
        ),
        mainPanel(
           plotOutput("normalplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$normalplot <- renderPlot({
      #sample <- seq(min(x), max(x), length.out = input$sample + 1)
      
      if (input$var == "Alter") {
        x <- breast_cancer_data$age
        xmean <- mean(x)
        xsd <- sd(x)
        var <- breast_cancer_data
        ggplot(data = breast_cancer_data, aes(x=age)) +
        stat_function(fun = function(x) dnorm(x,xmean,xsd),colour = "red")
      } else if(input$var == "Größe des Tumors") {
        x <- breast_cancer_data$size
        xmean <- mean(x)
        xsd <- sd(x)
        var <- breast_cancer_data
        ggplot(data = breast_cancer_data, aes(x=size)) +
        stat_function(fun = function(x) dnorm(x,xmean,xsd),colour = "red")
      }else if(input$var == "Anzahl der Lymphknoten") {
        x <- breast_cancer_data$nodes
        xmean <- mean(x)
        xsd <- sd(x)
        var <- breast_cancer_data
        ggplot(data = breast_cancer_data, aes(x=nodes)) +
        stat_function(fun = function(x) dnorm(x,xmean,xsd),colour = "red")
      }
      })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
