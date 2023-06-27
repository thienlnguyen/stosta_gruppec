#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)

breast_cancer_data <- read_delim("breast_cancer_data.csv", 
                                 delim = ";",
                                 skip = 1,
                                 col_names = FALSE,
                                 escape_double = FALSE,
                                 trim_ws = TRUE)

breast_cancer_data <- breast_cancer_data[-1]

header <- unlist(breast_cancer_data[1, ])

# Konvertieren des Vektors in eine Liste
header_list <- as.list(header)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Breast Cancer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            selectInput("var", 
                        label = "Choose a variable to display",
                        choices = header_list,
                        selected = header_list[3])
        ),
        sliderInput("num_values", "Anzahl der Werte:", min = 1, max = 100, value = 1),
        actionButton("calculate", "Berechnen"),
        selectInput("var2", 
                    label = "Choose a variable to display",
                    choices = header_list,
                    selected = header_list[3])
    ),
    
    mainPanel(
        plotOutput("distPlot"),
       textOutput("mean_output")
        ),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        x <- breast_cancer_data$age
        
        # generate bins based on input$bins from ui.R
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
       
        observeEvent(input$calculate, {
          sample <- sample_n(breast_cancer_data[[input$var2]], input$num_values)  # Eine Stichprobe von Daten auswählen
          values <- sample  # Annahme: Die Werte befinden sich in einer Spalte namens "value_column"
          mean_value <- mean(values)  # Mittelwert berechnen
          output$mean_output <- renderText(paste("Der Mittelwert ist:", mean_value))
        })
    })
}

#Run the application 
shinyApp(ui = ui, server = server)