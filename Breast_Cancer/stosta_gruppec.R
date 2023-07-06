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
            selectInput("topic", 
                        label = "Welchen Wert möchtest du testen?",
                        choices = c("Alter", 
                                    "Größe des Tumors in mm",
                                    "Anzahl der Lymphknoten"),
                        selected = "Alter"),
            checkboxInput("gradeTumor", "Nach Tumorgrad selektieren"),
            conditionalPanel(
              condition = "input.gradeTumor == true",
              selectInput("grade", 
                          label = "Wähle den Tumorgrad aus.",
                          choices = sort(unique(breast_cancer_data$grade)),
                          selected = "1")
            ),
            sliderInput("sample",
                        "Stichprobengröße",
                        min = 10,
                        max = nrow(breast_cancer_data),
                        value = 100),
            sliderInput("alpha",
                        "Signifikanzniveau",
                        min = 0,
                        max = 1,
                        value = 0.05),
            sliderInput("mu",
                        "Erwartungswert",
                        min = 1,
                        max = 40,
                        value = 10)
        ),
        mainPanel(
           
          tabsetPanel(type = "tabs",
                      tabPanel("Plot", plotOutput("normalplot"), textOutput("selected_var1"), textOutput("selected_var")),
                      tabPanel("Summary"),
                      tabPanel("Table")
          ),
           
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Max erwartungswert anpassen, je nach Thema
  observeEvent(input$topic, {
    col = ""
    if (input$topic == "Alter") {
      col= "age"
    } else if(input$topic == "Größe des Tumors in mm") {
      col = "size"
    } else if(input$topic == "Anzahl der Lymphknoten") {
      col= "nodes"
    }
    updateSliderInput(session, "mu", max = max(breast_cancer_data[[col]]))
  })

  output$normalplot <- renderPlot({
    col = ""
    x = c()
    
    if (input$topic == "Alter") {
      col = "age"
    } else if(input$topic == "Größe des Tumors in mm") {
      col = "size"
    } else if(input$topic == "Anzahl der Lymphknoten") {
      col =  "nodes"
    }
    
    if(input$gradeTumor == TRUE){
      x <- sample(breast_cancer_data[[col]][breast_cancer_data$grade == input$grade], input$sample, replace=TRUE)
    } else {
      x <- sample(breast_cancer_data[[col]], input$sample, replace=TRUE)
    }
    xmean <- mean(x)
    xsd <- sd(x)
    
    a <- qnorm((input$alpha/2), xmean, xsd)
    b <- qnorm(1-(input$alpha/2), xmean, xsd)
    
    if(a<0){
      ggplot(data = breast_cancer_data, aes(x = breast_cancer_data[[col]])) +
        stat_function(fun = function(x) dnorm(x, xmean, xsd), color = "black") +
        geom_vline(xintercept = b, color = "red") +
        xlab(input$topic) +
        ylab("Density")
      
    } else {
      ggplot(data = breast_cancer_data, aes(x = breast_cancer_data[[col]])) +
        stat_function(fun = function(x) dnorm(x, xmean, xsd), color = "black") +
        geom_vline(xintercept = a, color = "red") +
        geom_vline(xintercept = b, color = "red") +
        xlab(input$topic) +
        ylab("Density")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
