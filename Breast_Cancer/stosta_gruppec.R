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
                                    "Größe des Tumors",
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
           plotOutput("normalplot"),
           textOutput("resultText")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Max erwartungswert anpassen, je nach Thema
  observeEvent(input$topic, {
    data = c()
    if (input$topic == "Alter") {
      data <- breast_cancer_data$age
    } else if(input$topic == "Größe des Tumors") {
      data <- breast_cancer_data$size
    } else if(input$topic == "Anzahl der Lymphknoten") {
      data <- breast_cancer_data$nodes
    }
    updateSliderInput(session, "mu", max = max(data))
  })
  
  
  output$selected_var <- renderText({ 
    x <- sample(breast_cancer_data$age[breast_cancer_data$grade == input$grade], input$sample, replace=TRUE)
    xmean <- mean(x)
    xsd <- sd(x)
    var <- x
    z <- qnorm(1 - (input$alpha/2), input$mu, xsd)
    y <- dnorm(x,input$mu, xsd)

     qnorm(1-(input$alpha/2), input$mu, xsd)
     y
  })
  
  output$normalplot <- renderPlot({
    x = c()
    if (input$topic == "Alter") {
      if(input$gradeTumor == TRUE){
        x <- sample(breast_cancer_data$age[breast_cancer_data$grade == input$grade], input$sample, replace=TRUE)
      } else {
        x <- sample(breast_cancer_data$age, input$sample, replace=TRUE)
      }
      data <- x
      xmean <- mean(x)
      xsd <- sd(x)
      var <- x
      z <- qnorm(1 - (input$alpha/2), input$mu, xsd)
      y <- dnorm(x,input$mu, xsd)
      
      ggplot(data = breast_cancer_data, aes(x=age)) +
      stat_function(fun = function(x) dnorm(x,xmean,xsd),colour = "black") +
      geom_area(data = subset(data, x <= z),
                aes(ymax = y, ymin =0),
                fill = "steelblue",
                alpha = 0.2)
      
    } else if(input$topic == "Größe des Tumors") {
      if(input$gradeTumor == TRUE){
        x <- sample(breast_cancer_data$size[breast_cancer_data$grade == input$grade], input$sample, replace=TRUE)
      } else {
        x <- sample(breast_cancer_data$size, input$sample, replace=TRUE)
      }
      
      xmean <- mean(x)
      xsd <- sd(x)
      var <- x
      z <- qnorm(1 - input$alpha/2, input$mu, xsd)
      lower_limit <- input$mu - z * xsd
      upper_limit <- input$mu + z * xsd
      y <- dnorm(x,input$mu,xsd)
      
      ggplot(data = breast_cancer_data, aes(x=size)) +
        stat_function(fun = function(x) dnorm(x,xmean,xsd),colour = "black") #+
       # geom_area(data = subset(breast_cancer_data, x <= qnorm(1-input$alpha/2, input$mu, xsd)),
       #           aes(ymax = y, ymin = 0),
       #           fill = "lightblue",
       #           alpha = input$alpha)
      
    }else if(input$topic == "Anzahl der Lymphknoten") {
      if(input$gradeTumor == TRUE){
        x <- sample(breast_cancer_data$nodes[breast_cancer_data$grade == input$grade], input$sample, replace=TRUE)
      } else {
        x <- sample(breast_cancer_data$nodes, input$sample, replace=TRUE)
      }
      xmean <- mean(x)
      xsd <- sd(x)
      var <- x
      z <- qnorm(1 - input$alpha/2, input$mu, xsd)
      lower_limit <- input$mu - z * xsd
      upper_limit <- input$mu + z * xsd
      y <- dnorm(x, input$mu, xsd)
      
      ggplot(data = breast_cancer_data, aes(x=nodes)) +
        stat_function(fun = function(x) dnorm(x,xmean,xsd),colour = "black") #+
        #geom_area(data = subset(breast_cancer_data, x <= qnorm(1-input$alpha/2, input$mu, xsd)),
        #          aes(ymax = y, ymin = 0),
        #          fill = "lightblue",
        #          alpha = input$alpha)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
