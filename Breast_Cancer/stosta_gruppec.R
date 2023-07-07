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
library(dplyr)
library(visNetwork)
library(DT)

breast_cancer_data <- read_delim("breast_cancer_data.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE,show_col_types = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Breast Cancer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Hypothesentest"),
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
      selectInput("testtype", 
                  label = "Wähle die Testart aus!",
                  choices = c("zweiseitiger Test", 
                              "linksseitiger Test",
                              "rechtsseitiger Test"),
                  selected = "zweiseitiger Test"),
      sliderInput("alpha",
                  "Signifikanzniveau",
                  min = 0,
                  max = 1,
                  value = 0.05),
      sliderInput("mu",
                  "Erwartungswert",
                  min = 1,
                  max = 40,
                  value = 10),
    ),
    mainPanel(
      plotOutput("normalplot"),
      htmlOutput("key_figures"),
      htmlOutput("hypotest_output")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$key_figures <- renderUI({
    # Erstellen Sie den HTML-Inhalt
    html <- "<b>Hypothesen:</b></br>"
    html <- paste(html, "H0: µ = ", input$mu,"</br>H1: µ ≠ ", input$mu, "</br>")
    html <- paste(html, "<b>Signifikanzniveau &alpha;: ",input$alpha*100,"%</b>")
    
    # Rückgabe des HTML-Inhalts
    return(HTML(html))
  })
  
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
  
  # Außerhalb der observe-Funktion:
  sample_bc <- reactive({
    breast_cancer_data %>% sample_n(size = input$sample, replace = FALSE)
  })
  
  
  observeEvent(c(input$sample, input$topic, input$grade, input$gradeTumor, input$testtype, input$alpha),{
    # Die Funktion wird ausgeführt, wenn sich der sample-Slider oder das Dropdown Tonic ändert
    sample_bc_value <- sample_bc()
    col <- ""
    
    if (input$topic == "Alter") {
      col <- "age"
    } else if (input$topic == "Größe des Tumors in mm") {
      col <- "size"
    } else if (input$topic == "Anzahl der Lymphknoten") {
      col <- "nodes"
    }
    
    if (input$gradeTumor == TRUE) {
      x <- sample_bc_value[[col]][sample_bc_value$grade == input$grade]
    } else if (input$gradeTumor == FALSE) {
      x <- sample_bc_value[[col]]
    }
    
    xmean <- mean(x)
    xsd <- sd(x)
    
    a <- -1
    b <- -1
    
    if(input$testtype=="zweiseitiger Test"){
    a <- qnorm((input$alpha/2), xmean, xsd)
    b <- qnorm(1 - (input$alpha/2), xmean, xsd)  
    } else if(input$testtype=="linksseitiger Test"){
      a <- qnorm((input$alpha), xmean, xsd)
    } else if(input$testtype=="rechtsseitiger Test"){
      b <- qnorm(1 - (input$alpha), xmean, xsd) 
    }
    
    hypotest <- reactive({
      result <- c()
      if (input$testtype == "zweiseitiger Test") {
        result <- t.test(x, mu=input$mu, conf.level = 1-input$alpha)
      } else if (input$testtype == "linksseitiger Test") {
        result <- t.test(x, mu=input$mu, alternative = "less", conf.level = 1-input$alpha)
      } else if (input$testtype == "rechtsseitiger Test") {
        result <- t.test(x, mu=input$mu, alternative = "greater", conf.level = 1-input$alpha)
      }
      return(result)
    })
    
    output$hypotest_output <- renderUI({
      result <- hypotest()
      a <- -Inf
      b <- Inf
      
      if(input$testtype=="zweiseitiger Test"){
        a <- qnorm((input$alpha/2), xmean, xsd)
        b <- qnorm(1 - (input$alpha/2), xmean, xsd)  
      } else if(input$testtype=="linksseitiger Test"){
        a <- qnorm((input$alpha), xmean, xsd)
      } else if(input$testtype=="rechtsseitiger Test"){
        b <- qnorm(1 - (input$alpha), xmean, xsd) 
      }
      
      html <- paste("p-Wert: ", result$p.value,"\n</br>")
      html <- paste(html, (1-input$alpha)*100,"% Konfidenzintervall: [", round(a, digits = 2), ", ", round(b, digits = 2), "]\n")
      if(result$p.value<input$alpha){
        html <- paste(html, "</br>Die Nullhypothese wird abgelehnt.")
      } else {
        html <- paste(html, "</br>Die Nullhypothese wird nicht abgelehnt.")
      }
      return(HTML(html))
    })
    
    output$normalplot <- renderPlot({
      if (a < (min(x)) ) {
        ggplot(data = sample_bc_value, aes(x = sample_bc_value[[col]])) +
          stat_function(fun = function(x) dnorm(x, xmean, xsd), color = "black") +
          geom_vline(xintercept = b, color = "red") +
          xlab(input$topic) +
          ylab("Relative Häufigkeit")+
          xlim(min(breast_cancer_data[[col]]), max(breast_cancer_data[[col]]))
        
        
      } else if (b<0) {
        ggplot(data = sample_bc_value, aes(x = sample_bc_value[[col]])) +
          stat_function(fun = function(x) dnorm(x, xmean, xsd), color = "black") +
          geom_vline(xintercept = a, color = "red") +
          xlab(input$topic) +
          ylab("Relative Häufigkeit")+
          xlim(min(breast_cancer_data[[col]]), max(breast_cancer_data[[col]]))
        
      }else {
        ggplot(data = sample_bc_value, aes(x = sample_bc_value[[col]])) +
          stat_function(fun = function(x) dnorm(x, xmean, xsd), color = "black") +
          geom_vline(xintercept = a, color = "red") +
          geom_vline(xintercept = b, color = "red") +
          xlab(input$topic) +
          ylab("Relative Häufigkeit")+
          xlim(min(breast_cancer_data[[col]]), max(breast_cancer_data[[col]]))
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
