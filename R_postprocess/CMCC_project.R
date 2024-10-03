library(shiny)
library(readr)
library(ggplot2)


base_no_emis = read.csv("Base_no_constraints_on_emissions_1000.csv")
paris_original = read.csv("paris_original.csv")

times <- seq(2020, 2100, by = 5)

data_base <- base_no_emis
data_paris <- paris_original


ui <- fluidPage(
  titlePanel("CMCC project"),
  
  sidebarLayout(
    sidebarPanel(

      selectInput("user_choice", "Select the variable:", 
                  choices = c("Climate damages, frac of output", 
                              "Total CO2 Emissions, GtCO2/yr", 
                              "Cumulative CO2 emissions"), 
                  selected = "Climate damages, frac of output")  
    ),
    
    mainPanel(
      plotOutput("plot")  
    )
  )
)


server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    data1 <- NULL
    data2 <- NULL
    
    if (input$user_choice == "Climate damages, frac of output") {
      b <- data_base[, 27]
      p <- data_paris[, 27]
    } else if (input$user_choice == "Total CO2 Emissions, GtCO2/yr") {
      b <- data_base[, 34]
      p <- data_paris[, 34]
    } else if (input$user_choice == "Cumulative CO2 emissions") {
      b <- data_base[, 39]
      p <- data_paris[, 39]
    }
    
    data1 <- data.frame(times, value = b, Scenario = "Base")
    data2 <- data.frame(times, value = p, Scenario = "Paris")
    
    data_combined <- rbind(data1, data2)
    
    ggplot(data_combined, aes(x = times, y = value, color = Scenario)) +
      geom_line(size = 1) +
      labs(title = "Andamento delle variabili nel tempo",
           x = "Anno",
           y = input$user_choice) +
      theme_minimal() +
      scale_color_manual(values = c("Base" = "blue", "Paris" = "red"))
  })
  
}

# Lanciare l'applicazione Shiny
shinyApp(ui = ui, server = server)
