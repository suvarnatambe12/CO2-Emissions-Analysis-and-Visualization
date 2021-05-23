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
library(dplyr)


data <- read_csv("natural-resource.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("CO2 Emissions details by Region, Country and Year"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("regionInput", "Region", choices = c("Asia", "Americas", "Europe", "Africa", "Oceania")),
      uiOutput("countryOutput"),
      uiOutput("yearOutput")
    ),
    mainPanel(
      span(textOutput("coolplotText"), style="color:blue;font-size:20px"),
      plotOutput("coolplot"),
      br(),
      
      span(textOutput("barText"), style="color:blue;font-size:20px"),
      plotOutput("bar"),
      br(), 
      
      span(textOutput("resultsText"), style="color:blue;font-size:20px"),
      tableOutput("results")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filteredRegion <- reactive({ data %>% filter(Region==input$regionInput) })
  
  filteredCountry <- reactive({ data %>% filter(Country_Name==input$countryInput) })
  
  filteredRegionAndYear <- reactive({ data %>% filter(Region==input$regionInput, Year==input$yearInput) })
  
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country", sort(unique(filteredRegion()$Country_Name)) )
  })  
  
  output$yearOutput <- renderUI({
    selectInput("yearInput", "Year", sort(unique(filteredRegion()$Year)) )
  })  
  
  # Plot 1: Corelation betwwen Oil consumotion and CO2 - Region Wise
  output$coolplotText<-renderText(paste( input$regionInput, " - Corelation betwwen Oil consumotion and CO2"))
  output$coolplot <- renderPlot({
    output$coolplot <- renderPlot({
      plot(filteredRegion()$Oil_Consumtion_Per_Person, 
           filteredRegion()$co2_emissions_tonnes_per_person, 
           xlab = "Oil Consumption",
           ylab = "Co2",
           col= "green")
    })
  })
  
  #Barchart 2 : CO2 Eimmition yearly trend - Country Wise
  output$barText<-renderText(paste(input$countryInput, " - CO2 Eimmition yearly trend"))
  output$bar <- renderPlot({
    barplot(filteredCountry()$co2_emissions_tonnes_per_person, 
            filteredCountry()$Year,
            xlab = "Year",
            ylab = "CO2",
            col= "lightblue",
            names.arg=2010:2014) 
  })
  
  
  # Table: Top 10 Co2 Emissions countries in selected region - Region and Year wise
  output$resultsText<-renderText(paste(input$regionInput, " in " , input$yearInput , " - Top 10 Co2 Emissions countries"))
  output$results <- renderTable({
    head(filteredRegionAndYear()[order(-filteredRegionAndYear()$co2_emissions_tonnes_per_person), c(2,7)], n=10)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
