#before taking COSC 5500
library(shiny)

# Define vector datasets
Grain_Types <- c("all", "corn", "sorghum", "barley", "oats")
Year <- c(2021, 2022)
all_Feed_and_residual_use <- c(5798.06, 5866.95)
corn_Feed_and_residual_use <- c(5607, 5726)
sorghum_Feed_and_residual_use <- c(97.06, 79.95)
barley_Feed_and_residual_use <- c(26, 17)
oats_Feed_and_residual_use <- c(68, 44)

# UI
ui <- fluidPage(
  titlePanel("Grain Feed and Residual Use"),
  sidebarLayout(
    sidebarPanel(
      selectInput("grain", "Grain Type", choices = Grain_Types),
      selectInput("year", "Year", choices = Year),
      hr(),
      plotOutput("Feed_and_residual_usehistogram"),
      plotOutput("Feed_and_residual_usescatterplot")
    ),
    mainPanel(
      dataTableOutput("table_data")
    )
  )
)

# server
server <- function(input, output) {
  
# Function to filter data based on selected grain and year
  filtered_data <- reactive({
    switch(input$grain,
           "all" = ifelse(input$year == 2021, all_Feed_and_residual_use[1], all_Feed_and_residual_use[2]),
           "corn" = ifelse(input$year == 2021, corn_Feed_and_residual_use[1], corn_Feed_and_residual_use[2]),
           "sorghum" = ifelse(input$year == 2021, sorghum_Feed_and_residual_use[1], sorghum_Feed_and_residual_use[2]),
           "barley" = ifelse(input$year == 2021, barley_Feed_and_residual_use[1], barley_Feed_and_residual_use[2]),
           "oats" = ifelse(input$year == 2021, oats_Feed_and_residual_use[1], oats_Feed_and_residual_use[2]))
  })
  
# Render histogram
  output$Feed_and_residual_usehistogram <- renderPlot({
    hist(filtered_data(), main = "Histogram", xlab = "Feed and Residual Use", col = "black", border = "white")
  })
  
# Render scatterplot
  output$Feed_and_residual_usescatterplot <- renderPlot({
    if(length(filtered_data()) == 1) {
      points_2021 <- ifelse(input$grain == "all", all_Feed_and_residual_use[1], filtered_data()[1])
      points_2022 <- ifelse(input$grain == "all", all_Feed_and_residual_use[2], filtered_data()[2])
      plot(Year[1], points_2021, type = "p", main = "Scatterplot", xlab = "Year", ylab = "Feed and Residual Use", col = "black")
      points(Year[2], points_2022, type = "p", col = "black")
    } else {
      plot(Year, filtered_data(), type = "p", main = "Scatterplot", xlab = "Year", ylab = "Feed and Residual Use", col = "black")
    }
  })
  
# Render table
  output$table_data <- renderDataTable({
    data <- data.frame(Grain_Type = input$grain,
                       Year = input$year,
                       Feed_and_Residual_Use = filtered_data())
    data
  }, options = list(paging = FALSE, searching = FALSE))
}

shinyApp(ui = ui, server = server)
