rm(list=ls())
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(readxl)

# Load data
d1 <- read_excel("/Users/wangjiaqi/Desktop/Milk Buddy/FSP_V7/milk.xlsx", sheet ="Data") #before kNN
d2 <- readRDS("/Users/wangjiaqi/Desktop/Milk Buddy/FSP_V7/milkbud.rds") #After kNN
d3 <- readRDS("/Users/wangjiaqi/Desktop/Milk Buddy/FSP_V7/monthd.rds") #dataset for waterfall chart
#reading Geojson file of Wisconsin county 
wisco_counties <- geojsonio::geojson_read("/Users/wangjiaqi/Desktop/Milk Buddy/FSP_V7/wisconsin-with-county-boundaries.geojson", what = "sp")
# Define UI for dashboard
ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "Milk Buddy"),
  
  # Sidebar menu
  dashboardSidebar(
    sidebarMenu(
      # About page
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      # Map page
      menuItem("Map", tabName = "map", icon = icon("globe")),
      # Data page
      menuItem("Data", tabName = "data", icon = icon("table"))
    )
),

  # Application body
  dashboardBody(
    # Tab panels
    tabItems(
      ## About page
      tabItem(tabName = "about",
              # Box to display about information, display Feeding America Eastern Wisconsin logo under the paragraph
              box(width = 12, status = "primary", solidHeader = TRUE, title = "About Milk Buddy", 
                  htmlOutput("about_text")), #Render HTML
              fluidRow(
                  column(12, style = "text-align: center;", 
                        img(tags$a(href='https://feedingamericawi.org/',   
                                   tags$img(src='FAEW1.png'))),
                  )
              )
      ),
      ## Map page
      tabItem(tabName = "map",
              # Box to display map
              box(width = 8, status = "primary", solidHeader = TRUE, title = "Milk Shortage Map", 
                  leafletOutput("map")),
              # Box to display input widgets
              box(width = 4, status = "primary", solidHeader = TRUE, title = "Input", 
                  selectInput("county", "County", choices = unique(d2$County), multiple = TRUE, selected = "Adams"),
                  selectInput("month", "Month", choices = unique(d2$Month), multiple = TRUE, selected = "Jan"),
                  selectInput("qpr.group", "QPR.Group", choices = c("1 Donated", "2 Purchased", "3 Federal-USDA"), selected = "All"),
                  plotlyOutput("waterfall")
              )
      ),
      # Data page
      tabItem(tabName = "data",
              # Box to display data table
              box(width = 12, status = "primary", solidHeader = TRUE, title = "Data Table", 
                  dataTableOutput("data_table")),
              # Box to display download buttons
              box(width = 12, status = "primary", solidHeader = TRUE, title = "Download Data", 
                  downloadButton("csv_download", "Download as CSV"),
                  downloadButton("excel_download", "Download as Excel")
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # About text output
  output$about_text <- renderText({
    paste("<ul>",
          "<li>The Milk Buddy app predicts monthly milk shortage by county 
      and provides an interactive map and data table for visualization and download.</li>",
          "<li>By examining the relationship between Wisconsin's milk production, 
      maximum Wisconsin temperatures, and the distribution operations of Feeding America Eastern Wisconsin (FAEW), 
      this application aims to uncover insights for FAEW food procurement and operations teams.</li>",
          "<li>A kNN classification model was used to predict monthly shortfall of milk. 
      The key findings suggest that the kNN model achieved an overall accuracy of approximately 86% 
      on the evaluation set, meaning that the model accurately classified 86% of the instances in the dataset.</li>",
          "</ul>"
          )
  })
  
  
  
  # Create map
  output$map <- renderLeaflet({
    selectedcounties1 <- c(input$county) # Select user input county
    selectedcounties <- d2$County[d2$County %in% selectedcounties1] # Look at list of counties in d2
    unique_county <- unique(selectedcounties) # Ensure only one county
    filtered_county <- wisco_counties[wisco_counties$name %in% unique_county,] # Filter the user input data
    
    # Filter data for counties with MonthlyShortfall equal to 0 in the selected Month
    greendata <- d2[d2$Month == input$month & d2$MonthlyShortfall == 0 & d2$QPR.Group == input$qpr.group, ]
    # Create a vector to assign colors based on the presence of any MonthlyShortfall
    color <- ifelse(filtered_county$name %in% greendata$County, "green", "red")
    
    # Create map object
    leaflet() %>%
      # Add map tiles
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      # Set view to Wisconsin
      setView(lng = -89.6414, lat = 43.7534, zoom = 6) %>%
      # Add Polygons
      addPolygons(data = filtered_county, 
                  fillColor = color, # Use color vector
                  fillOpacity = 0.7,
                  weight = 1,
                  color = "white",
                  popup = ~as.character(name), )
  })
  
  #Create Waterfall Chart(Bar Chart)
  output$waterfall <- renderPlotly({
    # Filter data based on user input
    filtered_data_plot <- d3 %>%
      filter(County %in% input$county, Month %in% input$month)
    
    # Check if filtered_data_plot is empty
    if (nrow(filtered_data_plot) == 0) {
      return(NULL)
    }
    
    # Create a vector to assign colors based on the presence of any MonthlyShortfall
    color <- ifelse(filtered_data_plot$MonthlyShortfall < 0, "No Shortfall", "Shortfall")
    
    # Create plotly chart
    chart <- plot_ly(data = filtered_data_plot, x = ~Month, y = ~abs(MonthlyShortfall), type = "bar", 
                     color = color) %>%
      layout(
        title = "Milk Shortfalls",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Overall Monthly Shortfall (Gallons)", range = c(min(abs(filtered_data_plot$MonthlyShortfall)), max(abs(filtered_data_plot$MonthlyShortfall)))),
        barmode = "group"
      )
    
    return(chart)
  })
  
    # Create data table output
  output$data_table <- renderDataTable({
    # Filter data based on user input
      filtered_data1 <- d1
    
    # Display data
    filtered_data1
    #}
  })
  
  # Create CSV download function
  output$csv_download <- downloadHandler(
    filename = "milk_buddy_data.csv",
    content = function(file) {
      write_csv(d1, file)
    }
  )
  
  # Create Excel download function
  output$excel_download <- downloadHandler(
    filename = "milk_buddy_data.xlsx",
    content = function(file) {
      write_xlsx(d1, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)