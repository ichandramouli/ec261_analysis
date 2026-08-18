library(shiny)
library(tidyverse)
library(arrow)

#Business logic
flt_data <- read_parquet('../data_raw/flights_merged.parquet')



flt_data <- flt_data |> 
  pivot_wider(names_from=type,values_from=Total)

#create new mutated columns that will be used in data viz. 
flt_data <- flt_data |> 
  filter(!(is.na(Seats) | is.na(Passengers))) |> 
  mutate(city_pair=str_c(usg_apt,fg_apt,sep='')) |> 
  mutate(load_factor = Passengers/Seats)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Flight Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("timevar",
                        "Select over Time Variable:",
                        choices=c('Departures', 'Passengers', 'Seats', 'load_factor'),
                        selected='Seats')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("timePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$timePlot <- renderPlot({
        flt_data |> 
        group_by(Year) |> 
        summarise(total_count=sum(.data[[input$timevar]])) |> 
        ggplot(mapping=aes(x= Year, y= total_count))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
