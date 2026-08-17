#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

#Business logic

EU261_data <- read_csv('../data/EUSeats2003_2005.csv')
canada_data <- read_csv('../data/CanadaSeats_2017_2024.csv')

EU261_data <- EU261_data |> 
  mutate(city_pair=str_c(usg_apt,fg_apt,sep=''))

canada_data <- canada_data |> 
  mutate(city_pair=str_c(usg_apt,fg_apt,sep=''))

#EU261 took effect on 2/17/2005
#also want to add a EU261 in effect column. Probably create a binary variable to do that.
#okay i think what we need to do here is:
EU261_data <- EU261_data |> 
  mutate(EU261_active=ifelse(((Year>2005)|(Year==2005 & Month>2)),'Yes','No'))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
