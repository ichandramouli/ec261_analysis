library(shiny)
library(bslib)
library(tidyverse)
library(arrow)
library(airportr)

#Business logic
flt_data <- read_parquet('../data_raw/flights_merged.parquet')



flt_data <- flt_data |> 
  pivot_wider(names_from=type,values_from=Total)

#create new mutated columns that will be used in data viz. 
flt_data <- flt_data |> 
  filter(!(is.na(Seats) | is.na(Passengers))) |> 
  filter(Departures > 4) |> 
  mutate(city_pair=str_c(usg_apt,fg_apt,sep='-')) |> 
  mutate(load_factor = Passengers/Seats)



ui <- fluidPage(

    theme = bs_theme(version=5,bootswatch='lux'),
    titlePanel("Flight Data"),

     
    sidebarLayout(
        sidebarPanel(
          sliderInput('date_range',
                         'Select Start and End Year',
                         min=1990,
                         max=2025,
                      value=c(2020,2024),
                      sep=''),
          
            selectInput("timevar",
                        label="Select over Time Variable:",
                        choices=c('Departures', 'Passengers', 'Seats', 'load_factor'),
                        selected='Seats'),
          
          selectizeInput(inputId='us_market_choice',
                         label='Select US Market',
                         choices=unique(flt_data$usg_apt),
                         selected='IAD',
                         multiple=FALSE),
          
          selectizeInput(inputId='fg_market_choice',
                         label='Select Foreign Market',
                         choices=unique(flt_data$fg_apt),
                         selected='LHR',
                         multiple=FALSE)
        ),

        
        mainPanel(
          tabsetPanel(
            tabPanel(
              'Overall US Data',
              tableOutput('us_overallbusiestTable'),
              tableOutput('fg_overallbusiestTable'),
              plotOutput("timePlot")
            ),
            tabPanel(
              'US Gateway Airport Drilldown',
              textOutput('us_marketAirportText'),
              tableOutput('us_busiestTable'),
              plotOutput('us_marketPlot')
              
            ),
            tabPanel(
              'Foreign Market Airport Drilldown',
              textOutput('fg_marketAirportText'),
              tableOutput('fg_busiestTable'),
              plotOutput('fg_marketPlot')
            ),
            tabPanel(
              'City Pair Drilldown',
              textOutput('citypairText'),
              plotOutput('citypairPlot'),
              plotOutput('marketsharePlot')
            )
          )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$us_overallbusiestTable <- renderTable({
    if (input$timevar=='load_factor') {
      us_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        group_by(usg_apt) |> 
        summarise(total_count=mean(.data[[input$timevar]])) |> 
        arrange(desc(total_count)) |> 
        head(10)
    } else {
      
      us_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        group_by(usg_apt) |> 
        summarise(total_count=sum(.data[[input$timevar]])) |> 
        arrange(desc(total_count)) |> 
        head(10)
      
    }})
  
  output$fg_overallbusiestTable <- renderTable({
    if (input$timevar=='load_factor') {
      us_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        group_by(fg_apt) |> 
        summarise(total_count=mean(.data[[input$timevar]])) |> 
        arrange(desc(total_count)) |> 
        head(10)
    } else {
      
      us_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        group_by(fg_apt) |> 
        summarise(total_count=sum(.data[[input$timevar]])) |> 
        arrange(desc(total_count)) |> 
        head(10)
      
    }})
  
  output$timePlot <- renderPlot({
    
    if (input$timevar=='load_factor') {
      macroplot_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        group_by(data_dte) |> 
        summarise(total_count=mean(.data[[input$timevar]]))
    } else {
      
      macroplot_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        group_by(data_dte) |> 
        summarise(total_count=sum(.data[[input$timevar]]))
    }
    
       macroplot_df |> 
         ggplot(mapping=aes(x=data_dte, y=total_count)) +
         geom_line()
     })
  
  output$us_marketAirportText <- renderText(paste0('Selected US Airport: ', 
                                                airport_lookup(input$us_market_choice, 
                                                               input_type='IATA', 
                                                               output_type='name')))
  output$us_busiestTable <- renderTable({
    if (input$timevar=='load_factor') {
      us_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(usg_apt==input$us_market_choice) |> 
        group_by(fg_apt) |> 
        summarise(total_count=mean(.data[[input$timevar]])) |> 
        arrange(desc(total_count)) |> 
        head(10)
    } else {
      
      us_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(usg_apt==input$us_market_choice) |> 
        group_by(fg_apt) |> 
        summarise(total_count=sum(.data[[input$timevar]])) |> 
        arrange(desc(total_count)) |> 
        head(10)
      
    }
    us_busiest_df
  }
    
  )
  output$us_marketPlot <- renderPlot({
    if (input$timevar=='load_factor') {
      us_marketplot_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(usg_apt==input$us_market_choice) |> 
        group_by(data_dte) |> 
        summarise(total_count=mean(.data[[input$timevar]]))
    } else {
      
      us_marketplot_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(usg_apt==input$us_market_choice) |> 
        group_by(data_dte) |> 
        summarise(total_count=sum(.data[[input$timevar]]))
    }
    
    us_marketplot_df |> 
      ggplot(mapping=aes(x=data_dte, y=total_count)) +
      geom_line()
    
  }
  )
  
  output$fg_marketAirportText <- renderText(paste0('Selected Foreign Airport: ', 
                                                   airport_lookup(input$fg_market_choice, 
                                                                  input_type='IATA', 
                                                                  output_type='name')))
  
  
  
  output$fg_busiestTable <- renderTable({
    if (input$timevar=='load_factor') {
      fg_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(fg_apt==input$fg_market_choice) |> 
        group_by(usg_apt) |> 
        summarise(total_count=mean(.data[[input$timevar]])) |> 
        arrange(desc(total_count)) |> 
        head(10)
    } else {
      
      fg_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(fg_apt==input$fg_market_choice) |> 
        group_by(usg_apt) |> 
        summarise(total_count=sum(.data[[input$timevar]])) |> 
        arrange(desc(total_count)) |> 
        head(10)
      
    }
    fg_busiest_df
  })
  
  
  output$fg_marketPlot <- renderPlot({
    if (input$timevar=='load_factor') {
      fg_marketplot_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(fg_apt==input$fg_market_choice) |> 
        group_by(data_dte) |> 
        summarise(total_count=mean(.data[[input$timevar]]))
    } else {
      
      fg_marketplot_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(fg_apt==input$fg_market_choice) |> 
        group_by(data_dte) |> 
        summarise(total_count=sum(.data[[input$timevar]]))
    }
    
    fg_marketplot_df |> 
      ggplot(mapping=aes(x=data_dte, y=total_count)) +
      geom_line()
    
  }
  )
  
  output$citypairText <- renderText(paste0('Selected City Pair: ',
                                           airport_lookup(input$us_market_choice,
                                                          input_type = 'IATA',
                                                          output_type='name'),'-',
                                           airport_lookup(input$fg_market_choice,
                                                          input_type='IATA',
                                                          output_type='name')))
  output$citypairPlot <- renderPlot({
    if (input$timevar=='load_factor') {
      cityPair_df <- flt_data |>
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |>
        filter(usg_apt == input$us_market_choice & fg_apt==input$fg_market_choice) |>
        group_by(data_dte,carrier) |>
        summarise(total_count=mean(.data[[input$timevar]]))
    } else {

      cityPair_df <- flt_data |>
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |>
        filter(usg_apt == input$us_market_choice & fg_apt==input$fg_market_choice) |>
        group_by(data_dte,carrier) |>
        summarise(total_count=sum(.data[[input$timevar]]))
    }

    cityPair_df |>
      ggplot(mapping=aes(x=data_dte, y=total_count, color = carrier)) +
      geom_line()

  }
  )
  
  output$marketsharePlot <- renderPlot({
    cityPair_df <- flt_data |>
    filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |>
    filter(usg_apt == input$us_market_choice & fg_apt==input$fg_market_choice) |>
    group_by(data_dte,carrier) |>
    summarise(total_count=sum(.data[[input$timevar]]))
    
    cityPair_df |> 
      ggplot(mapping=aes(x="", y=total_count, fill=carrier)) +
      geom_bar(stat='identity', width=1) +
      coord_polar('y', start=0)
  })
  
  

  

      
    }


# Run the application 
shinyApp(ui = ui, server = server)
