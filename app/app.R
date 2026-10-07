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

pretty_variable_names <- c(
  Seats = 'Number of Seats',
  Passengers = 'Passengers',
  load_factor = 'Load Factor',
  Departures = 'Departures',
  carrier = 'Airline'
)

options(shiny.useragg=TRUE)

ui <- fluidPage(

    theme = bs_theme(version=5,bootswatch='lux'),
 titlePanel("International Passenger Air Traffic Data"),

     
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
                        choices=c('Departures', 'Passengers', 'Seats'),
                        selected='Seats'),
          
          selectizeInput(inputId='us_market_choice',
                         label='Select US Airport Code',
                         choices=unique(flt_data$usg_apt),
                         selected='IAD',
                         multiple=FALSE),
          
          selectizeInput(inputId='fg_market_choice',
                         label='Select Foreign Airport Code',
                         choices=unique(flt_data$fg_apt),
                         selected='LHR',
                         multiple=FALSE)
        ),

        
        mainPanel(
          tabsetPanel(
            tabPanel(
              title='Overall US Data',
              fluidRow(textOutput('timePlotHeader')),
              fluidRow(plotOutput("timePlot")),
              fluidRow(
                column(width=6,
                       textOutput('us_overallbusiestTableHeader')),
                column(width=6,
                        textOutput('fg_overallbusiestTableHeader'))
              ),
              fluidRow(
                column(width=6,
              tableOutput('us_overallbusiestTable')),
              column(width=6,
              tableOutput('fg_overallbusiestTable'))
              )
              ),
            tabPanel(
              title='US Gateway Airport Drilldown',
              fluidRow(column(12,uiOutput('us_marketAirportText'))),
              fluidRow(column(12,textOutput('us_marketPlotHeader'))),
              fluidRow(column(12,plotOutput('us_marketPlot'))),
              fluidRow(column(12,textOutput('us_busiestTableHeader'))),
              fluidRow(column(12,tableOutput('us_busiestTable')))
              
              
            ),
            tabPanel(
              'Foreign Market Airport Drilldown',
              fluidRow(
                column(12,textOutput('fg_marketAirportText'))
              ),
              fluidRow(column(12,textOutput('fg_marketPlotHeader'))),
              fluidRow(column(12, plotOutput('fg_marketPlot'))),
              fluidRow(column(12,textOutput('fg_busiestTableHeader'))),
              fluidRow(
                column(12,tableOutput('fg_busiestTable'))
              ),
            ),
            tabPanel(
              'City Pair Drilldown',
              fluidRow(column(12,textOutput('citypairText'))
                       ),
              fluidRow(column(12,textOutput('citypairPlotHeader'))),
              fluidRow(column(12,plotOutput('citypairPlot'))),
              fluidRow(column(12,textOutput('citytrendlinePlotHeader'))),
              fluidRow(column(12,plotOutput('citytrendlinePlot')))
            )
          )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$us_overallbusiestTableHeader <- renderText({
    paste0('Largest US International Gateways by ',input$timevar)
  })
  
  output$us_overallbusiestTable <- renderTable({
      
      us_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        group_by(usg_apt) |> 
        summarise(total_count=as.integer(sum(.data[[input$timevar]]))) |> 
        arrange(desc(total_count)) |> 
        head(10)
      
    
    
    colnames(us_busiest_df) <- c('US Airport',pretty_variable_names[input$timevar])
    us_busiest_df 
    
    })
  
  output$fg_overallbusiestTableHeader <- renderText({
    paste0('Largest Foreign Gateway from US by ',input$timevar)
  })
  
  output$fg_overallbusiestTable <- renderTable({
    
      us_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        group_by(fg_apt) |> 
        summarise(total_count=as.integer(sum(.data[[input$timevar]]))) |> 
        arrange(desc(total_count)) |> 
        head(10)
      
    
    colnames(us_busiest_df) <- c('Foreign Airport',pretty_variable_names[input$timevar])
    us_busiest_df 
    })
  
  output$timePlotHeader <- renderText({paste0(input$timevar,
                                      ' To & From the United States ',
                                      input$date_range[[1]],'-',input$date_range[[2]])})
  
  output$timePlot <- renderPlot({
    
      
      macroplot_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        group_by(data_dte) |> 
        summarise(total_count=sum(.data[[input$timevar]]))
    
    
       macroplot_df |> 
         ggplot(mapping=aes(x=data_dte, y=total_count)) +
         geom_line() +
         labs(x='Date',
              y=pretty_variable_names[input$timevar]) +
         theme_minimal()
     })
  
  output$us_marketAirportText <- renderUI(strong(paste0('Selected US Airport: ', 
                                                airport_lookup(input$us_market_choice, 
                                                               input_type='IATA', 
                                                               output_type='name'))))
  
  output$us_busiestTableHeader <- renderText(paste0('Top 10 International Destinations from ', 
                                                    input$us_market_choice,' by ', 
                                                    input$timevar))
  
  output$us_busiestTable <- renderTable({
     
      
      us_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(usg_apt==input$us_market_choice) |> 
        group_by(fg_apt) |> 
        summarise(total_count=as.integer(sum(.data[[input$timevar]]))) |> 
        arrange(desc(total_count)) |> 
        head(10)
      
    
    colnames(us_busiest_df) <- c('Foreign Airport',pretty_variable_names[input$timevar])
    us_busiest_df
  }
    
  )
  
  output$us_marketPlotHeader <- renderText({
    paste0('International ',input$timevar, ' to & from ', input$us_market_choice, ' ',
           input$date_range[[1]],'-',input$date_range[[2]])
  })
  
  output$us_marketPlot <- renderPlot({
    
      
      us_marketplot_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(usg_apt==input$us_market_choice) |> 
        group_by(data_dte) |> 
        summarise(total_count=sum(.data[[input$timevar]]))
    
    
    us_marketplot_df |> 
      ggplot(mapping=aes(x=data_dte, y=total_count)) +
      geom_line() +
      labs(x='Date',
           y=pretty_variable_names[input$timevar]) +
      theme_minimal()
    
  }
  )
  
  output$fg_marketAirportText <- renderText(paste0('Selected Foreign Airport: ', 
                                                   airport_lookup(input$fg_market_choice, 
                                                                  input_type='IATA', 
                                                                  output_type='name')))
  
  
  output$fg_busiestTableHeader <- renderText({
    paste0('Top 10 US Markets to/from ',input$fg_market_choice,' by ',input$timevar)
  }
  )
  
  output$fg_busiestTable <- renderTable({
    
      
      fg_busiest_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(fg_apt==input$fg_market_choice) |> 
        group_by(usg_apt) |> 
        summarise(total_count=as.integer(sum(.data[[input$timevar]]))) |> 
        arrange(desc(total_count)) |> 
        head(10)
      
    
    colnames(fg_busiest_df) <- c('US Airport',pretty_variable_names[input$timevar])
    fg_busiest_df
  })
  
  output$fg_marketPlotHeader <- renderText({
    paste0(input$timevar,' between US & ',input$fg_market_choice, ' ',
           input$date_range[[1]],'-',input$date_range[[2]])
  })
  
  output$fg_marketPlot <- renderPlot({
   
      
      fg_marketplot_df <- flt_data |> 
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |> 
        filter(fg_apt==input$fg_market_choice) |> 
        group_by(data_dte) |> 
        summarise(total_count=sum(.data[[input$timevar]]))
    
    
    fg_marketplot_df |> 
      ggplot(mapping=aes(x=data_dte, y=total_count)) +
      geom_line() +
      labs(x='Date', y=pretty_variable_names[input$timevar]) +
      scale_color_brewer(palette='Dark2') +
      theme_minimal()
    
  }
  )
  
  output$citypairText <- renderText(paste0('Selected City Pair: ',
                                           airport_lookup(input$us_market_choice,
                                                          input_type = 'IATA',
                                                          output_type='name'),'-',
                                           airport_lookup(input$fg_market_choice,
                                                          input_type='IATA',
                                                          output_type='name')))
 
  output$citypairPlotHeader <- renderText(paste0(input$us_market_choice,'-',
                                                 input$fg_market_choice,' ',
                                                 input$timevar, ' & Market Share ',
                                                 input$date_range[[1]],'-',
                                                 input$date_range[[2]]))
  
   
  output$citypairPlot <- renderPlot({
    

      cityPair_df <- flt_data |>
        filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |>
        filter(usg_apt == input$us_market_choice & fg_apt==input$fg_market_choice) |>
        group_by(data_dte,carrier) |>
        summarise(total_count=sum(.data[[input$timevar]])) |> 
        ungroup() 
        
    
    
    validate(need(nrow(cityPair_df)>0, 'No observed flights between the city pairs during the time period')) 
    
    cityPair_df |>
      complete(data_dte=seq(min(data_dte),max(data_dte),by='month'),carrier,fill=list(total_count=0)) |> 
      ggplot(mapping=aes(x=data_dte, y=total_count, fill = carrier)) +
      geom_area() +
      labs(x='Date',
           y=pretty_variable_names[input$timevar],
           fill='Airline') +
      scale_color_brewer(palette='Dark2') +
      theme_minimal()

  }
  )
  
  output$citytrendlinePlotHeader <- renderText({
    paste0(input$us_market_choice, '-',input$fg_market_choice,' ',input$timevar,
           ' Over Time by Airline ',
           input$date_range[[1]],
           '-',
           input$date_range[[2]])
      
    
  })
  
  output$citytrendlinePlot <- renderPlot({
    cityPair_df <- flt_data |>
      filter(Year >= input$date_range[[1]] & Year <= input$date_range[[2]]) |>
      filter(usg_apt == input$us_market_choice & fg_apt==input$fg_market_choice) |>
      group_by(data_dte,carrier) |>
      summarise(total_count=sum(.data[[input$timevar]])) |> 
      ungroup() 
      
    
    
    validate(need(nrow(cityPair_df)>0, 'No observed flights between the city pairs during the time period')) 
    
    cityPair_df |> 
      complete(data_dte=seq(min(data_dte),max(data_dte),by='month'),carrier,fill=list(total_count=0)) |> 
      ggplot(mapping=aes(x=data_dte, y=total_count, color=carrier)) +
      geom_line() +
      labs(x='Date',
           y=pretty_variable_names[input$timevar],
           color='Airline') + 
      theme_minimal() +
      scale_color_brewer(palette='Dark2') 
  })
  
  

  

      
    }


# Run the application 
shinyApp(ui = ui, server = server)
