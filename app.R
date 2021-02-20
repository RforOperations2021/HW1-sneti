#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(maps) 

#load and clean data
protests <- read.csv("protests_cleaned.csv")
protests$Mnth_Yr <- as.factor(format(as.Date(protests$fixeddate), "%B %Y"))
protests$Mnth_Yr <- factor(protests$Mnth_Yr, c("January 2020", "February 2020", "March 2020", "April 2020", "May 2020", "June 2020", "July 2020", "August 2020", "September 2020", "October 2020", "November 2020", "December 2020", "January 2021", "February 2021"))

mainstates <- map_data("state")

protestsmainstates <- filter(protests, admin1 != "Alaska", admin1 != "Hawaii")

# Define UI for application using titlepanels and fluidRows
ui <- fluidPage(

    # Application title and data source
    titlePanel(HTML("<h1>Tracking Protests in 2020</h1>
                    <br>
                    <h4>Data Source: <a href = https://acleddata.com/special-projects/us-crisis-monitor/>ACLED US Crisis Monitor</a></h4><br>")),

    #main panel with inputs and plots
    fluidRow(column(2,downloadButton("download", "Download selected data"))),
    
    hr(),
    
    fluidRow(
        #choose event type
        column(4,selectInput(input = "eventtype",
                        label = "Event Type:",
                        choices = unique(protests$event_type),
                        selected = "Protests",
                        multiple = T)),
        
        #choose state
        column(4,selectInput(input = "state",
                        label = "Choose which states to show:",
                        choices = unique(protests$admin1),
                        selected = "Pennsylvania",
                        multiple = T)),
        
        #choose date range
        column(4,dateRangeInput(input = "daterange",
                           label = "Choose the time frame you'd like to view data for:",
                           start = min(protests$fixeddate),
                           end = max(protests$fixeddate),
                           min = min(protests$fixeddate),
                           max = max(protests$fixeddate))),
        
        #show first plot, time vs number of events by event type
        fluidRow(
            column(8, offset = 2, align="center", plotOutput("distPlot"))
            ),
        
        #second and third plots on one row
        #one is number of events by state
        #two is subtypes of events chosen 
        fluidRow(
            column(6,plotOutput("statePlot")),
            column(6, plotOutput("subtypePlot"))
        ),
        
        fluidRow(column(7, offset=3, align="center", plotOutput("mapPlot"))),
        
        #data table, showing based on inputs selected 
        fluidRow(
            column(12, DT::dataTableOutput(outputId = "table"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #create subset of dataset based on all user input
    protestssubset <- reactive({
        req(input$eventtype)
        req(input$state)
        req(input$daterange)
        protests %>%
        filter(event_type %in% input$eventtype, admin1 %in% input$state, as.Date(fixeddate) >= input$daterange[1], as.Date(fixeddate) <= input$daterange[2])
    })
    
    protestsmainstatessubset <- reactive({
        req(input$eventtype)
        req(input$state)
        req(input$daterange)
        protestsmainstates %>%
        filter(event_type %in% input$eventtype, admin1 %in% input$state, as.Date(fixeddate) >= input$daterange[1], as.Date(fixeddate) <= input$daterange[2])
    })
    
    #create summary table grouped by state and event type
    bystate <-reactive({
        protestssubset() %>%
                group_by(admin1, event_type) %>%
                summarise(count=n(), percent_of_total = (n()/nrow(protestssubset()))*100, deaths = sum(fatalities))
    })
    
    #grouped by month/year and event type
    bydate <- reactive({
        protestssubset() %>%
                group_by(Mnth_Yr, event_type) %>%
                summarise(count=n(), percent_of_total = (n()/nrow(protestssubset()))*100, deaths = sum(fatalities))
    })
    
    #grouped by event type and subtype 
    bysubtype <- reactive({
        protestssubset() %>%
            group_by(event_type, sub_event_type) %>%
            summarise(count=n(), percent_of_total = (n()/nrow(protestssubset()))*100, deaths = sum(fatalities))
    })
    
    #line chart of time vs number of events, colored by events 
    output$distPlot <- renderPlot({
        ggplot(data = bydate(), aes(x=Mnth_Yr, y=count)) + 
            geom_line(aes(color = event_type, group = event_type), size = 1.25) +
            geom_point(aes(color = event_type), size = 2) +
            scale_color_brewer(palette="Set2") +
            theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    })
    
    #bar chart showing number of events for each state, colored by events
    output$statePlot <- renderPlot({
        ggplot(data = bystate(), aes(x=count, y=admin1)) + 
            geom_bar(aes(fill = event_type), stat = 'identity') +
            scale_fill_brewer(palette="Set2")
    })
    
    #bar chart showing subtype by event type 
    output$subtypePlot <- renderPlot({
        ggplot(data=bysubtype(), aes(x=sub_event_type, y=count)) +
            geom_bar(aes(fill = event_type), stat='identity') + 
            scale_fill_brewer(palette="Set2") +
            theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    })
    
    output$mapPlot <- renderPlot({
        ggplot() + 
            geom_polygon(data=mainstates, aes(x=long, y=lat, group=group), color = "white", fill = "grey") + 
            geom_point(data=protestsmainstates, aes(x=longitude, y=latitude), color = "black", alpha = .25) + 
            geom_point(data=protestsmainstatessubset(), aes(x=longitude, y=latitude, color = event_type))
    })
    
    #output datatable based on subset of data
    output$table <- DT::renderDataTable(
        DT::datatable(data = protestssubset(), 
                      options = list(pageLength = 10), 
                      rownames = FALSE))
    
    output$download <- downloadHandler(
        filename = function() {
            paste("protestsdata", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(protestssubset(), file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
