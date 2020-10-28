#
# 
#
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(plotly)

# setwd("~/OneDrive/Documents/R/dashboards_learning/rig_counts_states")

# Read in the Baker Hughes rig count dataset
tidy_df <- read_csv('US_land_rig_counts_tidy.csv')

# Filter the data to only include states with significant rig counts
tidy_df_filt <- tidy_df %>% group_by(state) %>% 
    summarize(total=sum(land_rig_count)) %>%
    filter(total > 1000)

# Define the list of state names for the ui input picker
state_names <- unique(tidy_df_filt$state)
state_names

# Define UI for application that plots rig counts for selected states and date range
ui <- fluidPage(

    # Application title
    titlePanel(" U.S. Land Rig Counts, January 2000 - August 2020"),

    # Sidebar with a picker input for state and slider input for date range
    sidebarLayout(
        sidebarPanel(
            pickerInput('state', 'State', 
                        choices=state_names, options = list(`actions-box` = TRUE),
                        selected = c('Colorado', 'N. Dakota', 'Wyoming'), multiple = T),
            sliderInput("date_range",
                        "Date range:",
                        min = as.Date(min(tidy_df$DATE), "%Y-%m-%d"),
                        max = as.Date(max(tidy_df$DATE), "%Y-%m-%d"),
                        value = c(as.Date("2003-08-08"), as.Date("2020-04-09")),
                        timeFormat="%Y-%m-%d")
        ),

        # Show the plot of rig counts for selected states and date range
        mainPanel(
            plotly::plotlyOutput("rig_counts", height = '800')
        )
    )
)


server <- function(input, output, session){
    
    output$rig_counts <- plotly::renderPlotly({
        ggplotly({
        
            # Filter data to plot from input selections
            data_state <- tidy_df %>% filter(state %in% input$state) %>%
            filter(DATE >= input$date_range[1] & DATE <= input$date_range[2])
        
            # Generate plot of filtered data
            p <- ggplot(data_state) + 
                geom_line(aes(x = DATE, y = land_rig_count, col = state)) +
                labs(x = "Date", y = "Rig Count")
                #ggtitle("Drilling Rig Counts for Selected States and Date Range")
            #plot_ly(data_state, x = ~DATE, y = ~land_rig_count, color = ~state, type = 'scatter', mode = 'lines') %>%
                #layout(title = "Drilling Rig Counts for Selected States and Dates",
                #xaxis = list(title = "Date"),
                #yaxis = list(title = "Number of Rigs"))
            p
        })
        
    })

}

shinyApp(ui = ui, server = server, options = list(height = 1080))
