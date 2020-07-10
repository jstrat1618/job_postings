library(RSQLite)
library(tidyverse)
library(tidytext)
library(ggthemes)
library(RColorBrewer)
library(shiny)


ui <- fluidPage(

    # Application title
    titlePanel("Stackoverflow Jobs"),

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
    #################################################################################################
    #Get Data
    #################################################################################################
    conn <- dbConnect(SQLite(), '../../venv/data/jobs_database.db')
    
    query <- dbSendQuery(conn, "SELECT * FROM main_jobs")
    
    dat <- dbFetch(query)
    
    dat <- as_tibble(dat)
    
    dbDisconnect(conn)
    
    
    # Data Cleaning
    dat$pulled <- as.Date(dat$pulled, format = '%m/%d/%Y')
    

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
