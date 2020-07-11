library(RSQLite)
library(tidyverse)
library(tidytext)
library(ggthemes)
library(RColorBrewer)
library(shiny)


ui <- fluidPage(

    # Application title
    titlePanel("Jobs Posted on Stackoverflow"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("num_rows",
                        "Number of bins:",
                        min = 1,
                        max = 25,
                        value = 13)
        ),

        # Show a plot of the generated distribution
        
        tabPanel("main_df", tableOutput("main_df"))
        #mainPanel()
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
    

    output$main_df <- renderTable(
    dat %>% 
        dplyr::select(title, link, location, pulled) %>%
        rename(Title = title,
               Location = location, 
               `Date Pulled` = pulled) %>%
       head(input$num_rows)
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
