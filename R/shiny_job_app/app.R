library(RSQLite)
library(tidyverse)
library(tidytext)
library(ggthemes)
library(RColorBrewer)
library(scales)
library(plotly)
library(shiny)


theme_set(theme_bw(base_size = 20,
                   base_family = 'Times'))

toproper <- function(x) paste(toupper(substr(x, 0,1)), substr(tolower(x), 2, nchar(x)), sep='')

ui <- shinyUI(
  navbarPage("Jobs Database",
             tabPanel(
               "Filter Data",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("num_rows",
                               "Number of Rows:",
                               min = 1,
                               max = 10,
                               value = 5),
                   textInput("title_contains",label = 
                               'Title Contains (Not case sensitive)'),
                   textInput("summary_contains",label = 
                               'Job Summary Contains (Not case sensitive)'),
                   actionButton('refresh', "Refresh"),
                   textOutput('total_sum')
                 ),
                 
                 # Show a plot of the generated distribution
                 
                 mainPanel(h1('First 5 Entries'), tableOutput("main_df")))
             ),
             tabPanel("View Data by Location", 
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("num_cities",
                                      "Number of Cities:",
                                      min = 2,
                                      max = 25,
                                      value = 5)),
                        mainPanel(plotOutput("city_plt")))),
             
             tabPanel("View Data Over Time",
        
                        mainPanel(plotOutput("ts_plt"))),
             tabPanel("Job Description Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("num_words",
                                      "Number of Words",
                                      min = 2,
                                      max = 25,
                                      value = 10),
                          textInput("word1",label = 
                                      'Enter Word (Not case sensitive)'),
                          textInput("word2",label = 
                                      'Enter Word (Not case sensitive)'),
                          textInput("word3",label = 
                                      'Enter Word (Not case sensitive)'),
                          textInput("word4",label = 
                                      'Enter Word (Not case sensitive)')),
                        mainPanel(plotOutput("word_sum_plt"),
                                  plotOutput("unigram_plt"))))
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
  
  newdata <- reactive({
    
    input$refresh
    isolate({
      
      newdat <- dat
      
      title_contains <- tolower(input$title_contains)
      summary_contains <- tolower(input$summary_contains)
      
      if(!title_contains %in% c(""," ")){
        newdat <- filter(newdat, grepl(title_contains, tolower(dat$title)))
      }
      
      if(!summary_contains  %in% c(""," ")){
        newdat <-  filter(newdat, grepl(summary_contains, tolower(newdat$summary)))
      }
      
      return(newdat)})
    
  })
  
  
  #output$num_jubs <- renderText()
  
  output$main_df <- renderTable(
    
    newdata() %>% 
      dplyr::select(title, link, location, pulled) %>%
      rename(Title = title,
             Location = location, 
             `Date Pulled` = pulled) %>%
      head(input$num_rows)
  )
  
  output$total_sum <- renderText({
    #data.frame(`Total Postings`=comma(nrow(dat)),  Remaining=comma(nrow(newdata())))
    num_tot <- comma(nrow(dat))
    num_left <- comma(nrow(newdata()))
    
    paste("Total: ", num_tot,". Remaining: ", num_left, ".", sep='')
    })
  
  output$city_plt <- renderPlot({
    city_plt <- 
      newdata() %>%
      count(location) %>%
      arrange(desc(n)) %>%
      filter(!(location %in% c(',', "", " ")) & !is.na(location)) %>%
      head(input$num_cities) %>%
      ggplot(aes(reorder(location, n), n))+
      geom_bar(stat='identity')+
      geom_text(aes(x=reorder(location, n), y=n, label=comma(n)), hjust=-0.5)+
      labs(x = "", y = "Number of Postings", title ='Job Posts by City')+   
      coord_flip()
    
    print(city_plt)
  })
  
  output$ts_plt <- renderPlot({
    ts_plt <- 
      newdata() %>%
      count(pulled) %>%
      ggplot(aes(pulled, n))+
      geom_line()+
      geom_point()+
      labs(x = "Date Pulled", y = "Number of Postings", 
           title ='Job Postings over Time')
    
    print(ts_plt)
  })
  
  output$word_sum_plt <- renderPlot({
    word_df <-
      newdata() %>%
      select(summary) %>%
      #Remove html tags
      mutate(summary = gsub('<.*?>', '', summary)) %>%
      unnest_tokens(word, summary) %>%
      # Filter out stop-words but BE CAREFUL- R is a stop-word
      anti_join(tidytext::stop_words %>%
                  filter(word != 'r'), by='word') %>%
      filter(!word  %in% c('nbsp', 'rsquo', 'data', 'science'))%>%
      count(word) %>%
      arrange(desc(n))
    
    word_plt <- 
      word_df %>%
      head(input$num_words) %>%
      mutate(word=toproper(word))%>%
      ggplot(aes(reorder(word, n), n))+
      geom_bar(stat = 'identity')+
      labs(x="", y="Number of Times Used", 
           title='Most Common Words in Job Description')+
      geom_text(aes(reorder(word, n), n, label=comma(n), hjust=-0.1))+
      coord_flip()
    
    
    
    print(word_plt)
  })
  
  output$unigram_plt <- renderPlot({
   
    user_words <- tolower(c(input$word1, input$word2, input$word3, input$word4))
    
    if(sum(user_words == "")>2){
      return(NULL)
      } else{
        
    udf <-
      newdata() %>%
      select(summary) %>%
      #Remove html tags
      mutate(summary = gsub('<.*?>', '', summary)) %>%
      unnest_tokens(word, summary) %>%
      # Filter out stop-words but BE CAREFUL- R is a stop-word
      filter(word %in% user_words)%>%
      count(word) %>%
      arrange(desc(n))
    
    u_plt <- 
      udf %>%
      head(input$num_words) %>%
      mutate(word = toproper(word))%>%
      ggplot(aes(reorder(word, n), n))+
      geom_bar(stat = 'identity')+
      geom_text(aes(reorder(word, n), n, label=comma(n), hjust=-0.1))+
      labs(x="", y="Number of Times Used", 
           title='')+
      coord_flip()
    
    
     print(u_plt)
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)