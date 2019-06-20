library(RSQLite)
library(tidyverse)

#Get Data
conn <- dbConnect(SQLite(), '../venv/data/jobs_database.db')

query <- dbSendQuery(conn, "SELECT * FROM main_jobs")

dat <- dbFetch(query)

dat <- as_tibble(dat)

dbDisconnect(conn)

#Explore Data
city_df <-
  dat %>% 
  count(location) %>%
  arrange(desc(n))

