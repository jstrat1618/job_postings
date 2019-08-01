library(RSQLite)
library(tidyverse)
library(tidytext)

#Get Data
conn <- dbConnect(SQLite(), '../venv/data/jobs_database.db')

query <- dbSendQuery(conn, "SELECT * FROM main_jobs")

dat <- dbFetch(query)

dat <- as_tibble(dat)

dbDisconnect(conn)

# MAIN LINES
dat <- 
  dat %>%
  mutate(title = tolower(title)) %>%
  filter(grepl('data', title))


city_df <-
  dat %>% 
  count(location) %>%
  arrange(desc(n))

city_df %>%
  filter(location != "") %>%
  head(25) %>%
  ggplot(aes(reorder(location, n), n))+
  geom_bar(stat='identity')+
  coord_flip()+
  theme_bw()+
  labs(x = "Number of Postings", y = "City")

# Most common words in job summary
dat %>%
  select(summary) %>%
  mutate(summary = gsub('<.*?>', '', summary)) %>%
  unnest_tokens(word, summary) %>%
  anti_join(tidytext::stop_words, by='word') %>%
  filter(!word  %in% c('nbsp', 'rsquo'))%>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(25) %>%
  ggplot(aes(reorder(word, n), n))+
  geom_bar(stat = 'identity')+
  labs(x="Word", y="Number Used", 
       title='Most Common Words in Job Description')+
  coord_flip()


dat %>%
  select(id, summary) %>%
  mutate(summary = gsub('<.*?>', '', summary)) %>%
  unnest_tokens(word, summary) %>%
  anti_join(tidytext::stop_words, by='word') %>%
  filter(!word  %in% c('nbsp', 'rsquo')) %>%
  count(id, word) %>%
  tidytext::cast_tdm(document = id, value=word, weighting = tm::weightTf)
