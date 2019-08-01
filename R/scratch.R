library(RSQLite)
library(tidyverse)
library(tidytext)

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

# Most Common cities (Excluding jobs that don't have a city)
city_df %>%
  filter(location != "") %>%
  head(25) %>%
  ggplot(aes(reorder(location, n), n))+
  geom_bar(stat='identity')+
  coord_flip()+
  theme_bw()+
  labs(x = "Number of Postings", y = "City")
  
#Most common states; I would expect CA followed by TX and then NY by population
dat %>%
  select(location) %>%
  separate(location, into = c("city", "state"), sep = ",") %>%
  mutate(state = gsub(" ", "", state)) %>%
  filter(state %in% datasets::state.abb)%>%
  count(state) %>%
  arrange(desc(n)) %>%
  head(10)%>%
  ggplot(aes(reorder(state, n), n)) +
    geom_bar(stat = 'identity')+
    coord_flip()+
    theme_bw()+
    labs(x = 'Number of Postings', y = "State")

# Most common words in job title
dat %>%
  select(title) %>%
  unnest_tokens("word", title) %>%
  anti_join(tidytext::stop_words, by = "word") %>%
  anti_join(tibble(word = tolower(datasets::state.abb)), by = 'word') %>%
  anti_join(city_df %>%
              separate(location, into = c('city', 'state'), sep=',') %>%
              # I have to lower case and rename "cit" so why not just
              mutate(word = tolower(city)), by='word') %>%
  filter(!word %in% c('san', 'antonio', 'diego', 'francisco')) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(25) %>%
  ggplot(aes(reorder(word, n), n))+
    geom_bar(stat='identity')+
    coord_flip()+
    labs(x='Word', y='Number of Times Used', title = "Most Common Words in Title")+
    theme_bw()

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


wmat <-
  dat %>%
  select(id, summary) %>%
  mutate(summary = gsub('<.*?>', '', summary)) %>%
  unnest_tokens(word, summary) %>%
  anti_join(tidytext::stop_words, by='word') %>%
  filter(!word  %in% c('nbsp', 'rsquo')) %>%
  count(id, word) %>%
  tidytext::cast_tdm(document = id, value=word, n)
    
  
