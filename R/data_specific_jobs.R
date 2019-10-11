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
  filter(grepl('data sci', title))


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
word_df  <- 
  dat %>%
  select(summary) %>%
  mutate(summary = gsub('<.*?>', '', summary)) %>%
  unnest_tokens(word, summary, ) %>%
  # Filter out stop-words but BE CAREFUL- R is a stop-word
  anti_join(tidytext::stop_words %>%
              filter(word != 'r'), by='word') %>%
  filter(!word  %in% c('nbsp', 'rsquo', 'data', 'science'))%>%
  count(word) %>%
  arrange(desc(n))
  
word_df %>%
  head(25) %>%
  ggplot(aes(reorder(word, n), n))+
  geom_bar(stat = 'identity')+
  labs(x="Word", y="Number Used", 
       title='Most Common Words in Job Description')+
  coord_flip()

word_df %>% filter(word %in% c('r', 'python'))

# Cluster Words
dtm <- 
  dat %>%
  select(id, summary) %>%
  mutate(summary = gsub('<.*?>', '', summary)) %>%
  unnest_tokens(word, summary) %>%
  anti_join(tidytext::stop_words, by='word') %>%
  # get rid of some straggling html
  filter(!word  %in% c('nbsp', 'rsquo')) %>%
  # get rid of the obvious
  filter(!word %in% c('data', 'science'))%>%
  filter(!grepl('[0-9]', word)) %>%
  count(id, word) %>%
  cast_dtm(term = word, document =id, value = n, weighting = tm::weightTf)



pca <- prcomp(dtm, scale = TRUE)

pca_df <- 
  pca$x %>%
  as_tibble()


pca_df %>%
  #Some Extreme outliers appear to cluster the data
  filter(abs(PC1) < mean(PC1) + 2*sd(PC1) & abs(PC2) < mean(PC2) + 2*sd(PC2)) %>%
  ggplot(aes(PC1, PC2))+
  geom_point()


num_clust <- 2:15
wss <- numeric()

for(i in num_clust){
  fit <- kmeans(pca_df, centers = i)
}

wss <- c(fit$totss, wss)
num_clust <- 1:15

plot(num_clust, wss)
