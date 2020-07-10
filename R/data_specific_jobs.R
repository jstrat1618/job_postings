library(RSQLite)
library(tidyverse)
library(tidytext)
library(ggthemes)
library(RColorBrewer)


# Helper stuff
theme_set(theme_bw())
toproper <- function(txt){
  substr(txt, 1, 1) <- toupper(substring(txt, 1,1))
  return(txt)
}

#remove_whitespace <-function(txt)gsub(' ', '', txt)


#################################################################################################
#Get Data
#################################################################################################
conn <- dbConnect(SQLite(), '../venv/data/jobs_database.db')

query <- dbSendQuery(conn, "SELECT * FROM main_jobs")

dat <- dbFetch(query)

dat <- as_tibble(dat)

dbDisconnect(conn)


# Data Cleaning
dat$pulled <- as.Date(dat$pulled, format = '%m/%d/%Y')

# Look at posts over time- Note part of this is the way I pulled it
dat %>%
  count(pulled) %>%
  ggplot(aes(pulled, n))+
  geom_line()+
  geom_point()+
  labs(x='', y='Number of posts')

post_df <-
  dat %>%
  mutate(
    title = tolower(title),
    ds_posts = ifelse(grepl('data sci', title), 1, 0),
    da_posts = ifelse(grepl('data ana', title), 1, 0),
    de_posts = ifelse(grepl('data engi', title), 1, 0),
    d_posts = ifelse(grepl('data', title), 1, 0),
    webdev_posts = ifelse(grepl('web dev', title), 1, 0),
    softdev_posts = ifelse(grepl('software dev', title), 1, 0),
    alldev_posts = ifelse(grepl('dev', title), 1, 0))

# Words in Title
word_title_df <- 
  dat %>%
  select(title, pulled) %>%
  unnest_tokens(word, title) %>%
  # Filter out stop-words but BE CAREFUL- R is a stop-word
  anti_join(tidytext::stop_words %>%
              filter(word != 'r'), by='word') %>%
  count(word, pulled) %>%
  arrange(desc(n))

bigram_title_df <-
  dat %>%
  select(title, pulled) %>%
  unnest_tokens("bigram", title, token = "ngrams", n = 2) %>%
  separate(bigram, into = c('word1', 'word2'), remove = FALSE, sep=' ')


bigram_title_df %>%
  filter(word2 == 'developer') %>%
  count(word1) %>%
  arrange(desc(n)) %>%
  head(25) %>%
  ggplot(aes(reorder(word1, n), n))+
  geom_bar(stat='identity')+
  labs(x='Preceeding Word', y='Number of Occurences',
       title='Word Preceeding "Developer"')+
  coord_flip()


post_df %>%
  summarise(
    ds_posts = sum(ds_posts),
    da_posts = sum(da_posts),
    de_posts = sum(de_posts),
    d_posts = sum(d_posts)
  ) %>%
  rename(`Data Science` = ds_posts,
         `Data Analyst` = da_posts,
         `Data Engineer` = de_posts,
         `Data- All` = d_posts) %>%
  pivot_longer(cols = `Data Science`:`Data- All`,
               names_to = 'post',
               values_to ="num_posts") %>%
  ggplot(aes(reorder(post, num_posts), num_posts))+
  geom_bar(stat='identity')+
  labs(x='', y='Number of Posts', title='Posts with "Data" in Title')+
  coord_flip()

post_df %>%
  summarise(
    webdev_posts = sum(webdev_posts),
    softdev_posts = sum(softdev_posts),
    alldev_posts = sum(alldev_posts)
  ) %>%
  rename(`Web Developer` = webdev_posts,
         `Software Developer` = softdev_posts,
         `Developer- All` = alldev_posts
         ) %>%
  pivot_longer(cols = `Web Developer`:`Developer- All`,
               names_to = 'post',
               values_to ="num_posts") %>%
  ggplot(aes(reorder(post, num_posts), num_posts))+
  geom_bar(stat='identity')+
  labs(x='', y='Number of Posts', title='Job Postings')+
  coord_flip()


post_df %>%
  group_by(pulled) %>%
  summarise(
    ds_posts = sum(ds_posts),
    da_posts = sum(da_posts),
    d_posts = sum(d_posts),
    .groups='drop'
  ) %>%
  ggplot()+
  geom_line(aes(pulled, d_posts, col='Data- Other'))+
  geom_line(aes(pulled, ds_posts, col='Data Science'))+
  geom_line(aes(pulled, da_posts, col='Data Analyst'))+
  labs(x='', y='Number of Posts', col='Post Type')
  
  


# MAIN LINES
ds_dat <- 
  dat %>%
  mutate(title = tolower(title)) %>%
  filter(grepl('data sci', title))


city_df <-
  ds_dat %>% 
  count(location) %>%
  arrange(desc(n)) %>%
  filter(location != ',')

city_df %>%
  filter(location != "") %>%
  head(25) %>%
  ggplot(aes(reorder(location, n), n))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(x = "", y = "City", title ='Data Science Posts')


# By State
city_df %>%
  separate(location, into = c('city', 'state'), sep=',') %>%
  mutate(state = gsub(" ", "", state)) %>%
  group_by(state) %>%
  summarise(state_tot = sum(n), .groups="drop")%>%
  filter(!is.na(state) & nchar(state) == 2) %>%
  arrange(desc(state_tot))%>%
  head(25) %>%
    ggplot(aes(reorder(state, state_tot), state_tot))+
    geom_bar(stat = 'identity')+
    coord_flip()+
    labs(x = "Total Postings", y = "State")


#################################################################################################
# Word Breakdown
#################################################################################################
# Most common words in job summary
word_df  <- 
  ds_dat %>%
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



bigram_df <- 
  ds_dat %>%
  select(summary) %>%
  mutate(summary = gsub('<.*?>', '', summary)) %>%
  unnest_tokens("bigram", summary, token = "ngrams", n = 2) %>%
  count(bigram) %>%
  arrange(desc(n)) %>%
  separate(bigram, into = c('word1', 'word2'), remove = FALSE, sep=' ')


word_df %>%
  head(25) %>%
  ggplot(aes(reorder(word, n), n))+
  geom_bar(stat = 'identity')+
  labs(x="", y="Number of Times Used", 
       title='Most Common Words in Job Description')+
  coord_flip()


bigram_df %>%
  head(25) %>%
  ggplot(aes(reorder(bigram, n), n))+
  geom_bar(stat = 'identity')+
  labs(x="Word", y="Number Used", 
       title='Most Common Words in Job Description')+
  coord_flip()


############
# Software #
############
bigram_df %>%
  filter(bigram %in% c("power bi", 'data studio'))

word_df %>%
  filter(word == 'tableau')


# Programming Language
langs <- c('r', 'python', 'octave', 'julia', 'golang', 'rust',
           'scala', 'swift', 'octave', 'matlab', 'sas', 'java', 'c++', 'c')

lang_df <- tibble(lang = langs)

lang_df %>%
  left_join(word_df %>% 
              filter(word %in% langs) %>%
              rename(lang = word), by='lang') %>%
  mutate(n = replace_na(n, 0)) %>%
  ggplot(aes(reorder(toproper(lang), n), n))+
  geom_bar(stat = 'identity')+
  labs(x = '', y = 'Number of Times used', 
       title = "Programming Languages used in Data Science Job Descriptions")+
  coord_flip()

# R and Python over time
ds_dat %>%
  select(summary, pulled) %>%
  #Remove html tags
  mutate(summary = gsub('<.*?>', '', summary)) %>%
  unnest_tokens(word, summary) %>%
  # Filter out stop-words but BE CAREFUL- R is a stop-word
  anti_join(tidytext::stop_words %>%
              filter(word != 'r'), by='word') %>%
  filter(word  %in% c('r', 'python'))%>%
  count(word, pulled) %>%
  ggplot(aes(pulled, n, col=toproper(word)))+
  geom_line()+
  scale_color_manual(values = c('#D7DF01', '#0101DF'))+
  # Override theme because I think it looks better on this plot
  theme_dark()+
  labs(x='', y='Number of Times Used', col='', title='R and Python over Time')


# Cluster Words
dtm <- 
  ds_dat %>%
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
