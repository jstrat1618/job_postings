# Brief Summary of the

# Set the environment
library(RSQLite)
library(tidyverse)
library(tidytext)

theme_set(theme_classic())

# Getting the Data
conn <- dbConnect(SQLite(), '../../venv/data/jobs_database.db')

query <- dbSendQuery(conn, "SELECT * FROM main_jobs")

dat <- dbFetch(query)

dat <- as_tibble(dat)

dbDisconnect(conn)

hard_skills <- read_csv('../../venv/data/hard_skills.csv')
soft_skills <- read_csv('../../venv/data/soft_skills.csv')


# FOR STOP WORDS
keep_words <- c("r", "present")
my_stop_words <- tidytext::stop_words %>% filter(!word %in% keep_words)

data_jobs <- 
  dat %>%
  filter(grepl('data', tolower(title)))%>%
  mutate(job_lab = case_when(
                            grepl('/', tolower(title))~"Dual Hat", #Be ware of dual hats
                            grepl('data ana', tolower(title))~"Data Analyst",
                            grepl('data eng', tolower(title))~"Data Engineer",
                            grepl('data sci', tolower(title))~"Data Scientist",
                             TRUE ~"Other"))



obj_dat <-
  data_jobs %>%
    filter(job_lab %in% c("Data Analyst", "Data Engineer", "Data Scientist"))

# Data Exploration
obj_dat %>%
  count(job_lab) %>%
  ggplot(aes(reorder(job_lab, n), n))+
  geom_bar(stat='identity')+
  labs(x='Job Title', y='Number of Postings')+
  coord_flip()

word_dat <- 
  obj_dat %>%
  select(id, job_lab, summary) %>%
  mutate(summary = gsub('<.*?>', '', summary)) %>%
  unnest_tokens(word, summary) %>%
  anti_join(my_stop_words, by='word') %>%
  filter(!word  %in% c('nbsp', 'rsquo', 'data'))



word_job <-
  word_dat %>%
  count(word, job_lab)

word_count <-
  word_dat %>%
  count(word) %>%
  arrange(desc(n))


word_count %>%
  rename(total = n) %>%
  head(25) %>%
  left_join(word_job, by='word') %>%
  ggplot(aes(reorder(word, total), n, fill=job_lab))+
  geom_bar(stat='identity')+
  labs(x="Word", y="Number Used", 
       title='Most Common Words in Job Description')+
  coord_flip()


#Hard Skills and Soft Skills
# first let's make sure none of our "hard skills" or "soft skills" are in the stop_words 
hard_test <- tolower(hard_skills$skill) %in% my_stop_words$word
fail_msg <- "FAIL: You have a skill that is also listed in your stop words"
#IF FAIL hard_skills$skill[tolower(hard_skills$skill) %in% stop_words$word]
ifelse(sum(hard_test)==0, "Pass", fail_msg)

soft_test <- tolower(soft_skills$skill) %in% my_stop_words$word
ifelse(sum(soft_test)==0, "Pass", fail_msg)
#IF FAIL soft_skills$skill[tolower(soft_skills$skill) %in% stop_words$word]

hard_skills <-
  hard_skills %>%
  mutate(word = tolower(skill)) %>%
  left_join(word_dat %>%
              count(word), by='word')
  
hard_skills %>%
  ggplot(aes(reorder(skill, n), n))+
  geom_bar(stat='identity')+
  labs(x='Skill', y='Number of Times mentioned in Summary')+
  coord_flip()

soft_skills <-
  soft_skills %>%
  mutate(word = tolower(skill)) %>%
  left_join(word_dat %>%
              count(word), by='word')

soft_skills %>%
  ggplot(aes(reorder(skill, n), n))+
  geom_bar(stat='identity')+
  labs(x='Skill', y='Number of Times mentioned in Summary')+
  coord_flip()

all_skills <-
  hard_skills %>%
  mutate(stype='hard') %>%
  bind_rows(soft_skills %>%
              mutate(stype='soft')) %>%
  filter( n > 5)


# Creating the Matrix- unigrams, bigrams, and trigrams?
dfm <-
  word_dat %>%
  filter(word %in% tolower(all_skills$skill)) %>%
  count(id, word) %>%
  cast_dfm(term = word, document=id, value = n)






# Plot Results
