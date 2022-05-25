library(tidyverse)
library(udpipe)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(SnowballC)
library(lubridate)
library(fs)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

patents <- read_csv("_data/patents_tags.csv")

removed_ids = c("CN109960966_A")
extra_ids = c("WO2019043406_A1" , "US10507793_B1", "US10510234_B2")
tags_monitoring = c("road monitoring", "vehicle classification", "speed & trajectory", "gps", "survillance")

monitoring <- patents %>% 
  filter(tags %in% tags_monitoring | patent_id %in% extra_ids) %>% 
  filter(! patent_id %in% removed_ids)

write_csv(monitoring, "_data/monitoring.csv")

monitoring <- monitoring %>%
  mutate(text = paste(title, abstract, claims, sep=" ")) %>%
  mutate(text = tolower(text)) %>%
  mutate(year = year(priority_date))


monitoring_tagged_date <- udpipe_annotate(ud_model, x = monitoring$text, doc_id = monitoring$year) %>%
  as_tibble()
monitoring_tagged_date$phrase_tag <- as_phrasemachine(monitoring_tagged_date$upos, type = "upos")
stats <- keywords_phrases(x = monitoring_tagged_date$phrase_tag, term = tolower(monitoring_tagged_date$token),
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                          is_regex = TRUE, detailed = FALSE)


remove_word <- c("system", "data", "image", "method", "that", "model", "set", "user", "first", "vehicle","device", "images", "claim", "comprise", "base")

set <- monitoring_tagged_date %>%
  filter(upos == "ADJ" | upos == "NOUN" | upos == "VERB") %>%
  filter( !(lemma %in% remove_word)) %>%
  count(lemma, doc_id, sort = TRUE)

set %>% 
  group_by(doc_id)%>%
  slice(which.max(n))

# RAKE to find keywords
pat_stats <- keywords_rake(x = monitoring_tagged_date, term = "lemma", group = "doc_id",
                           relevant = monitoring_tagged_date$upos %in% c("NOUN", "ADJ")) %>%
  as_tibble()

pat_stats %>%
  top_n(10, rake) %>%
  ggplot(aes(x = reorder(keyword, rake), y = rake)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  theme_bw()


