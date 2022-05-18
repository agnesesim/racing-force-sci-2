library(udpipe)
library(tidyverse)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

patents <- read_csv("_data/patents.csv")

patents_tagged <- udpipe_annotate(ud_model, x = patents$abstract, doc_id = patents$patent_id) %>%
  as_tibble()

patents_tagged %>%
  count(upos) %>%
  ggplot(aes(x = reorder(upos, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  theme_bw()

graph_noun <- patents_tagged %>%
  filter(upos == "NOUN") %>%
  count(lemma) %>%
  filter(n > 100) %>%
  ggplot(aes(x = reorder(lemma, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  theme_bw()
graph_noun

graph_adj <- patents_tagged %>%
  filter(upos == "ADJ") %>%
  count(lemma) %>%
  filter(n > 40) %>%
  ggplot(aes(x = reorder(lemma, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  theme_bw()
graph_adj

graph_vb <- patents_tagged %>%
  filter(upos == "VERB") %>%
  count(lemma) %>%
  filter(n > 80) %>%
  ggplot(aes(x = reorder(lemma, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  theme_bw()
graph_vb

# RAKE to find keywords
pat_stats <- keywords_rake(x = patents_tagged, term = "lemma", group = "doc_id",
                           relevant = patents_tagged$upos %in% c("NOUN", "ADJ")) %>%
  as_tibble()

pat_stats %>%
  top_n(10, rake) %>%
  ggplot(aes(x = reorder(keyword, rake), y = rake)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  theme_bw()

# Pointwise Mutual Information Collocations
patents_tagged$word <- tolower(patents_tagged$token)
pat_stats <- keywords_collocation(x = patents_tagged, term = "word", group = "doc_id")
pat_stats$key <- factor(pat_stats$keyword, levels = rev(pat_stats$keyword))

pat_stats %>%
  top_n(10, pmi) %>%
  ggplot(aes(x = reorder(key, pmi), y = pmi)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  theme_bw()

# sequence of POS TAGS
# transform the pos in letters, to be readed by the tool
patents_tagged$phrase_tag <- as_phrasemachine(patents_tagged$upos, type = "upos")
# extract exact patterns of POS, using regex
stats <- keywords_phrases(x = patents_tagged$phrase_tag, term = tolower(patents_tagged$token),
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                          is_regex = TRUE, detailed = FALSE)

stats %>%
  filter(ngram >1 ) %>%
  top_n(10, freq) %>%
  ggplot(aes(x = reorder(keyword, freq), y = freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  theme_bw()
