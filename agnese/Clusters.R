library(udpipe)
library(tidyverse)
library(rebus)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

patents <- read_csv("_data/patents.csv")

patents_tagged <- udpipe_annotate(ud_model, x = patents$title, doc_id = patents$patent_id) %>%
  as_tibble()

patents_tagged$phrase_tag <- as_phrasemachine(patents_tagged$upos, type = "upos")
stats <- keywords_phrases(x = patents_tagged$phrase_tag, term = tolower(patents_tagged$token),
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                          is_regex = TRUE, detailed = FALSE)

tmp_keywords <- stats %>%
  filter( (ngram >= 2 & freq>1) | ngram == 1) %>%
  pull(keyword) %>% 
  unique() %>% 
  or1()

keywords <- BOUNDARY %R% tmp_keywords %R% BOUNDARY

# find the keywords
patents_keyword <- patents %>% 
  mutate(keys = str_match_all(title, keywords))


# matrice di incidenza per grafo
x <- integer(nrow(patents)*nrow(patents))
rown <- patents[["patent_id"]]
coln <- patents[["patent_id"]]
m <- matrix(x, nrow =nrow(patents), byrow = TRUE, 
            dimnames = list(rown, coln))

patents_left = patents[["patent_id"]]
for (p1 in patents[["patent_id"]]){
 
  print(p1)

  t1 = patents_keyword %>% 
    filter(str_detect(patent_id, p1))
  list1 = unlist(t1$keys)
  
  print(list1)
  
  patents_left = patents_left[patents_left != p1]
  for(p2 in patents_left){

    t2 = patents_keyword %>% 
      filter(str_detect(patent_id, p2))
    list2 = unlist(t2$keys)
    
    print(list2)
    
    m[p1, p2] = length(intersect(list1, list2))
    m[p2, p1] = length(intersect(list1, list2))
  }
  
}

save(m, file = "agnese/matrix/matrix_adiac_title.RData")
write.table(m, file="agnese/matrix/title_correlation.txt", col.names = NA)
