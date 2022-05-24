library(udpipe)
library(tidyverse)
library(rebus)
library(igraph)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

patents <- read_csv("_data/patents.csv")

patents_tagged <- udpipe_annotate(ud_model, x = patents$abstract, doc_id = patents$patent_id) %>%
  as_tibble()

patents_tagged$phrase_tag <- as_phrasemachine(patents_tagged$upos, type = "upos")
stats <- keywords_phrases(x = patents_tagged$phrase_tag, term = tolower(patents_tagged$token),
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                          is_regex = TRUE, detailed = FALSE)

tmp_keywords <- stats %>%
  filter(freq > 30) %>%
  #filter( (ngram >= 2 & freq>3) | (ngram == 1 & freq>30)) %>%
  pull(keyword) %>% 
  unique() %>% 
  or1()

keywords <- BOUNDARY %R% tmp_keywords %R% BOUNDARY

# find the keywords
patents_keyword <- patents %>% 
  mutate(keys = str_match_all(abstract, keywords))


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

save(m, file = "agnese/matrix/matrix_adiac_abstract.RData")
write.table(m, file="agnese/matrix/abstract_correlation.txt", col.names = NA)

load(file="agnese/matrix/matrix_adiac_abstract.RData")

tmp_m = m
diag(tmp_m) <- 1

g = graph_from_adjacency_matrix(m, weighted = TRUE)
g=delete.edges(g, which(E(g)$weight <3))

min(E(g)$weight)
max(E(g)$weight)
ecount(g)

coords = layout_with_kk(g)
p <- plot(g,
          vertex.size = 5,
          vertex.color = "white",
          vertex.shape = "circle",
          vertex.label = NA,
          edge.width = 1,
          edge.color = "black",
          edge.lty = 3,
          edge.label = NA,
          edge.curved = TRUE,
          layout = coords)


# cosine similarity
euclidean = function(x) {sqrt(x %*% x)}
d = apply(tmp_m, 2, euclidean)
D = diag(1/d)
S = D %*% t(tmp_m) %*% tmp_m %*% D

# distance matrix
D = 1-S
# distance object
d = as.dist(D)
d[is.na(d)]
d[is.nan(d)]
sum(is.infinite(d)) # THIS SHOULD BE 0


# average-linkage clustering method
cc = hclust(d, method = "average")
# plot dendrogram
plot(cc)

clusters = cutree(cc, k =3)
# plot graph with clusters
coords = layout_with_fr(g)
p <- plot(g,
          vertex.size = 20,
          vertex.color = clusters,
          vertex.shape = "circle",
          vertex.label = NA,
          edge.width = 3,
          edge.color = "black",
          edge.lty = 1,
          edge.label = NA,
          edge.curved = TRUE,
          arrow.mode = 0,
          layout = coords)

