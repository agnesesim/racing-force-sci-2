library(tidyverse)
library(rebus)

patents <- read_csv("_data/patents.csv")

# funzione per la fusione e la pulizia del testo 
get_text_clean <- function(data){
  data_text <- data %>%
    # riunisco tutto il testo del patent sotto un solo attributo 'text'
    mutate(text = paste(title, abstract, claims, sep=" ")) %>%
    # trasformo tutto il testo in minuscolo
    mutate(text = tolower(text)) %>% 
    # rimuovo la punteggiatura
    #mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
    # estraggo solo le 3 colonne interessanti per la ricerca
    select(patent_id, text) %>%
    mutate(totwords = str_count(text, "\\s")+1) 
  
  return(data_text)
}

patents_text <- get_text_clean(patents)

image_words<-c("image elaborat",
               "computer vision" ,
               "image analys",
               "image process",
               "image data",
               "neural network")

tech_words<-c(     	 
  "vr",
  "virtual real" ,
  "visor" ,
  "viewer" ,
  "simulator",
  "satellite navigator" ,
  "display" ,
  "gps",
  "(?<![\\w])camera",
  "(?<![\\w])cam(?![\\w])",
  "helmet",
  "helmet.{1,10}cam.",
  "cam.{1,10}helmet",
  "foot.{1,10}cam",
  "foot-cam",
  "shoulder{1,10}cam",
  "shoulder-cam"
)

sport_words <-c(
  "(?<![\\w])sport",
  "(?<![\\w])race(?![\\w])",
  "racetrack",
  "race.{1,10}track",
  "speedway",
  "speed.{1,10}way",
  "(?<![\\w])ride",
  "car.{1,5}rac",
  "boat.{1,5}rac",
  "race.{1,5}car",
  "racing{1,5}car",
  "race.{1,5}boat",
  "racing{1,5}boat",
  "race.{1,5}rally",
  "racing{1,5}rally",
  "rally.{1,5}race",
  "rally{1,5}racing.",
  "race.{1,5}kart.",
  "racing{1,5}kart.",
  "kart.{1,5}race.",
  "kart{1,5}racing.",
  "moto.{1,5}sport.",
  "moto.{1,5}racing",
  "racing{1,5}moto.")

image_words <- image_words %>%
  unique() %>% 
  or1()

tech_words <- tech_words %>%
  unique() %>% 
  or1()

sport_words <- sport_words %>%
  unique() %>% 
  or1()


image_words_regex <- BOUNDARY %R% image_words %R% BOUNDARY
patents_text <- patents_text %>% 
  mutate(image_matches = str_match_all(text, image_words_regex)) %>%
  mutate(image_count = lengths(image_matches)) %>%
  mutate(image_freq = (image_count/totwords)*100) 

tech_words_regex <- BOUNDARY %R% tech_words %R% BOUNDARY
patents_text <- patents_text %>% 
  mutate(tech_matches = str_match_all(text, tech_words_regex)) %>%
  mutate(tech_count  = lengths(tech_matches)) %>%
  mutate(tech_freq = (tech_count/totwords)*100)

motor_words_regex <- BOUNDARY %R% sport_words %R% BOUNDARY
patents_text <- patents_text %>% 
  mutate(motor_matches = str_match_all(text, motor_words_regex)) %>%
  mutate(motor_count  = lengths(motor_matches)) %>%
  mutate(motor_freq = (motor_count/totwords)*100)

patents_text <- patents_text %>%
  mutate(class="")


find_class <- function(id){
  out <- tryCatch(
    {
      pat = patents_text %>% 
        filter(str_detect(patent_id, id))
      
      class_freq = max(pat$image_freq, pat$tech_freq, pat$motor_freq)
      
      if (pat$image_freq == class_freq ){
        pat$class="image"
      }
      else if (pat$tech_freq == class_freq ){
        pat$class="tech"
      }
      else if (pat$motor_freq == class_freq ){
        pat$class="sport"
      }
      
      return (pat)
    },
    error=function(cond) {
      message(paste("Error:",cond))
      return(NA)
    },
    warning=function(cond) {
      message(paste("Warning:",cond))
      return(NULL)
    }
  )
  return(out)
}

data <-lapply(patents_text$patent_id,
              find_class)
patents_class <- do.call(rbind, data)

patents_class %>%
count(class, sort = TRUE) %>%
  mutate(class = reorder(class, n)) %>%
  ggplot(aes(class, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()


patents_image <- patents_class %>% 
  filter(class == "image")

patents_tech <- patents_class %>% 
  filter(class == "tech")

patents_sport <- patents_class %>% 
  filter(class == "sport")
