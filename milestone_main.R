library(tidytext)
library(wordcloud)
library(tidyverse)
library(ggraph)
library(igraph)


length(news <- readLines("en_US.news.txt", warn = FALSE))
length(twitter <- readLines("en_US.twitter.txt", warn = FALSE)) 
length(blogs <- readLines("en_US.blogs.txt", warn = FALSE))

rm(news, twitter, blogs)

txt_files <- list.files(pattern = ".txt")

df <- list.files(pattern = "*.txt") %>% 
  map_chr(~ read_file(.)) %>% 
  tibble(text = .) %>% 
  drop_na() %>% 
  mutate(filename = txt_files) %>%
  unnest_tokens(word, text) 

df <- df %>% mutate(word = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = NA)) %>% 
  drop_na() %>% 
  anti_join(stop_words) 


word_counts2 <- df %>% 
  group_by(filename, word) %>% 
  summarise(Total = n()) %>%  
  arrange(desc(Total))

rare_words <- word_counts2 %>% filter(Total == 1)
df <- df %>% anti_join(rare_words, by = 'word')

word_counts2 %>% 
  group_by(filename) %>% 
  top_n(30) %>% 
  ggplot(aes(reorder(word, -Total), Total, fill = filename)) + 
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col() + 
  coord_flip() + 
  facet_wrap(~filename, scales = "free") +
  scale_y_reordered() +
  scale_y_continuous(expand = c(0,0)) 

cloud_count <- df %>% select(word) %>%  
  group_by(word) %>% 
  summarise(Total = n()) %>% 
  arrange(desc(Total)) %>% 
  mutate(word = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = NA)) %>% 
  drop_na() %>% 
  anti_join(stop_words)

wordcloud(cloud_count$word, cloud_count$Total, max.words = 120)

bigrams <- list.files(pattern = "*.txt") %>% 
  map_chr(~ read_file(.)) %>% 
  tibble(text = .) %>% 
  drop_na() %>% 
  mutate(filename = txt_files) %>%
  
  unnest_tokens(word, text, token = 'ngrams', n = 2) %>% 
  count(word, sort = TRUE) %>%
  separate(word, into =c('first_word', 'second_word'), sep = '\\s') %>% 
  anti_join(stop_words, by=c(first_word='word' ) ) %>%
  anti_join(stop_words, by=c(second_word='word' ) ) %>%
  anti_join(rare_words, by=c(first_word='word') ) %>% 
  anti_join(rare_words, by=c(second_word= 'word')) %>% 
  mutate(ngram = paste(first_word, second_word))

bigrams <- bigrams %>% mutate(ngram = gsub(x = ngram, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = NA)) %>% 
  drop_na()


wordcloud(bigrams$ngram, bigrams$n, max.words = 50)

bigram_graph <- bigrams %>% 
  filter(n > 1700) %>% 
  graph_from_data_frame()  




ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph::ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()