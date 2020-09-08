library(tidyverse)
theme_set(theme_light())


medium_datasci <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-12-04/medium_datasci.csv")


medium_datasci <- medium_datasci %>% 
  select(-x1) %>% 
  mutate(post_id = row_number())
  
medium_datasci %>% 
  count(publication, sort = T)

medium_datasci %>% 
  count(author, sort = T) %>% 
  view()

medium_datasci %>% 
  summarise_at(vars(starts_with("tag_")), sum)



#which tags are the most popular based on the number of claps?
medium_gathered <- medium_datasci %>% 
  gather(tag, value, starts_with("tag_")) %>% 
  mutate(tag = str_remove(tag, "tag_")) %>% 
  filter(value == 1)
medium_gathered %>% 
  count(tag, sort = T)

#this shows which tag to use if you want to write a popular article
medium_gathered %>%
  group_by(tag) %>% 
  summarize(median_claps = median(claps)) %>% 
  arrange(desc(median_claps)) 

medium_gathered %>%
  group_by(tag) %>% 
  summarize(mean_claps = mean(claps)) %>% 
  arrange(desc(mean_claps)) 


medium_gathered %>% 
  ggplot(aes(claps))+
  geom_histogram()+
  scale_x_log10(labels = scales::comma_format())


#What is the distribution of reading time
medium_gathered %>% 
  mutate(reading_time = pmin(10, reading_time)) %>% 
  ggplot(aes(reading_time))+
  geom_histogram(binwidth = .5)+
  scale_x_continuous(breaks = seq(2,10,2), labels = c(seq(2,8,2), "10+"))


medium_gathered %>% 
  group_by(tag) %>% 
  summarize(reading_time = mean(reading_time)) %>% 
  arrange(desc(reading_time))

#####Text Mining
library(tidytext)

medium_datasci$title <- as.character(medium_datasci$title)
medium_datasci$subtitle <- as.character(medium_datasci$subtitle)

medium_words <- medium_datasci %>%
  filter(!is.na(title)) %>% 
  select(post_id, title, subtitle, year, reading_time, claps) %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter((!word %in% c("de", "en", "la", "para")), str_detect(word, "[a-z]")) #takes out spanish word and all numbers


medium_words %>%   
  count(word, sort = T) %>% 
  mutate(word = fct_reorder(word, n)) %>% #ensures the words are plotted in descending order 
  head(20) %>% 
  ggplot(aes(word, n))+
  geom_col()+
  coord_flip()+
  labs(title = "Common words in Medium Titles")

#gets the 50 most common words (we know it is 50 because that is what count is returning), 49 really.
medium_words_filter <- medium_words %>% 
  add_count(word) %>% #counting the occurence of each individual word and creating column "n()"
  filter(n >= 250) 

tag_claps <- medium_words_filter %>% 
  group_by(word) %>% 
  summarize(median_claps = median(claps), 
            geometric_mean_claps = exp(mean(log(claps+1)))-1, #the +1 is added here because claps can be zero
            occurences = n()) %>%
  arrange(desc(median_claps))


#If one word appears in a title, what else will I see
library(widyr)               
top_word_corrs <- medium_words_filter %>% 
                    select(post_id, word) %>% 
                    pairwise_cor(word, post_id, sort = T) %>% 
                    head(150)



#visualize the results
library(ggraph)
library(igraph)

vertices <- tag_claps %>% 
              filter(word %in% top_word_corrs$item1 | word %in% top_word_corrs$item2)

  
set.seed(2018)

top_word_corrs %>% 
  graph_from_data_frame(vertices = vertices) %>% 
  ggraph(layout = 'nicely')+
  geom_edge_link()+
  geom_node_point(aes(size = occurences *1.1))+
  geom_node_point(aes(size = occurences, color = geometric_mean_claps))+
  geom_node_text(aes(label = name), repel = T)+
  scale_color_gradient2(low = "Blue", high = "red", midpoint = median(vertices$geometric_mean_claps))+
  theme_void()+
  labs(color = "Claps (mean)", size = "Number of Occurences")
  

#####predicting the number of claps based on title and tag (influence on the number of claps based on word choice)

#turn into a sparse matrix
post_word_matrix <- medium_words_filter %>% 
                        distinct(post_id, word, claps) %>% 
                        cast_sparse(post_id, word)

dim(post_word_matrix)
post_word_matrix[1:10, 1:10]


#fit a lasso model
library(glmnet)

claps <- medium_datasci$claps[match(rownames(post_word_matrix), medium_datasci$post_id)]

lasso_model <- cv.glmnet(post_word_matrix, log(claps+1))
plot(lasso_model)

tidy(lasso_model$glmnet.fit) %>% 
  view()


library(broom)
tidy(lasso_model) %>% 
  view()
lasso_model$lambda.min

tidy(lasso_model$glmnet.fit) %>% 
  filter(lambda == lasso_model$lambda.min) %>% 
  arrange(desc(estimate)) %>% 
  view()


            