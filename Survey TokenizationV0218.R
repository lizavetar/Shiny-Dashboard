library(textreadr)
library(dplyr)
library(tidytext)
library(stringr)
library(quanteda)
library(RColorBrewer)
library(topicmodels)
library(wordcloud)


#### Making Survey Into DataFrame ####

survey <- read_document(file="Polished answers Text Analytics (1).docx")

a <- length(survey)/6
b <- 6
my_df <- as.data.frame(matrix(nrow=a, ncol=b))
for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- survey[i*b+z-b]
  }#closing z loop
}#closing i loop

my_df$group<-rep(1:4,each =7)

#### Stop words ####

word<-c('night', 'perfect', 'lot', "it's","celebrity", "celebrities", "favorite", "love", "he's", "she's", "it's",'wake', "that's", "i’m", 'gonna','beauty', 'products', 'body',"that's","don't",'sf', 'live', 'francisco',"celebrity", "celebrities", "favorite", "love", "he's", "she's", "it's")

cust_stop<- data_frame(word,lexicon = rep('CUST', each = length(word)))

#### Tokenization of each column####

V1 <- data_frame(my_df$V1, my_df$group)
V2 <- data_frame(my_df$V2 ,my_df$group)
V3 <- data_frame(my_df$V3, my_df$group)
V4 <- data_frame(my_df$V4, my_df$group)
V5 <- data_frame(my_df$V5, my_df$group)
V6 <- data_frame(my_df$V6, my_df$group)


frequencies_tokens_nostop_1 <- V1 %>%
  group_by(`my_df$group`) %>%
  unnest_tokens(word, my_df$V1) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) %>%
  count(word, sort=TRUE)



View(frequencies_tokens_nostop_1)

frequencies_tokens_nostop_2 <- V2 %>%
  group_by(my_df$group)%>%
  unnest_tokens(word, my_df$V2) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) %>%#here's where we remove tokens
  count(word, sort=TRUE)



frequencies_tokens_nostop_3 <- V3 %>%
  group_by(my_df$group)%>%
  unnest_tokens(word, my_df$V3) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) %>%
  count(word, sort=TRUE)



frequencies_tokens_nostop_4 <- V4 %>%
  group_by(my_df$group)%>%
  unnest_tokens(word, my_df$V4) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) %>%
  count(word, sort=TRUE)


frequencies_tokens_nostop_5 <- V5 %>%
  group_by(my_df$group)%>%
  unnest_tokens(word, my_df$V5) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) %>%
  count(word, sort=TRUE)


frequencies_tokens_nostop_6 <- V6 %>%
  group_by(my_df$group)%>%
  unnest_tokens(word, my_df$V6) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) %>%
  count(word, sort=TRUE)

View(all_token)

all_token <- bind_rows(mutate(frequencies_tokens_nostop_1, location = "Question1"),
                       mutate(frequencies_tokens_nostop_2, location = "Question2"),
                       mutate(frequencies_tokens_nostop_3, location = "Question3"),
                       mutate(frequencies_tokens_nostop_4, location = "Question4"),
                       mutate(frequencies_tokens_nostop_5, location = "Question5"),
                       mutate(frequencies_tokens_nostop_6, location = "Question6"))

names(all_token) <-c("group", "word", "n", "location")

#### Sentiment Analysis ####

afinn <- get_sentiments('afinn')
nrc <- get_sentiments('nrc')
bing <- get_sentiments('bing')

sentiments <- bind_rows(mutate(afinn, lexicon = 'afinn'),
                        mutate(nrc, lexicon = 'nrc'),
                        mutate(bing, lexicon = 'bing'))

afinn_V1 <- V1 %>%
  unnest_tokens(word, my_df$V1) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")


afinn_V2 <- V2 %>%
  unnest_tokens(word, my_df$V2) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

afinn_V3 <- V3 %>%
  unnest_tokens(word, my_df$V3) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

afinn_V4 <- V4 %>%
  unnest_tokens(word, my_df$V4) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

afinn_V5 <- V5 %>%
  unnest_tokens(word, my_df$V5) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

afinn_V6 <- V6 %>%
  unnest_tokens(word, my_df$V6) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

print(c(afinn_V1, afinn_V2, afinn_V3, afinn_V4, afinn_V5, afinn_V6))


#### Pizza Sentiment ####

nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise") #what is your sentiment

frequencies_tokens_nostop_1 %>%
  inner_join(get_sentiments("nrc") ) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 ) %>%



frequencies_tokens_nostop_2 %>%
  inner_join(get_sentiments("nrc") ) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 )

frequencies_tokens_nostop_3 %>%
  inner_join(get_sentiments("nrc") ) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 )

frequencies_tokens_nostop_4 %>%
  inner_join(get_sentiments("nrc") ) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 )

frequencies_tokens_nostop_5 %>%
  inner_join(get_sentiments("nrc") ) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 )

frequencies_tokens_nostop_6 %>%
  inner_join(get_sentiments("nrc") ) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 )





frequencies_tokens_nostop_1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 )


frequencies_tokens_nostop_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 )

frequencies_tokens_nostop_3 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 )

frequencies_tokens_nostop_4 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 )

frequencies_tokens_nostop_5 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 )

frequencies_tokens_nostop_6 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment, sort =T) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5,0.5), #Fit the thing in the window
                   fixed.asp=TRUE,
                   title.size=1 )



#### Word Cloud ####

frequencies_tokens_nostop_1 %>%
  with(wordcloud(word, n, max.words = 100))


frequencies_tokens_nostop_2 %>%
  with(wordcloud(word, n, max.words = 100))

frequencies_tokens_nostop_3 %>%
  with(wordcloud(word, n, max.words = 100))

frequencies_tokens_nostop_4 %>%
  with(wordcloud(word, n, max.words = 100))

frequencies_tokens_nostop_5 %>%
  with(wordcloud(word, n, max.words = 100))

frequencies_tokens_nostop_6 %>%
  with(wordcloud(word, n, max.words = 100))



#### ZIPFS ####

total_words <- all_token %>%
  group_by(location)%>%
  summarize(total=sum(n)) #Calculate the all the tokens per book

question_words <- left_join(all_token, total_words)


######################################
########## ZIPF's law ################ To see low frequecy but important business insights
######################################


freq_by_rank <- question_words %>%
  group_by(location) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

###################################################
################# TF_IDF ##########################
###################################################


question_words <- question_words %>%
  bind_tf_idf(word, location, n)

question_words # we get all the zeors because we are looking at stop words ... too common

question_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

question_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(location) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=location))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~location, ncol=2, scales="free")+
  coord_flip()


#### ZIPFS for groups! ####

total_words <- all_token %>%
  group_by(group)%>%
  summarize(total=sum(n)) #Calculate the all the tokens per book

question_words <- left_join(all_token, total_words)


######################################
########## ZIPF's law ################ To see low frequecy but important business insights
######################################


freq_by_rank <- question_words %>%
  group_by(group) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

###################################################
################# TF_IDF ##########################
###################################################


question_words <- question_words %>%
  bind_tf_idf(word, group, n)

question_words # we get all the zeors because we are looking at stop words ... too common

question_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

question_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(group) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=group))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~group, ncol=2, scales="free")+
  coord_flip()


#### Bigram Analysis ####

# putting untokenized questions together 

V1$id <- seq(from = 1, to = 28, by = 1)
my_bigrams <-  V1%>%
  unnest_tokens(bigram, `my_df$V1`, token = "ngrams", n=2)

my_bigrams 

my_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word2 %in% cust_stop$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts


bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(id, bigram) %>%
  bind_tf_idf(bigram, id, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idfp

#------ Toics LDA model -------

all_dtm <- all_token %>%
  cast_dtm( location, word, n)

survey_lda <- LDA(all_dtm, k=2, control=list(seed=123)) #K= no of th

survey_topics <- tidy(survey_lda, matrix="beta") #beta prob for given token in given topic 
survey_topics$beta <- round(survey_topics$beta , digits = 3)


library(ggplot2)
library(dplyr)

top_terms <- survey_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#lets calculate the relative difference between the betas for words in topic 1
#and words in topic 2

beta_spread <- survey_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.001 | topic2 >0.001) %>% #For classify topic 1 and topic by log 
  mutate(log_rate = log2(topic2/topic1))

beta_spread


#------ Documents -------


###############################################
#### Per documnet classification ##############
###############################################




survey_gamma <- tidy(survey_lda, matrix="gamma")
survey_gamma 






survey_gamma %>%
  mutate(document=reorder(document, gamma*topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot()+
  facet_wrap(~document)

survey_classifications <- survey_gamma %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup()

survey_classifications




all_token <- all_text %>% 
  filter(location == "Q1") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(paste0("cust_stop_","Q1")) %>%
  count(word, sort = T) %>%
  aes(x=n, y=word) %>%
  geom_bar()









