library(stringr)
library(tidytext)
library(tidyverse)

# read in text data files and organise these into a data frame
filenames <- c('1994_post_elections_Mandela.txt', '1994_pre_elections_deKlerk.txt', '1995_Mandela.txt', '1996_Mandela.txt', '1997_Mandela.txt', '1998_Mandela.txt', 
               '1999_post_elections_Mandela.txt', '1999_pre_elections_Mandela.txt', '2000_Mbeki.txt', '2001_Mbeki.txt', '2002_Mbeki.txt', '2003_Mbeki.txt', 
               '2004_post_elections_Mbeki.txt', '2004_pre_elections_Mbeki.txt', '2005_Mbeki.txt', '2006_Mbeki.txt', '2007_Mbeki.txt', '2008_Mbeki.txt', 
               '2009_post_elections_Zuma.txt', '2009_pre_elections_ Motlanthe.txt', '2010_Zuma.txt', '2011_Zuma.txt', '2012_Zuma.txt', '2013_Zuma.txt', 
               '2014_post_elections_Zuma.txt', '2014_pre_elections_Zuma.txt', '2015_Zuma.txt', '2016_Zuma.txt', '2017_Zuma.txt', '2018_Ramaphosa.txt', 
               '2019_post_elections_Ramaphosa.txt', '2019_pre_elections_Ramaphosa.txt', '2020_Ramaphosa.txt', '2021_Ramaphosa.txt', '2022_Ramaphosa.txt', '2023_Ramaphosa.txt')




this_speech <- c()
this_speech[1] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1994_post_elections_Mandela.txt', nchars = 27050)
this_speech[2] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1994_pre_elections_deKlerk.txt', nchars = 12786)
this_speech[3] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1995_Mandela.txt', nchars = 39019)
this_speech[4] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1996_Mandela.txt', nchars = 39524)
this_speech[5] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1997_Mandela.txt', nchars = 37489)
this_speech[6] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1998_Mandela.txt', nchars = 45247)
this_speech[7] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1999_post_elections_Mandela.txt', nchars = 34674)
this_speech[8] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1999_pre_elections_Mandela.txt', nchars = 41225)
this_speech[9] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2000_Mbeki.txt', nchars = 37552)
this_speech[10] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2001_Mbeki.txt', nchars = 41719)
this_speech[11] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2002_Mbeki.txt', nchars = 50544)
this_speech[12] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2003_Mbeki.txt', nchars = 58284)
this_speech[13] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2004_post_elections_Mbeki.txt', nchars = 34590)
this_speech[14] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2004_pre_elections_Mbeki.txt', nchars = 39232)
this_speech[15] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2005_Mbeki.txt', nchars = 54635)
this_speech[16] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2006_Mbeki.txt', nchars = 48643)
this_speech[17] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2007_Mbeki.txt', nchars = 48641)
this_speech[18] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2008_Mbeki.txt', nchars = 44907)
this_speech[19] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2009_post_elections_Zuma.txt', nchars = 31101)
this_speech[20] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2009_pre_elections_Motlanthe.txt', nchars = 47157)
this_speech[21] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2010_Zuma.txt', nchars = 26384)
this_speech[22] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2011_Zuma.txt', nchars = 33281)
this_speech[23] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2012_Zuma.txt', nchars = 33376)
this_speech[24] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2013_Zuma.txt', nchars = 36006)
this_speech[25] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2014_post_elections_Zuma.txt', nchars = 29403)
this_speech[26] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2014_pre_elections_Zuma.txt', nchars = 36233)
this_speech[27] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2015_Zuma.txt', nchars = 32860)
this_speech[28] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2016_Zuma.txt', nchars = 32464)
this_speech[29] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2017_Zuma.txt', nchars = 35981)
this_speech[30] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2018_Ramaphosa.txt', nchars = 33290)
this_speech[31] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2019_post_elections_Ramaphosa.txt', nchars = 42112)
this_speech[32] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2019_pre_elections_Ramaphosa.txt', nchars = 56960)
this_speech[33] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2020_Ramaphosa.txt', nchars = 47910)
this_speech[34] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2021_Ramaphosa.txt', nchars = 43352)
this_speech[35] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2022_Ramaphosa.txt', nchars = 52972)
this_speech[36] <- readChar('https://raw.githubusercontent.com/ClosestNeighbours/DS4I-Project-2/LDA---Puja/2023_Ramaphosa.txt', nchars = 53988)

sona <- data.frame(filename = filenames, speech = this_speech, stringsAsFactors = FALSE)

# extract year and president for each speech
sona$year <- str_sub(sona$filename, start = 1, end = 4)
sona$president_13 <- str_remove_all(str_extract(sona$filename, "[dA-Z].*\\."), "\\.")

# clean the sona dataset by adding the date and removing unnecessary text
replace_reg <- '(http.*?(\\s|.$))|(www.*?(\\s|.$))|&amp;|&lt;|&gt;|\n'

sona <-sona %>%
  mutate(speech = str_replace_all(speech, replace_reg , ' ')
         ,date = str_sub(speech, start=1, end=30)
         ,date = str_replace_all(date, "February", "02")
         ,date = str_replace_all(date, "June", "06")
         ,date = str_replace_all(date, "Feb", "02")
         ,date = str_replace_all(date, "May", "05")
         ,date = str_replace_all(date, "Jun", "06")
         ,date = str_replace_all(date, "Thursday, ","")
         ,date = str_replace_all(date, ' ', '-')        
         ,date = str_replace_all(date, "[A-z]",'')
         ,date = str_replace_all(date, '-----', '')
         ,date = str_replace_all(date, '----', '')
         ,date = str_replace_all(date, '---', '')
         ,date = str_replace_all(date, '--', '')
  )


#preprocessing
sona$date[36] <- "09-02-2023"
x <- sona$speech

y <- sub('^\\w*\\s*\\w*\\s*\\w*\\s*', '', x[1:34])
sona$speech[1:34] <- y

z <- sub("^[A-Za-z]+, \\d{1,2} [A-Za-z]+ \\d{4}  ", "", x[35])
sona$speech[35] <- z

a <- sub("\\d{1,2} [A-Za-z]+ \\d{4}", "", x[36])
sona$speech[36] <- a

#sona$speech <- str_replace_all(sona$speech, "[^[:alnum:]]", " ")

sona$speech <- gsub('[[:digit:]]+', '', sona$speech)    #remove numbers



##eda 

## tidy format
tidy_sona <- sona %>% 
  unnest_tokens(word, speech, token = 'words', to_lower = T) %>%
  filter(!word %in% stop_words$word)


## words per president
tidy_sona %>%
  group_by(president_13) %>% count(word) %>% summarise(n = n())

## common words
tidy_sona %>%
  count(word, sort = TRUE) %>% slice_head(n = 20)

# Identify the most common words for each president
most_common_words_per_pres <- tidy_sona %>% group_by(president_13) %>% count(word) %>% 
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  ungroup()

ggplot(most_common_words_per_pres, aes(x = reorder(word, n), y = n, fill = president_13)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 15 Words by President",
       x = "Word",
       y = "Frequency") +
  theme_minimal() + coord_flip() + theme(legend.position = "none") +
  facet_wrap(~ president_13, scales = "free")




exclude_words <- c("government", "people", "south", "africa", "african")

most_common_words_filtered <- most_common_words_per_pres %>%
  filter(!word %in% exclude_words) %>% filter(president_13 != 'Motlanthe') %>% 
  filter(president_13 != "deKlerk")

# Create the plot using the filtered data frame
ggplot(most_common_words_filtered, aes(x = reorder(word, n), y = n, fill = president_13)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 15 Words by President",
       x = "Word",
       y = "Frequency") +
  theme_minimal() + coord_flip() + theme(legend.position = "none") +
  facet_wrap(~ president_13, scales = "free")



## bigrams
tidy_sona_bigram <- sona %>% 
  unnest_tokens(word, speech, token = 'ngrams', n = 2, to_lower = T) %>%
  filter(!word %in% stop_words$word)

bigrams_separated <- tidy_sona_bigram %>%
  separate(word, c('word1', 'word2'), sep = ' ')

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word)

# join up the bigrams again
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = ' ')

bigrams_filtered %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(rank(desc(n)) <= 10) %>% 
  na.omit()  # if a tweet contains just one word, then the bigrams will return NA

bigrams_counted <- bigrams_united %>% group_by(president_13) %>% count(bigram) %>% arrange(desc(n)) %>%
  slice_head(n = 15) %>%
  ungroup() %>%
  filter(!bigram %in% 
           c("south africa", "south african", "south africans", 'madame speaker', 
             'honourable speaker')) %>% filter(president_13 != 'Motlanthe') %>% 
  filter(president_13 != "deKlerk")

ggplot(bigrams_counted, aes(x = reorder(bigram, n), y = n, fill = president_13)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 15 Words by President",
       x = "Word",
       y = "Frequency") +
  theme_minimal() + coord_flip() + theme(legend.position = "none") +
  facet_wrap(~ president_13, scales = "free")



## tokenise sentences


## tidy format
tidy_sona_sentences <- sona %>% 
  unnest_tokens(word, speech, token = 'sentences', to_lower = T) %>%
  anti_join(stop_words, by = c("word" = "word"))

tidy_sona_sentences %>%
  group_by(president_13) %>% count(word) %>% summarise(n = n())


#remove deklerk and motlanthe

tidy_sona_sentences_filtered <- tidy_sona_sentences %>%
  filter(president_13 != "Motlanthe") %>%
  filter(president_13 != "deKlerk")

tidy_sona_sentences_filtered$word <- 
  str_replace_all(tidy_sona_sentences_filtered$word, "[^[:alnum:]]", " ")


# bag of words
tidy_sona_sentences_filtered <- tidy_sona_sentences_filtered %>% 
  mutate(word_index = row_number())

bag_of_words <- tidy_sona_sentences_filtered %>% 
  unnest_tokens(word, word, token = "words" ) %>%
  select(c(3, 5, 6)) %>% group_by(word_index, president_13) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(word) 

words_to_keep <- bag_of_words %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n)) %>% slice_head(n=1000)

bag_of_words <- tidy_sona_sentences_filtered %>% 
  unnest_tokens(word, word, token = "words" ) %>%
  select(c(3, 5, 6)) %>% group_by(word_index, president_13) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(word) %>% filter(word %in% words_to_keep$word) %>% 
  pivot_wider(names_from = word, values_from = n) 

bag_of_words <- replace(bag_of_words, is.na(bag_of_words),0)
bag_of_words <- bag_of_words %>% rename(President = president_13, 
                                        sentence_index = word_index)


# tf-idf

tfidf_raw <- tidy_sona_sentences_filtered %>% 
  unnest_tokens(word, word, token = "words" ) %>% filter(!word %in% stop_words$word) %>% 
  filter(word %in% words_to_keep$word) %>%
  select(c(3, 5, 6)) %>% group_by(word_index, president_13) %>% count(word) 
  

tfidf_bind <- tfidf_raw %>% bind_tf_idf(word, word_index, n) 

tfidf_reduced <- tfidf_bind %>% select(c(1, 2, 3, 4, 7)) %>% 
  pivot_wider(names_from = word, values_from = tf_idf)


tfidf_reduced <- replace(tfidf_reduced, is.na(tfidf_reduced),0)
tfidf_reduced <- tfidf_reduced %>% rename(President = president_13, 
                                        sentence_index = word_index)



## balanced data?
#bag of words
bag_of_words %>% group_by(President) %>% count() 

bag_of_words_factor <- bag_of_words
bag_of_words_factor$President <- as.factor(ifelse(bag_of_words_factor$President == "Mandela", 0, 
                                                  ifelse(bag_of_words_factor$President == "Mbeki", 1, 
                                                         ifelse(bag_of_words_factor$President == "Zuma", 2, 3))))
set.seed(2493274)

library(caret)
upsampled_bof <- upSample(bag_of_words_factor[, 3:1002], bag_of_words_factor$President)


##tfidf
tfidf_reduced %>% group_by(President) %>% count() 

tfidf_factor <- tfidf_reduced
tfidf_factor$President <- as.factor(ifelse(tfidf_factor$President == "Mandela", 0, 
                                                  ifelse(tfidf_factor$President == "Mbeki", 1, 
                                                         ifelse(tfidf_factor$President == "Zuma", 2, 3))))
set.seed(2493274)

upsampled_bof_tfidf <- upSample(tfidf_factor[, 3:1003], tfidf_factor$President)




#smote
bag_of_words_2 <- bag_of_words
bag_of_words_2$President <- ifelse(bag_of_words_2$President == "Mandela", 0, 
                         ifelse(bag_of_words_2$President == "Mbeki", 1, 
                                ifelse(bag_of_words_2$President == "Zuma", 2, 3)))
bag_of_words_2 <- as.data.frame(bag_of_words_2)
bag_of_words_2$President <- as.factor(bag_of_words_2$President)



library(performanceEstimation)

smote_bow <- smote(President ~., bag_of_words_2[, 2:1002], perc.over = 0.5, 
                   perc.under = 9)
sum(smote_bow$President==0)
sum(smote_bow$President==1)
sum(smote_bow$President==2)
sum(smote_bow$President==3)

smote_tfidf <- smote(President ~., tfidf_factor[, 2:1003], perc.over = 0.5, 
                   perc.under = 9)
sum(smote_tfidf$President==0)
sum(smote_tfidf$President==1)
sum(smote_tfidf$President==2)
sum(smote_tfidf$President==3)


## feed forward neural network

#upsamples x2, smote x2, normal x2

library(keras)
library(tensorflow)

## training, test and validation
#normal bow

set.seed(2493274)
train_index_normal_bof <- createDataPartition(bag_of_words_2$President, 
                                          p = 0.7, list = FALSE, times = 1)
training_data_normal_bof <- bag_of_words_2[train_index_normal_bof, ]
remaining_data_normal_bof <- bag_of_words_2[-train_index_normal_bof, ]

splits_normal_bof <- createDataPartition(remaining_data_normal_bof$President, p = 0.5, 
                                         list = FALSE, times = 1)
test_data_normal_bof <- remaining_data_normal_bof[splits_normal_bof, ]
validation_data_normal_bof <- remaining_data_normal_bof[-splits_normal_bof, ]


training_data_normal_bof_x <- training_data_normal_bof[, 3:1002]
training_data_normal_bof_y <- training_data_normal_bof[, 2]

test_data_normal_bof_x <- test_data_normal_bof[, 3:1002]
test_data_normal_bof_y <- test_data_normal_bof[, 2]

validation_data_normal_bof_x <- validation_data_normal_bof[, 3:1002]
validation_data_normal_bof_y <- validation_data_normal_bof[, 2]


##smote bow
train_index_smote_bow <- createDataPartition(smote_bow$President, 
                                              p = 0.7, list = FALSE, times = 1)
training_data_smote_bow <- smote_bow[train_index_smote_bow, ]
remaining_data_smote_bow <- smote_bow[-train_index_smote_bow, ]

splits_smote_bow <- createDataPartition(remaining_data_smote_bow$President, p = 0.5, 
                                         list = FALSE, times = 1)
test_data_smote_bow <- remaining_data_smote_bow[splits_smote_bow, ]
validation_data_smote_bow <- remaining_data_smote_bow[-splits_smote_bow, ]


training_data_smote_bow_x <- training_data_smote_bow[, 2:1001]
training_data_smote_bow_y <- training_data_smote_bow[, 1]

test_data_smote_bow_x <- test_data_smote_bow[, 2:1001]
test_data_smote_bow_y <- test_data_smote_bow[, 1]

validation_data_smote_bow_x <- validation_data_smote_bow[, 2:1001]
validation_data_smote_bow_y <- validation_data_smote_bow[, 1]

##upsampled bow

train_index_upsampled_bow <- createDataPartition(upsampled_bof$Class, 
                                             p = 0.7, list = FALSE, times = 1)
training_data_upsampled_bow <- upsampled_bof[train_index_upsampled_bow, ]
remaining_data_upsampled_bow <- upsampled_bof[-train_index_upsampled_bow, ]

splits_upsampled_bow <- createDataPartition(remaining_data_upsampled_bow$Class, p = 0.5, 
                                        list = FALSE, times = 1)
test_data_upsampled_bow <- remaining_data_upsampled_bow[splits_upsampled_bow, ]
validation_data_upsampled_bow <- remaining_data_upsampled_bow[-splits_upsampled_bow, ]




training_data_upsampled_bow_x <- training_data_upsampled_bow[, 1:1000]
training_data_upsampled_bow_y <- training_data_upsampled_bow[, 1001]

test_data_upsampled_bow_x <- test_data_upsampled_bow[, 1:1000]
test_data_upsampled_bow_y <- test_data_upsampled_bow[, 1001]

validation_data_upsampled_bow_x <- validation_data_upsampled_bow[, 1:1000]
validation_data_upsampled_bow_y <- validation_data_upsampled_bow[, 1001]


##normal nn
tensorflow::set_random_seed(2493274)
model_nn_normal <- keras_model_sequential() %>%
  layer_dense(units = 124, activation = 'relu', input_shape = 1000) %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 4, activation = 'softmax')


model_nn_normal %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = "adam",
  metrics = "accuracy",
)

summary(model_nn_normal)
dim(training_data_normal_bof_x)
dim(training_data_normal_bof_y)

training_data_normal_bof_x <- as.matrix(training_data_normal_bof_x)
training_data_normal_bof_y <- to_categorical(training_data_normal_bof_y)
tail(training_data_normal_bof_y)
dim(training_data_normal_bof_y)

history_normal <- model_nn_normal %>% fit(
  training_data_normal_bof_x, testing, 
  epochs = 30, batch_size = 5, 
  validation_split = 0.2, shuffle = TRUE
)

save(history_normal, file = "history_normal.RData")



##upsampled nn
tensorflow::set_random_seed(2493274)
model_nn_upsample <- keras_model_sequential() %>%
  layer_dense(units = 124, activation = 'relu', input_shape = 1000) %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 4, activation = 'softmax')


model_nn_upsample %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = "adam",
  metrics = "accuracy",
)


training_data_upsampled_bow_x <- as.matrix(training_data_upsampled_bow_x)
training_data_upsampled_bow_y <- to_categorical(training_data_upsampled_bow_y)
dim(training_data_upsampled_bow_y)

history_upsample <- model_nn_upsample %>% fit(
  training_data_normal_bof_x, training_data_upsampled_bow_y, 
  epochs = 30, batch_size = 5, 
  validation_split = 0.2, shuffle = TRUE
)

save(history_upsample, file = "history_upsample.RData")


##smote nn
tensorflow::set_random_seed(2493274)
model_nn_smote <- keras_model_sequential() %>%
  layer_dense(units = 124, activation = 'relu', input_shape = 1000) %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 4, activation = 'softmax')


model_nn_smote %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = "adam",
  metrics = "accuracy",
)


training_data_smote_bow_x <- as.matrix(training_data_smote_bow_x)
training_data_smote_bow_y <- to_categorical(training_data_smote_bow_y)


history_smote <- model_nn_smote %>% fit(
  training_data_normal_bof_x, training_data_upsampled_bow_y, 
  epochs = 30, batch_size = 5, 
  validation_split = 0.2, shuffle = TRUE
)

save(history_smote, file = "history_smote.RData")



## testing all nn



validation_data_normal_bof_x <- as.matrix(validation_data_normal_bof_x)
validation_data_normal_bof_y <- to_categorical(validation_data_normal_bof_y)

model_nn_normal %>% keras::evaluate(validation_data_normal_bof_x, validation_data_normal_bof_y)
result <- model_nn_normal %>% evaluate(validation_data_normal_bof_x, 
                                       validation_data_normal_bof_y)

validation_data_upsampled_bow_x <- as.matrix(validation_data_upsampled_bow_x)
validation_data_upsampled_bow_y <- to_categorical(validation_data_upsampled_bow_y)

model_nn_upsample %>% evaluate(validation_data_upsampled_bow_x, validation_data_upsampled_bow_y)


validation_data_smote_bow_x <- as.matrix(validation_data_smote_bow_x)
validation_data_smote_bow_y <- to_categorical(validation_data_smote_bow_y)

model_nn_smote %>% evaluate(validation_data_smote_bow_x, validation_data_smote_bow_y)


#y_test_hat <- model_nn_upsample %>% predict(validation_data_smote_bow_x) %>% k_argmax() %>% as.numeric()
# table(y_test_original, y_test_hat)

## cnn
##normal
tensorflow::set_random_seed(2493274)
model_cnn_normal <- keras_model_sequential() %>% 
  layer_embedding(1000, output_dim = 10, input_length = 1000) %>%
  layer_dropout(0.2) %>%
  layer_conv_1d(filters = 64, kernel_size = 8, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(32, activation = "relu") %>%
  layer_dense(4, activation = "softmax")


model_cnn_normal %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

history_cnn_normal <- model_cnn_normal %>% fit(
  training_data_normal_bof_x, training_data_normal_bof_y, 
  epochs = 30, batch_size = 5, 
  validation_split = 0.2, shuffle = TRUE
)

save(history_cnn_normal, file = "history_cnn_normal.RData")


##upsampled
tensorflow::set_random_seed(2493274)
model_cnn_upsampled <- keras_model_sequential() %>% 
  layer_embedding(1000, output_dim = 10, input_length = 1000) %>%
  layer_dropout(0.2) %>%
  layer_conv_1d(filters = 64, kernel_size = 8, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(32, activation = "relu") %>%
  layer_dense(4, activation = "softmax")


model_cnn_upsampled %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

history_cnn_upsampled <- model_cnn_upsampled %>% fit(
  training_data_upsampled_bow_x, training_data_upsampled_bow_y, 
  epochs = 30, batch_size = 5, 
  validation_split = 0.2, shuffle = TRUE
)

save(history_cnn_upsampled, file = "history_cnn_upsampled.RData")


##smote
tensorflow::set_random_seed(2493274)
model_cnn_smote <- keras_model_sequential() %>% 
  layer_embedding(1000, output_dim = 10, input_length = 1000) %>%
  layer_dropout(0.2) %>%
  layer_conv_1d(filters = 64, kernel_size = 8, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(32, activation = "relu") %>%
  layer_dense(4, activation = "softmax")


model_cnn_smote %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

history_cnn_smote <- model_cnn_smote %>% fit(
  training_data_smote_bow_x, training_data_smote_bow_y, 
  epochs = 30, batch_size = 5, 
  validation_split = 0.2, shuffle = TRUE
)

save(history_cnn_smote, file = "history_cnn_smote.RData")


#testing all cnn

model_cnn_normal %>% evaluate(validation_data_normal_bof_x, validation_data_normal_bof_y)

model_cnn_upsampled %>% evaluate(validation_data_upsampled_bow_x, 
                                 validation_data_upsampled_bow_y)

model_cnn_smote %>% evaluate(validation_data_smote_bow_x, validation_data_smote_bow_y)


## random forest

#training

library(caret)

set.seed(2493274)

ctrl <- trainControl(method = 'cv', number = 3, search = "random", verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(500, 1000),
                        interaction.depth = c(1),
                        shrinkage = c(0.01, 0.001),
                        n.minobsinnode = 1)

gbm_gridsearch_normal <- train(President ~ ., data = training_data_normal_bof[2:1002], 
                        method = 'gbm', 
                        distribution = 'multinomial', 
                        trControl = ctrl, 
                        verbose = T, 
                        tuneGrid = gbm_grid)

save(gbm_gridsearch_normal, file = "gbm_gridsearch_normal.RData")
confusionMatrix(gbm_gridsearch_normal)


gbm_grid_searched <- expand.grid(n.trees = c(1000),
                        interaction.depth = c(1),
                        shrinkage = c(0.01),
                        n.minobsinnode = 1)

gbm_gridsearch_upsample <- train(Class ~ ., data = training_data_upsampled_bow, 
                               method = 'gbm', 
                               distribution = 'multinomial', 
                               trControl = ctrl, 
                               metric = "accuracy",
                               verbose = T, 
                               tuneGrid = gbm_grid_searched)

save(gbm_gridsearch_upsample, file = "gbm_gridsearch_upsample.RData")
confusionMatrix(gbm_gridsearch_upsample)


gbm_gridsearch_smote <- train(President ~ ., data = training_data_smote_bow, 
                               method = 'gbm', 
                               distribution = 'multinomial', 
                               trControl = ctrl, 
                               verbose = T, 
                               tuneGrid = gbm_grid_searched)

save(gbm_gridsearch_smote, file = "gbm_gridsearch_smote.RData")
confusionMatrix(gbm_gridsearch_smote)


## validation

## Prediction
gbm_pred_normal <- predict(gbm_gridsearch_normal, validation_data_normal_bof)
confusionMatrix(gbm_pred_normal, validation_data_normal_bof$President)

gbm_pred_upsample <- predict(gbm_gridsearch_upsample, validation_data_upsampled_bow)
confusionMatrix(gbm_gridsearch_upsample, validation_data_upsampled_bow$Class)

gbm_pred_smote <- predict(gbm_gridsearch_smote, validation_data_smote_bow)
confusionMatrix(gbm_gridsearch_smote, validation_data_smote_bow$President)



## final test! 



