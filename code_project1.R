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
most_common_words_per_pres <- tidy_sona %>% group_by(president_13) %>% count(word) %>% arrange(desc(n)) %>%
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
  filter(!word %in% exclude_words)

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
  slice_head(n = 20) %>%
  ungroup() %>%
  filter(!bigram %in% c("south africa", "south african", "south africans", 'madame speaker', 'honourable speaker'))

ggplot(bigrams_counted, aes(x = reorder(bigram, n), y = n, fill = president_13)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 20 Words by President",
       x = "Word",
       y = "Frequency") +
  theme_minimal() + coord_flip() + theme(legend.position = "none") +
  facet_wrap(~ president_13, scales = "free")



## tokenise sentences


## tidy format
tidy_sona_sentences <- sona %>% 
  unnest_tokens(word, speech, token = 'sentences', to_lower = T) %>%
  anti_join(stop_words, by = c("word" = "word"))

