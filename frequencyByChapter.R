# Most frequent word in each chapter/book/volume
pacman::p_load(tidyverse, magrittr, lubridate, scriptuRs, tidytext)
# Get the tokenized words ready upon loading the dashboard
scriptures <- lds_scriptures()
tidy_scriptures <- scriptures %>%
  unnest_tokens(word # Name of the column in which the data will be output
                , text # Input name of the dataframe
  ) %>%
  group_by(volume_id, book_id, chapter_id, verse_id) %>%
  dplyr::mutate(
    word_id = 1
    , word_id = cumsum(word_id)
  ) %>%
  ungroup()

otherStopWords <- c('ye', 'thou', 'thy', 'pass')
maxRank <- 5
volly <- tidy_scriptures %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% otherStopWords) %>%
  group_by(volume_long_title, word) %>%
  summarise(n = n()
            , .groups = 'keep') %>%
  dplyr::arrange(volume_long_title, desc(n)) %>%
  ungroup() %>%
  group_by(volume_long_title) %>%
  mutate(rank = rank(-n, ties.method = 'first')) %>%
  ungroup() %>%
  filter(rank <= maxRank)

booky <- tidy_scriptures %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% otherStopWords) %>%
  group_by(book_long_title, word) %>%
  summarise(n = n()
            , .groups = 'keep') %>%
  dplyr::arrange(book_long_title, desc(n)) %>%
  ungroup() %>%
  group_by(book_long_title) %>%
  mutate(rank = rank(-n, ties.method = 'first')) %>%
  ungroup() %>%
  filter(rank <= maxRank)

chappy <- tidy_scriptures %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% otherStopWords) %>%
  group_by(book_long_title, chapter_number, word) %>%
  summarise(n = n()
            , .groups = 'keep') %>%
  dplyr::arrange(book_long_title, chapter_number, desc(n)) %>%
  ungroup() %>%
  group_by(book_long_title, chapter_number) %>%
  mutate(rank = rank(-n, ties.method = 'min')) %>%
  ungroup() %>%
  filter(rank <= maxRank)

olly <- tidy_scriptures %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% otherStopWords) %>%
  group_by(word) %>%
  summarise(n = n()
            , .groups = 'keep') %>%
  ungroup() %>%
  dplyr::arrange(desc(n)) %>%
  mutate(rank = rank(-n, ties.method = 'min')) %>%
  filter(rank <= maxRank)

input <- list()
input$removeStopWords <- 'yes'
input$otherStopWords <- 'ye thee thou thy pass'
input$searchLevel <- 'volume'
input$volumeCheckBoxesPop <- c('OT', 'NT', 'BoM', 'D&C', 'PGP')
input$maxRankInput <- 5
