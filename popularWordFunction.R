popularWordTable <- function(searchLevel
                            , volume = c('OT', 'NT', 'BoM', 'D&C', 'PGP')
                            , maxRank
                            , otherStopWords
                            , removeStopWords = 'yes'
                            , tidy_scriptures){
  # Stop words
  if(removeStopWords == 'yes'){
    stopWords <- stop_words$word
  }else{
    stopWords <- c()
  }
  otherStopWords <- gsub('[^a-zA-Z ]', '', otherStopWords) %>% tolower(.) %>% str_split(., pattern = ' ')
  stopWords <- c(stopWords, otherStopWords[[1]])
  ntw <- tidy_scriptures %>%
    filter(volume_short_title %in% volume) %>%
    filter(!word %in% stopWords)
  
  # Summarize
  if(searchLevel == 'all'){
    summy <- ntw %>%
      group_by(word) %>%
      summarise(n = n()
                , .groups = 'keep') %>%
      ungroup() %>%
      dplyr::arrange(desc(n)) %>%
      mutate(rank = rank(-n, ties.method = 'min')) %>%
      filter(rank <= maxRank)
    colNames <- c('Word', 'Number of Occurrences', 'Rank')
  }else if(searchLevel == 'volume'){
    summy <- ntw %>%
      group_by(volume_long_title, word) %>%
      summarise(n = n()
                , .groups = 'keep') %>%
      dplyr::arrange(volume_long_title, desc(n)) %>%
      ungroup() %>%
      group_by(volume_long_title) %>%
      mutate(rank = rank(-n, ties.method = 'min')) %>%
      ungroup() %>%
      filter(rank <= maxRank)
    colNames <- c('Volume', 'Word', 'Number of Occurrences', 'Rank')
  }else if(searchLevel == 'book'){
    summy <- ntw %>%
      group_by(volume_long_title, book_long_title, word) %>%
      summarise(n = n()
                , .groups = 'keep') %>%
      dplyr::arrange(volume_long_title, book_long_title, desc(n)) %>%
      ungroup() %>%
      group_by(book_long_title) %>%
      mutate(rank = rank(-n, ties.method = 'min')) %>%
      ungroup() %>%
      filter(rank <= maxRank)
    colNames <- c('Volume', 'Book', 'Word', 'Number of Occurrences', 'Rank')
  }else if(searchLevel == 'chapter'){
    summy <- ntw %>%
      group_by(volume_long_title, book_long_title, chapter_number, word) %>%
      summarise(n = n()
                , .groups = 'keep') %>%
      dplyr::arrange(volume_long_title, book_long_title, chapter_number, desc(n)) %>%
      ungroup() %>%
      group_by(volume_long_title, book_long_title, chapter_number) %>%
      mutate(rank = rank(-n, ties.method = 'min')) %>%
      ungroup() %>%
      filter(rank <= maxRank)
    colNames <- c('Volume', 'Book', 'Chapter', 'Word', 'Number of Occurrences', 'Rank')
  }
  
  DT::datatable(summy, colnames = colNames)
}
