######## FUNCTION FOR WORD FREQUENCY PLOT #############
wordFreqPlot <- function(keyWord
                         , exactAndOr = 'and'
                         , volume = c('OT', 'NT', 'BoM', 'D&C', 'PGP')
                         , grouping = 'chapter'
                         , sortBy = 'book'
                         , threshhold = 1
                         , tidy_scriptures
                         , scriptures
){
  # exactAndOr indicates whether you want an exact string, all the words in the string but in any order, or any of the words in the string
  # grouping can be chapter or verse
  
  if(exactAndOr == 'or'){
    searchTerm <- strsplit(tolower(keyWord), ' ') %>% unlist() %>%
      paste0(., collapse = '|')
    keyWordByGroup <- tidy_scriptures %>%
      dplyr::filter(volume_short_title %in% volume) %>%
      dplyr::filter(grepl(searchTerm, x = word)) %>%
      group_by(volume_short_title, book_title, book_id, chapter_number, chapter_id) %>%
      summarise(n = n()
                , .groups = 'drop_last') %>%
      ungroup()
  }else if(exactAndOr == 'and'){
    searchTerm <- strsplit(tolower(keyWord), ' ') %>% unlist()
    keyWordByGroup <- tidy_scriptures %>%
      dplyr::filter(volume_short_title %in% volume) %>%
      mutate(
        vol_book_chap = paste(volume_short_title, book_title, chapter_id, sep = '_')
      )
    allKeyWords <- data.frame()
    for(i in 1:length(searchTerm)){
      if(i == 1){
        temp <- keyWordByGroup %>%
          dplyr::filter(grepl(searchTerm[i], ignore.case = T, x = word))
      }else{
        temp <- keyWordByGroup %>%
          dplyr::filter(vol_book_chap %in% allKeyWords$vol_book_chap) %>%
          dplyr::filter(grepl(searchTerm[i], ignore.case = T, x = word))
        
        allKeyWords %<>%
          dplyr::filter(vol_book_chap %in% temp$vol_book_chap)
      }
      if(nrow(temp) == 0){
        allKeyWords <- temp
        cat('No chapters have all of those words.\n')
        break
      }else if(i == 1){
        allKeyWords <- temp
      }else if(nrow(allKeyWords) == 0){
        cat('No chapters have all of those words.\n')
        break
      }else{
        allKeyWords %<>% bind_rows(temp)
      }
      rm(temp)
    }
    keyWordByGroup <- allKeyWords %>%
      select(-vol_book_chap)
    rm(allKeyWords)
    keyWordByGroup %<>%
      dplyr::arrange(volume_short_title, book_title, book_id, chapter_number, chapter_id, word_id) %>%
      group_by(volume_short_title, book_title, book_id, chapter_number, chapter_id) %>%
      summarise(n = n()
                , .groups = 'drop_last') %>%
      ungroup()
  }else if(exactAndOr == 'exact'){
    if(length(strsplit(keyWord, ' ') %>% unlist()) == 1){
      keyWordByGroup <- tidy_scriptures %>%
        dplyr::filter(volume_short_title %in% volume) %>%
        dplyr::filter(word == keyWord) %>%
        group_by(volume_short_title, book_title, book_id, chapter_number, chapter_id) %>%
        summarise(n = n()
                  , .groups = 'drop_last') %>%
        ungroup()
    }else{
      keyWord <- gsub('[^a-zA-Z0-9 ]', '', keyWord) %>% tolower()
      keyWordByGroup <- scriptures %>%
        mutate(
          text = gsub('[^a-zA-Z0-9 ]', '', text) %>% tolower()
          , n = str_count(text, pattern = tolower(keyWord))
        ) %>%
        filter(n > 0) %>%
        group_by(volume_short_title, book_title, book_id, chapter_number, chapter_id) %>%
        summarise(n = n()
                  , .groups = 'drop_last') %>%
        ungroup()
    }
  }
  

  if(grouping == 'chapter'){
    keyWordByGroup %<>%
      dplyr::arrange(book_id, chapter_id) %>%
      dplyr::select(-book_id, -chapter_id) %>%
      dplyr::mutate(
        book_title_abr = abbreviate(book_title, minlength = 2)
        , book_chapter = paste(book_title_abr, chapter_number, sep = '_')
        , book_chapter = factor(book_chapter, levels = unique(book_chapter))
        , volume_short_title = factor(volume_short_title, levels = unique(volume_short_title))
        # , rankPct = percent_rank(n)
        , rank = rank(n)
        , rank = abs(max(rank) - rank + 1)
      )
    
    countForPlotVolume <- keyWordByGroup %>%
      group_by(volume_short_title) %>%
      summarise(groupCount = sum(n, na.rm = T)) %>%
      ungroup()
    
    countForPlotBook <- keyWordByGroup %>%
      group_by(book_title) %>%
      summarise(groupCount = sum(n, na.rm = T)) %>%
      ungroup()
    
    keyWordByGroup %<>%
      left_join(countForPlotVolume, by = 'volume_short_title') %>%
      mutate(
        volume_short_title = paste0(volume_short_title, ' (', groupCount, ')')
      ) %>%
      select(-groupCount)
    
    keyWordByGroup %<>%
      left_join(countForPlotBook, by = 'book_title') %>%
      mutate(
        book_title_wc = paste0(book_title, ' (', groupCount, ')')
      ) %>%
      select(-groupCount)
    
  }
  
  if(sortBy == 'n'){
    keyWordByGroup %<>% 
      arrange(n) %>%
      mutate(
        book_chapter = fct_inorder(book_chapter)
      )
  }
  if(threshhold < 1){
    threshhold <- 1
  }
  keyWordByGroup %<>%
    filter(n >= threshhold)
  totalCount = sum(keyWordByGroup$n, na.rm = T)
  if(length(unique(keyWordByGroup$volume_short_title)) == 1){
    p <- ggplot(keyWordByGroup, aes(x = book_chapter, y = n, fill = book_title_wc))
  }else{
    p <- ggplot(keyWordByGroup, aes(x = book_chapter, y = n, fill = volume_short_title))
  }
  p <- p +
    geom_bar(stat = 'identity') + 
    labs(title = paste0('Distribution of ', totalCount, ' Instances of "', keyWord, '" by Chapter')
         , x = 'Book and Chapter'
         , y = paste0('Count of ', keyWord)) +
    coord_flip() +
    theme_classic() +
    guides(fill = guide_legend(title = 'Volume'))
  
  finalPlot <- ggplotly(p, source = 'scriptPlot') %>%
    event_register('plotly_click')

  
  returnList <- list()
  returnList$keyWordByGroup <- keyWordByGroup
  returnList$finalPlot <- finalPlot
  return(returnList)
}