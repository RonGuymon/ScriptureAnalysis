############## DASHBOARD TO COUNT WORD FREQUENCY AND RETURN CHARTS ###################
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(magrittr)
library(scriptuRs) # install_github("andrewheiss/scriptuRs")
library(tidytext)
library(plotly)
library(here)
library(DT)
source('wordFrequencyPlotFunction.R')
source('popularWordFunction.R')

# Get the tokenized words ready upon loading the dashboard----
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

# Body----
header <- dashboardHeader(title = 'Scripture Word Analysis')
body <- dashboardBody(
  tabsetPanel(type = 'tabs'
    , tabPanel('Search'
               , fluidRow(
                 column(width = 3,
                        box(width = NULL, status = 'warning',
                            
                            textInput('wordInput', 'Enter a word or phrase', value = 'order')
                            , radioButtons('searchRadio', label = 'Search Approach'
                                           , choices = c('Exact' = 'exact'
                                                         , 'And' = 'and'
                                                         , 'Or' = 'or')
                                           , selected = 'exact')
                            , checkboxGroupInput('volumeCheckBoxes', 'Volume of Scripture'
                                                 , choices = c('Old Testament' = 'OT'
                                                               , 'New Testament' = 'NT'
                                                               , 'Book of Mormon' = 'BoM'
                                                               , 'Doctrine & Covenants' = 'D&C'
                                                               , 'Pearl of Great Price' = 'PGP')
                                                 , selected = c('OT', 'NT', 'BoM', 'D&C', 'PGP'))
                            , radioButtons('sortByRadio', label = 'Sort By'
                                           , choices = c('Book and Chapter' = 'book'
                                                         , 'Frequency' = 'n'))
                            , numericInput('threshholdInput', label = 'Frequency Threshhold'
                                           , min = 1, max = 100, step = 1, value = 1)
                            , radioButtons('pictureRadio', label = 'Include Picture Behind Plot?'
                                           , choices = c('Yes' = 'yes'
                                                         , 'No' = 'no')
                                           , selected = 'no')
                            , actionButton('searchButton', label = 'Search')
                        )
                 )
                 , column(width = 9,
                          box(width = NULL, solidHeader = T,
                              uiOutput('scripPlot.ui')
                          )
                          , box(width = NULL, title = 'Scripture Text (Click on a bar in the bar chart)', background = 'light-blue',
                                htmlOutput('chapterText')
                          )
                 )
               )
               )
    , tabPanel('Most Popular Words'
               , fluidRow(
                 column(width = 3,
                        box(width = NULL, status = 'warning',
                            
                            radioButtons('removeStopWords', label = 'Remove regular stop words?'
                                           , choices = c('Yes' = 'yes'
                                                         , 'No' = 'no')
                                           , selected = 'yes')
                            , textInput('otherStopWords', 'Enter additional stop words to remove', value = 'ye thee thou thy pass')
                            , radioButtons('searchLevel', label = 'Level'
                                           , choices = c('All' = 'all'
                                                         , 'Volume' = 'volume'
                                                         , 'Book' = 'book'
                                                         , 'Chapter' = 'chapter')
                                           , selected = 'volume')
                            , checkboxGroupInput('volumeCheckBoxesPop', 'Volume of Scripture'
                                                 , choices = c('Old Testament' = 'OT'
                                                               , 'New Testament' = 'NT'
                                                               , 'Book of Mormon' = 'BoM'
                                                               , 'Doctrine & Covenants' = 'D&C'
                                                               , 'Pearl of Great Price' = 'PGP')
                                                 , selected = c('OT', 'NT', 'BoM', 'D&C', 'PGP'))
                            , numericInput('maxRankInput', label = 'Max Rank'
                                           , min = 1, max = 100, step = 1, value = 5)
                            , actionButton('summarizeButton', label = 'Summarize')
                        )
                 )
                 , column(width = 9,
                          box(width = NULL, solidHeader = T,
                              dataTableOutput('popTableDt')
                          )
                 )
               )
               )
  )
)
ui <- dashboardPage(header, dashboardSidebar(disable = T), body)
# Server----
server <- function(input, output, session){
  # Summarize button and plot
  observeEvent(input$summarizeButton, {
    popTable <<- popularWordTable(searchLevel = input$searchLevel
                     , volume = input$volumeCheckBoxesPop
                     , maxRank = input$maxRankInput
                     , otherStopWords = input$otherStopWords
                     , removeStopWords = input$removeStopWords
                     , tidy_scriptures)
    req(popTable)
    output$popTableDt <- renderDataTable({
      popTable
    })
  })
  
  # Search button and plot
  observeEvent(input$searchButton, {
    stuff <<- wordFreqPlot(keyWord = isolate(tolower(input$wordInput))
                           , exactAndOr = isolate(input$searchRadio)
                           , volume = isolate(input$volumeCheckBoxes)
                           , grouping = 'chapter'
                           , sortBy = isolate(input$sortByRadio)
                           , threshhold = isolate(input$threshholdInput)
                           , tidy_scriptures = tidy_scriptures
                           , scriptures = scriptures
    )
    req(stuff)
    
    output$scripPlot <- renderPlotly({
      if(isolate(input$pictureRadio) == 'yes'){
        stuff$finalPlot %>%
          layout(images = list(
            list(
              # source = paste0('https://loremflickr.com/320/240/', gsub(' .*$', '', isolate(input$wordInput)))
              source = paste0('https://source.unsplash.com/featured/?', isolate(input$wordInput))
              , xref = 'x'
              , yref = 'y'
              , x = 0
              , y = nrow(stuff$keyWordByGroup)
              , sizex = max(stuff$keyWordByGroup$n, na.rm = T) + 1
              , sizey = nrow(stuff$keyWordByGroup) + 1
              , sizing = 'stretch'
              , opacity = .3
              , layer = 'below'
            )
          ))
      }else{
        stuff$finalPlot
      }     
    })
    
    output$scripPlot.ui <- renderUI({
      plotlyOutput('scripPlot', height = ifelse(nrow(stuff$keyWordByGroup)*15 < 500, 
                                                500
                                                , nrow(stuff$keyWordByGroup)*15)
      )
    })
    
    
    # output$keyWords <- DT::renderDataTable({
    #     stuff$keyWordByGroup
    # })
  })
  
  # Chapter text below the plot
  observeEvent(event_data('plotly_click', source = 'scriptPlot'), {
    x <- event_data('plotly_click', source = 'scriptPlot')
    selectedBook <- stuff$keyWordByGroup[x[[4]],] %>% pull(book_title)
    selectedChapter <- stuff$keyWordByGroup[x[[4]],] %>% pull(chapter_number)
    nInstances <- stuff$keyWordByGroup[x[[4]],] %>% pull(n)
    instanceSingPlural <- 'instance'
    if(nInstances > 1){
      instanceSingPlural <- 'instances'
    }
    chapterText <- scriptures %>%
      filter(book_title == selectedBook & chapter_number == selectedChapter) 
    titleText <- paste0('<b>', toupper(chapterText$book_title[1]), ' '
                        , chapterText$chapter_number[1], '</b> '
                        , '(', nInstances, ' '
                        ,instanceSingPlural,')')
    textToPrint <- chapterText %>%
      mutate(
        textToPrint = paste0('[',verse_number,'] ',text)
      ) %>%
      pull(textToPrint) %>%
      paste(collapse = '<br/>')
    if(isolate(input$searchRadio) == 'exact'){
      textToPrint %<>%
        gsub(isolate(input$wordInput)
             , paste0('<mark>', isolate(input$wordInput), '</mark>')
             , x = .
             , ignore.case = T)
    }else{
      wordsToMark <- base::strsplit(input$wordInput, split = ' ')
      for(wtm in wordsToMark[[1]]){
        replacementText <- paste0('<mark>', wtm, '</mark>')
        textToPrint %<>%
          gsub(wtm, replacementText, x = ., ignore.case = T)
      }
    }
    
    textToPrint <- paste0(titleText, '<br/>', textToPrint)
    output$chapterText <- renderText({textToPrint})
  })
}

# Run the application 
shinyApp(ui = ui, server = server)