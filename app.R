library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(shiny)
library(tidytext)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}


ui <- fluidPage(
  
  # task6: add in shinythemes function
  theme = shinytheme("journal"),
  
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(
      # task2: add in the inputs in the sidebarPanel
      selectInput(inputId = 'book_name', label = 'Choose a book', choices = books),
      
      checkboxInput(inputId = 'stop_words', label = 'Stop Words', value = TRUE),
      
      actionButton(inputId = 'action', label = 'Rerun'),
      
      hr(),
      h3("Word Cloud Settings"),
      
      sliderInput(inputId = 'max_words', label = 'Max # of Words', min = 10, max = 200,
                  value = 100, step = 10),
      
      sliderInput(inputId = 'word_size_L', label = 'Size of the Largest Words', min = 1, max = 8,
                  value = 4),
      
      sliderInput(inputId = 'word_size_S', label = 'Size of the Smallest Words', min = 0.1, max = 4,
                  value = 0.5),
      
      hr(),
      h3("Word Count Settings"),
      
      sliderInput(inputId = 'min_words', label = 'Minimum # of Words for Count Chart', min = 10, max = 100,
                  value = 25),
      
      sliderInput(inputId = 'font_size', label = 'Font Size', min = 8, max = 30,
                  value = 14),
    
      
    ),
    
    
    mainPanel(
      tabsetPanel(
        # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
        # task3: add in the outputs in the tabsetPanel
        tabPanel("Word Cloud", plotOutput('cloud'), height = '600px'), 
        tabPanel("Word Counts", plotOutput('freq', height = '600px'))
      )
    )
  )
  
  
  
  
  # task6: and modify your figure heights
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  
  
  
  re <- eventReactive(input$action, {
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$book_name, input$stop_words) 
    })
  })
  
  
  output$cloud <- renderPlot({
    v <- re()
    pal <- brewer.pal(8,"Dark2")
    
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$word_size_L, input$word_size_S),
          random.order = FALSE, 
          max.words = input$max_words, 
          colors=pal))
    
    })

  output$freq <- renderPlot({
    v <- re()
    v %>%
      filter(n>input$min_words)%>%
      ggplot(aes(x=reorder(word, n), y=n)) +
      geom_col() +
      coord_flip() +
      theme(text = element_text(size = input$font_size),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
  })
}

shinyApp(ui = ui, server = server)
