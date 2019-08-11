## app.R ##
library(shiny)
library(shinydashboard)
library(magrittr)

## Auto Addition [>7]: verweisen Abschnitte auf nur einen weiteren, so soll dieser direkt angehängt werden.
## Würfel, Stats, Inventar
## Kampf
## Ende-Marker [E+], [E-1]

source("books.R")

ui <- dashboardPage(
  dashboardHeader(title = "Shiny Books"),
  dashboardSidebar(
    selectInput("selectBook", "Choose Your Adventure", choices = books.booklist()),
    actionButton("buttonLoad", "Start"),
    "Game Stats"
  ),
  dashboardBody(
    tags$head(tags$script(src = "books.js")),
    htmlOutput("htmlChapter")
  )
)

server <- function(input, output, session) {
  
  book <- reactiveVal()
  chapter <- reactiveVal()
  state <- reactiveValues()
  
  # Load new book
  observeEvent(input$buttonLoad, {
    book(books.load.book(input$selectBook))
    chapter(1)
  })
  
  # Render chapter
  output$htmlChapter <- renderUI({
    req(book(), chapter())
    text <- parse.chapter(book()[[chapter()]]$text, state)
    HTML(text)
  })
  
  # JS-induced chapter switch
  observe({ chapter(input$jsSwitchToChapter) })
  
}

shinyApp(ui, server)