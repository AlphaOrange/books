# books.R #

library(dplyr)
library(stringr)
# library(openssl)
# source("db.R")

books.booklist <- function() { list.files("Books", "*.txt") }

books.load.book <- function(filename) {
  
  # Load data
  book.data <- read.table(paste0("books/", filename),
                          fileEncoding = "UTF-8",
                          sep = "\n", comment.char = "", stringsAsFactors = FALSE,
                          blank.lines.skip = FALSE)[, 1]

  # Split into sections
  nlines <- length(book.data) %>% as.numeric
  overview <- book.data %>%
    str_match("^##([0-9]+)## ?(.*)$") %>%
    set_colnames(c("Full", "Number", "Title")) %>%
    as_tibble %>%
    select(- Full) %>%
    mutate(Line = 1:n()) %>%
    filter(!is.na(Number)) %>%
    mutate(End = lead(Line, 1, default = !!nlines + 1))
  
  book <- list()
  for (i in seq_len(nrow(overview))) {
    chapter <- list(
      title = overview$Title[i],
      text = book.data[(overview$Line[i] + 1) : (overview$End[i] - 1)]
    )
    book[[i]] <- chapter
  }
  
  # Pre-Parse sections
  book <- lapply(book, function(chapter) {
    
    # Remove blank lines leading or trailing
    text.lines <- (chapter$text != "") %>% which
    chapter$text <- chapter$text[min(text.lines):max(text.lines)]
    
    # Remove all single blank lines
    blank.lines <- which(chapter$text == "")
    remove.blanks <- blank.lines[!(blank.lines %in% (blank.lines - 1))]
    if (length(remove.blanks) > 0) {
      chapter$text <- chapter$text[- remove.blanks]
    }
    
    # Format paragraphs and line breaks and combine into one text
    chapter$text[chapter$text != ""] <- paste0("<p>", chapter$text[chapter$text != ""], "</p>")
    chapter$text[chapter$text == ""] <- "<br>"
    chapter$text <- chapter$text %>% paste(collapse = "")
    
    # Add title if available
    if (chapter$title != "") {
      chapter$text <- paste0("<h3>", chapter$title, "</h3>", chapter$text)
    }
    
    # Create section links
    chapter$text <- str_replace_all(chapter$text, "\\[([0-9])+\\]([^\\]]+)\\[/[0-9]+\\]", '<a onclick = "toChapter(\\1)">\\2</a>')
    
    # Create story endings
    chapter$text <- str_replace_all(chapter$text, "\\[E\\+\\]", '<p><b>Glückwunsch, du hast die Geschichte erfolgreich beendet!</b></p>')
    chapter$text <- str_replace_all(chapter$text, "\\[E-([0-9])+\\]", '<p><b>Hier endet die Geschichte leider für dich.
                                    <a onclick = "toChapter(\\1)">Klicke hier</a>, um es noch einmal von vorne zu versuchen.</b></p>')
    
    chapter
  })
  
  book
    
}

# Late-parse chapter (e.g. variable-depending events) # TODO: named variables, named chapters
parse.chapter <- function(chapter, state) {
  
  # Update numeric variables
  var.queries <- str_match_all(chapter, "\\[V([0-9])+ ?= ?([0-9]+)\\]")[[1]]
  
  if (nrow(var.queries) > 0) {
    for (i in 1:nrow(var.queries)) {
      state[[var.queries[i, 2]]] <- as.numeric(var.queries[i, 3])
    }
  }
  
  chapter <- str_replace_all(chapter, "\\[V([0-9])+ ?= ?([0-9]+)\\]", "")

  # Replace numeric variables
  var.queries <- str_match_all(chapter, "\\[V([0-9])+\\]")[[1]]
  
  if (nrow(var.queries) > 0) {
    var.queries <- var.queries[!duplicated(var.queries), , drop = FALSE]
    for (i in 1:nrow(var.queries)) {
      if (is.null(state[[var.queries[i, 2]]])) { state[[var.queries[i, 2]]] <- 0 } # if un-initialised set to 0
      # if (TRUE) { state[[var.queries[i, 2]]] <- 0 } # if un-initialised set to 0
      chapter <- str_replace_all(chapter, paste0("\\[V", var.queries[i, 2], "\\]"), as.character(state[[var.queries[1, 2]]]))
    }
  }
  
  # Replace conditional text

  # Return parsed text
  chapter
  
}