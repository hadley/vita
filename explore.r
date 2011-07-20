library("stringr")
library("bibtex")
library("brew")
library("plyr")
library("tools")

me <- read.bib("me.bib", "UTF-8")
source("render.r")
source("render-page.r")

type <- vapply(me, function(x) attr(unclass(x)[[1]], "bibtype"), 
  FUN.VALUE = character(1))
by_type <- split(me, type)


titles <- c("Article" = "Journal articles", "Book" = "Books", "InCollection" = "Book chapters", "PhdThesis" = "Theses", "TechReport" = "Technical reports", 
"Unpublished" = "Selected presentations and posters")

paths <- c("Article" = "articles", "Book" = "books", "InCollection" = "chapters", "PhdThesis" = "theses", "TechReport" = "tech-reports", 
"Unpublished" = "presentations")

make_thumbs()
for(type in names(titles)) {
  page <- render_page(by_type[[type]], type)
  
  values <- new.env(parent = globalenv())
  values$title <- titles[type]
  values$content <- page$content
  values$sidebar <- page$sidebar
  values$header <-  page$header
  
  brew("template.html", str_c(paths[type], ".html"), envir = values)
  
}

for(id in seq_along(me)) {
  entry <- me[[id]]
  if (str_detect(entry$key, ":")) next()
  
  page <- render_entry_page(entry)
  
  values <- new.env(parent = globalenv())
  values$title <- entry$title
  values$content <- page$content
  values$sidebar <- "&nbsp;"
  values$header <-  page$header
  
  brew("template.html", str_c("papers/", entry$key, ".html"), 
    envir = values) 
}