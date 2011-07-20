library("stringr")
library("bibtex")
library("brew")
library("plyr")
library("tools")
library("sinartra")

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

preview <- function(show = FALSE) {
  router <- Router$clone()
  router$set_file_path(getwd())
  router$get("*", function(splat, ...) {
    if (splat == "") return(router$redirect("/"))
    
    path <- file.path(getwd(), splat)
    if (file_test("-d", path)) {
      index <- file.path(path, "index.html") 
      if (file.exists(index)) {
        path <- index
      } else {         
        return(sinartra:::render(dir(path)))
      }
    }
    static_file(path)
  })
  route <- function(path, query, ...){
    router$route(path, query)
  } 
  port <- tools:::httpdPort
  if (port == 0L) {
    port <- startDynamicHelp()    
  }
  assignInNamespace("httpd", route, "tools")
  if (show) {
    home <- str_c("http://localhost:", port, "/index.html")
    browseURL(home)        
  }
}