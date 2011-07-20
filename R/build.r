titles <- c("Article" = "Journal articles", "Book" = "Books", "InCollection" = "Book chapters", "PhdThesis" = "Theses", "TechReport" = "Technical reports", 
"Unpublished" = "Selected presentations and posters")

paths <- c("Article" = "articles", "Book" = "books", "InCollection" = "chapters", "PhdThesis" = "theses", "TechReport" = "tech-reports", 
"Unpublished" = "presentations")

make_vita <- function(path = "~/documents/vita/public", templates = "~/documents/vita/vita") {
  old <- setwd(path)
  on.exit(setwd(old))
  
  me <- read.bib("me.bib", "UTF-8")
  me <- lapply(me, clean_bib)

  type <- vapply(me, function(x) x$bibtype, FUN.VALUE = character(1))
  by_type <- split(me, type)

  make_thumbs()

  for(id in seq_along(me)) {
    entry <- me[[id]]
    if (str_detect(entry$key, ":")) next()

    page <- render_entry_page(entry)

    values <- new.env(parent = globalenv())
    values$title <- entry$title
    values$content <- page$content
    values$sidebar <- "&nbsp;"
    values$header <-  page$header

    brew(file.path(templates, "template-article.html"), 
      str_c("papers/", entry$key, ".html"), envir = values) 
  }

  for(type in names(titles)) {
    page <- render_page(by_type[[type]], type)

    values <- new.env(parent = globalenv())
    values$title <- str_c(titles[type], " by Hadley Wickham.")
    values$content <- page$content
    values$sidebar <- page$sidebar
    values$header <-  page$header

    brew(file.path(templates, "template.html"), 
      str_c(paths[type], ".html"), envir = values)
  }
  
}

make_thumbs <- function() {
  papers <- file_path_sans_ext(dir("papers", "*.pdf"))
  thumbs <- file_path_sans_ext(dir("thumbs"))
  
  needed <- setdiff(papers, thumbs)
  if (length(needed) == 0) return()
  
  cmd <- str_c("convert papers/", needed, ".pdf[0] -antialias -thumbnail '100' ",
    " thumbs/", needed, ".png")
  lapply(cmd, system)
  
  invisible()
}

clean_bib <- function(entry) {
  entry$title <- str_replace_all(entry$title, "[{}]", "")
  if (!is.null(entry$note)) {
    entry$note <- str_replace_all(entry$note, "\\[|\\]", "")    
  }
  if (!is.null(entry$pages)) {
    entry$pages <- str_replace_all(entry$pages, "-|--", "&#8211;")    
  }

  entry
}
