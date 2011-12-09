
render_entry <- function(entry, link = TRUE) {
  if (link) {
    link_f <- function(x) {
      paper_path <- file.path("papers", str_c(entry$key, ".html"))
      if (!file.exists(paper_path)) return(x)
      
      str_c("<a href='/", paper_path, "'>", x, "</a>")
    }
  } else {
    link_f <- function(x) x
  }
  
  authors <- str_c(vapply(entry$author, format, character(1)), 
    collapse = ", ")
  
  thumb_path <- file.path("thumbs", str_c(entry$key, ".png"))
  if (file.exists(thumb_path)) {
    thumb <- link_f(str_c("  <img src='/", thumb_path, "' width='100' />\n"))
  } else {
    thumb <- NULL
  }
  

  paper_path <- file.path("papers", str_c(entry$key, ".pdf"))
  links <- c(
    if (file.exists(paper_path))
      str_c("<a href='/", paper_path, "'>pre-print</a>"),
    if (!is.null(entry$url)) 
      str_c("<a href='", entry$url, "'>from publisher</a>"),
    if (!is.null(entry$videourl)) 
      str_c("<a href='", entry$videourl, "'>watch online</a>"),
    if (!is.null(entry$doi)) 
      str_c("<a href='http://dx.doi.org/", entry$doi, "'>via doi</a>\n")
    )
  if (length(links) > 0) {
    download <- str_c("<p class='download'><span>Download</span>: \n", 
      str_c(links, collapse = " | "), "</p>\n")    
  } else {
    download <- NULL
  }

  citation <- cite(entry)
  
  str_c(
    "<div class='citation'>\n",
    thumb,
    "  <ul>\n",
    "    <li class='author'>", authors, ".</li>\n",
    "    <li class='title'>", link_f(entry$title), ".</li>\n",
    if (!is.null(citation)) 
    str_c("    <li class='citation'>", citation, "</li>\n"),
    if (!is.null(entry$note)) 
    str_c("    <li class='note'>[", entry$note, "]</li>\n"),
    "  </ul>\n",
    download,
    "  <br clear='all' />",
    "</div>\n"
  )
}

