render_entry_page <- function(entry) {
  out <- render_entry(entry, link = FALSE)
  
  
  if (!is.null(entry$abstract)) {
    abstract <- str_replace(entry$abstract, "\n\n", "</p>\n<p>")
    out <- str_c(out, "<p class='abstract'>", abstract , "</p>\n\n")
  }
  
  entry$abstract <- NULL
  out <- str_c(out, 
    "<pre class='bibtex'>",
    str_c(toBibtex(entry), collapse = "\n"), 
    "</pre>\n")
  
  header <- str_c("<h1>", entry$title, "</h1>\n")
  
  list(content = out, header = header)
}