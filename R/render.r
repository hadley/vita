render_page <- function(entries, type) {
  year <- vapply(entries, function(x) x$year, FUN.VALUE = character(1))
    
  by_year <- rev(split(entries, year))
  
  header <- str_c("<h1>", titles[type], "</h1>\n")
  
  content <- str_c(vapply(by_year, render_year, character(1)), 
    collapse = "\n")
    
  sidebar <- str_c("<nav><h3>By year</h2>\n<ul>\n", 
    str_c("  <li><a href='#", names(by_year), "'>", names(by_year),
      "</a></li>\n", collapse = "\n"), "</ul></nav>\n",
      
    "<h3>Other publications</h3>\n",
    "<ul>\n",
    str_c("  <li><a href='", paths, ".html'>", titles, "</a></li>\n", collapse = "\n"),
    "<ul>\n"
  )
      
  list(content = content, sidebar = sidebar, header = header)
}

render_year <- function(entries) {
  year <- entries[[1]]$year
  
  str_c("<h2 id='", year, "'>", year, "</h2>\n", 
    str_c(vapply(entries, render_entry, character(1)), collapse = "\n"))
}
