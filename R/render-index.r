render_index <- function(entries) {

  selected <- Filter(function(x) !is.null(x$selected), entries)
  year <- vapply(selected, function(x) as.numeric(x$year), numeric(1))
  selected <- selected[order(-year)]

  content <- str_c(
    "<h2>Selected publications</h2>\n",
    str_c(vapply(selected, render_entry, character(1)),
      collapse = "\n"))

  sidebar <- str_c(
    "<h3>All publications</h3>\n",
    "<ul>\n",
    str_c("  <li><a href='", paths, ".html'>", titles, "</a></li>\n", collapse = "\n"),
    "<ul>\n"
  )

  header <- "<h1>Academic portfolio</h1>\n"
  title <- "Academic portfolio"

  list(content = content, sidebar = sidebar, header = header, title = title)
}
