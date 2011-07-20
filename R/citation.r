cite <- function(entry) {
  f <- cite_f[[entry$bibtype]]
  if (is.null(f)) return(NULL)
  
  f(entry)
}

cite_f <- list(
  Article = function(entry) {
    str_c(
      "<em>", entry$journal, "</em>,", 
      if (!is.null(entry$volume)) 
        str_c("vol. ", entry$volume, ", "), 
      if (!is.null(entry$number)) 
        str_c("no. ", entry$number, ", "), 
      if (!is.null(entry$pages)) 
        str_c("pp. ", entry$pages, ", "), 
      entry$year, ".")    
  }
)
