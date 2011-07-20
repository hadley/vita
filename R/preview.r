#' @importFrom sinartra static_file
#' @importFrom sinartra Router
#' @importFrom tools startDynamicHelp
preview <- function(show = FALSE) {
  router <- Router$clone()
  router$set_file_path("~/documents/vita/public/")
  router$get("*", function(splat, ...) {
    if (splat == "") return(router$redirect("/"))
    
    path <- file.path("~/documents/vita/public/", splat)
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
    home <- str_c("http://localhost:", port, "/")
    browseURL(home)        
  }
}