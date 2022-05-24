#' Extract link texts and urls from a web page
#' @param url character an url
#' @return a data frame of link text and urls
#' @examples
#' \dontrun{
#' web_get_links("http://localhost/")
#' glinks <- get_webpage_links("http://google.com/")
#' }
#' @export
web_get_links <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}
