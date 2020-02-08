# Scrape Gutenberg project
library(curl)
library(rvest)
library(magrittr)

find_book_number <- function(url_string){
     found <- gregexpr("/", url_string)[[1]]
     position <- found[length(found)]
     book_number <- substr(url_string,position + 1, nchar(url_string))
     return(book_number)
}
genre <- read.csv("genre.txt", header = FALSE)[1,1]
root_directory <- paste0("http://www.gutenberg.org/wiki/", genre)
page <- read_html(root_directory)
all_links <- page %>% html_nodes("a") %>% html_attr("href")
relevant_links <- all_links[grepl("gutenberg.org/ebooks/", all_links)]
population_size <- length(relevant_links)
sample_size <- 60
if (population_size < sample_size) {
     sample_size <- population_size
}
sampled_links <- sample(relevant_links, sample_size)

for (this_book in sampled_links) {
     Sys.sleep(runif(1,min=5,max=35))
     this_url <- paste0("http:",this_book)
     page <- read_html(this_url)
     all_page_links <- page %>% html_nodes("a") %>% html_attr("href")
     useful_links <- all_page_links[grepl("txt.utf-8", all_page_links)]
     if(!identical(useful_links, character(0))){
          book_url <- paste0("http://gutenberg.org",useful_links[1])
          book_number <- find_book_number(this_book)
          file_name <- paste0(book_number,".txt")
          curl_download(book_url, file_name)
          print(file_name)
     }
     
}
# /html/body/div[1]/div[2]/div[2]/div/ul[2]/li[1]/a[1]

# /html/body/div[1]/div[2]/div[2]/div/ul[2]/li[4]/a[1]

# /html/body/div[1]/div[2]/div[2]/div/ul[6]/li[1]/a[1]
