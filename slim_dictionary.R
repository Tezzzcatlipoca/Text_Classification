# Slim this dictionary
library(tidytext) # Eng
library(tm) # Sp
library(dplyr)
library(readr)
library(tokenizers)

# Set global variables
work_directory <<- "C:/Users/Tezzz/Documents/Proyectos/DS_Literature/data/dictionaries"
setwd(work_directory)
first_file_name <- "eng1.txt"
file_connection <- file(first_file_name, "r")
file_encoding <- guess_encoding(first_file_name)[1,1][[1]]
file_contents <- readLines(con = file_connection, encoding = file_encoding)
close(file_connection)

new_dictionary <- data.frame()
also_copy_next <- FALSE
for (each_line in file_contents) {
     if (also_copy_next) {
          new_line <- data.frame(Term=each_line)
          new_dictionary <- rbind(new_dictionary, new_line)
          also_copy_next <- FALSE
     }
     if (!grepl("[a-z]", each_line)) {
          new_line <- data.frame(Term=each_line)
          new_dictionary <- rbind(new_dictionary, new_line)
          also_copy_next <- TRUE
     }
}

