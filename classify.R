library(tidytext) # Eng
library(tm) # Sp
library(dplyr)
library(readr)
library(tokenizers)
library(udpipe)
data("stop_words")

# Set global variables
work_directory <<- "C:/Users/Tezzz/Documents/Proyectos/DS_Literature/data/top100"
dictionaries <<- "C:/Users/Tezzz/Documents/Proyectos/DS_Literature/data/dictionaries/"
character_freq_threshold <<- 10
keyword_freq_threshold <<- 10

# Set global constants
vowels <<- c("a","e","i","o","u","y")
consonants <<- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z")
all_vowels <<- paste0(vowels,collapse = "")
all_consonants <<- paste0(consonants, collapse = "")

# Declare all function tools
mark_all_CR <- function (this_text) {
     this_text[this_text==""] <- "\n"
     return(this_text)
}

find_pattern_in_word <- function(this_word, this_pattern) {
     coincidences<-gregexpr(this_pattern,this_word)
     number_found<-length(coincidences[[1]])
     if (coincidences[[1]][1]==-1) {
          number_found<-0
     }
     return(number_found)
}

count_syllables_in_array <- function(this_word_array) {
     syllable_count<-c()
     for (each_word in this_word_array) {
          # Use regular expressions to find vowells separated by other characters
          tpattern <- paste0("[",all_vowels,"][",all_consonants,"]|[",all_vowels,"]$",collapse = "")
          number_of_syllables <- find_pattern_in_word(each_word,tpattern)
          syllable_count<-c(syllable_count,number_of_syllables)
     }
     return(syllable_count)
}

remove_genitives <- function(this_word_array) {
     corrected <- gsub("[^[:alnum:]]s$", "", this_word_array)
     return(corrected)
}

find_capitalised_words <- function(this_word_array) {
     found_capitalised <- grepl("^[A-Z]", this_word_array)
     output <- this_word_array[found_capitalised]
     return(output)
}

please_remove <- function(these_words, from) {
     found_coincidences <- from %in% these_words
     output <- from[!found_coincidences]
     return(output)
}

select_main <- function(this_word_array, this_threshold) {
     ordered_words <- sort(table(this_word_array), decreasing = TRUE)
     most_important <- ordered_words[ordered_words>=this_threshold]
     return(most_important)
}

annotate_text <- function(this_text){
     if (!exists("udmodel_english")) {
          dictionary_path <- paste0(dictionaries, 'english-ewt-ud-2.3-181115.udpipe')
          udmodel_english <<- udpipe_load_model(file = dictionary_path)
     }
     output <- data.frame(udpipe_annotate(udmodel_english, this_text))
     return(output)
}

find_this_particle <- function(this_df, this_particle, boolean_output = FALSE){
     boolean_findings <- this_df$upos==this_particle
     if(!boolean_output){
          output <- boolean_findings * 1
     } else {
          output <- boolean_findings
     }
     return(output)
}

find_this_tense <- function(this_df, this_tense, boolean_output = FALSE){
     verb_locations <- find_this_particle(this_df, "VERB", boolean_output = TRUE)
     sought_particle <- paste0("Tense=",this_tense)
     tense_locations <- grepl(sought_particle, this_df$feats)
     verb_and_tense <- verb_locations & tense_locations
     if(!boolean_output){
          output <- verb_and_tense * 1
     } else {
          output <- verb_and_tense
     }
     return(output)
}

find_english_future_tense <- function(this_df, boolean_output = FALSE){
     where_auxiliary <- text_notes$upos=="AUX" 
     where_contains_ll <- grepl("ll|LL", text_notes$token)
     where_both <- where_auxiliary & where_contains_ll
     if(boolean_output){
          output <- where_both
     } else {
          output <- where_both * 1
     }
     return(output)
}

# Open first file
setwd(work_directory)
first_file_name <- dir()[1]
file_connection <- file(first_file_name, "r")
file_encoding <- guess_encoding(first_file_name)[1,1][[1]]
file_contents <- readLines(con = file_connection, encoding = file_encoding)
close(file_connection)
ttext <- mark_all_CR(file_contents) # correct source files
complete_text <- paste0(ttext, collapse = " |°| ") # Mark line-breaks

########################### First, reduce text to digital fingerprint #####################


## ------------------------- Get number of unique words (or stems?) ----------------------
all_tokens <- tokenize_ptb(complete_text,lowercase = TRUE)[[1]]
each_unique_token <- unique(all_tokens)
riqueza_lexica <- length(each_unique_token) # (tokenizer does not stem -alice's, alice-)

## ------------------------ Get a map of pauses and syllables (rythm) --------------------
# First, where are stops/silences.
stop_characters <- c(",", ";", ".")
where_stop_chars <- which(all_tokens %in% stop_characters)

syllables_per_word <- count_syllables_in_array(all_tokens)
average_syllables_per_sentence <- sum(syllables_per_word)/length(where_stop_chars)
     # Should be able to do the same on a paragraph and page level.

## --------------------- Get all possible characters and their positions -----------------
all_case_sens_tokens <- tokenize_ptb(complete_text,lowercase = FALSE)[[1]]
# Which start with Uppercase?
name_candidates <- remove_genitives(find_capitalised_words(all_case_sens_tokens))
name_candidates <- tolower(name_candidates)
name_candidates <- please_remove(these_words = stop_words$word, from = name_candidates)
most_important_characters <- select_main(name_candidates, character_freq_threshold)

# Get most important keywords and their locations
all_stems <- tokenize_word_stems(complete_text)[[1]]
all_stems <- please_remove(these_words = stop_words$word, from = all_stems)
keywords <- select_main(all_stems, keyword_freq_threshold)
     # This clearly does not work! Need Better method!
     # Perhaps try with n-grams (2 or 3 grams)

# identify adjectives and adverbs
complete_text_2 <- paste0(ttext, collapse = " ")
ttext_2 <- tokenize_sentences(complete_text_2)[[1]]
text_notes <- annotate_text(ttext_2)
# Create parallel array with one observation per word: 1-verbs; 0-non-adverbs
#                                                      1-adj; 0-non-adjectives
adverb_location <- find_this_particle(text_notes, "ADV")
adjective_location <- find_this_particle(text_notes, "ADJ")

# identify verbal times per sentence
past_locations <- find_this_tense(text_notes, "Past")
present_locations <- find_this_tense(text_notes, "Pres")
future_locations <- find_english_future_tense(text_notes)
participle_locations <- find_this_tense(text_notes, "Past|VerbForm=Part")
# Are specific tenses in the proximity of specific characters?

# Graph verbal time frequencies
plot(future_locations * 1.15, col = "red")
points(present_locations, col = "black")
points(past_locations * .8, col = "blue")
# Find a better way to visualize

# Graph adjective usage per paragraph, per page.
plot(adverb_location, col = "blue")
points(adjective_location * .75, col = "green")
# Find a better way to visualize

