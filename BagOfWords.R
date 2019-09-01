# Version original de Julio Guadarrama, Abril 2019
simp_text <- function(texto) {
     # Separamos las palabras juntas
     limpio <- str_replace_all(texto, c("A" = " A",     "B" = " B",     "C" = " C",     "D" = " D",     "E" = " E",     "F" = " F",     "G" = " G",     "H" = " H",     "I" = " I",     "J" = " J",     "K" = " K",     "L" = " L",     "M" = " M",     "N" = " N",     "Ñ" = " Ñ",     "O" = " O",     "P" = " P",     "Q" = " Q",     "R" = " R",     "S" = " S",     "T" = " T",     "U" = " U",     "V" = " V",     "X" = " X",     "Y" = " Y",     "Z" = " Z"))
     # Convertimos el texto en corpus para poder procesarlo.
     corpus<-Corpus(VectorSource(limpio))
     # Mandamos todas las letras a minúsculas
     corpus<-tm_map(corpus, content_transformer(tolower))
     # Eliminamos los números
     corpus<-tm_map(corpus, content_transformer(removeNumbers))
     # Eliminamos la puntuación
     corpus<-tm_map(corpus, removePunctuation)
     # Eliminamos las stop words
     corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
     # Obtenemos las palabras raíz
     corpus_steam <- tm_map(corpus, stemDocument, language = "spanish")
     # Quitamos siglas coorporativas
     corpus_steam <- tm_map(corpus_steam, removeWords, c("sa", "sc","s", "c", "v", "r", "l", "rl", "cv", "cde", "de"))
     # Eliminamos espacios 
     corpus_steam <- tm_map(corpus_steam, stripWhitespace)
     corpus_steam <- tm_map(corpus_steam, trimws)
     # Regresamos a lista
     return(unlist(as.list(corpus_steam)))
}