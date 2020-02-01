# -*- coding: utf-8 -*-
import pandas as pd
import nltk
from nltk import word_tokenize, sent_tokenize
from nltk.corpus import stopwords
from nltk.stem import LancasterStemmer, WordNetLemmatizer

def pre_process_text(this_text):
    stage1 = nltk.word_tokenize(this_text)
    stop_words = set(stopwords.words('english'))
    stemmer = LancasterStemmer()
    stem = stemmer.stem(word)
    new_word = unicodedata.normalize('NFKD', word).encode('ascii', 'ignore').decode('utf-8', 'ignore')
    

data_route = 'data/journals/'

file_list = open("data/journals/file_list.log","r")
list_to_load = file_list.read().split("\n")

dbase = pd.DataFrame(list_to_load,columns=['file_name'])
dbase['contents']=""

for each_file in range(0, len(dbase.file_name)):
    complete_name = data_route + dbase.file_name[each_file]
    this_file = open(complete_name,'r')
    dbase.iloc[each_file,1] = this_file.read()
    this_file.close()
    

