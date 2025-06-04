import numpy
import scipy
import cython
import gensim
from gensim import models, similarities
from gensim.models.word2vec import PathLineSentences
import smart_open
import logging
import os
import re
import sys  
import itertools
import math
import multiprocessing
from time import time

logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

class MySentences(object):
    def __init__(self, dirname, start_year, end_year, limit=None):
        self.dirname = dirname
        self.start_year = start_year
        self.end_year = end_year
        self.limit = limit
 
    def __iter__(self):
         ## iterate through each the compressed file directory
        for fname in os.listdir(self.dirname):
            ## for each file open it
            with gensim.utils.open_file(os.path.join(self.dirname, fname)) as fin:
                for line in itertools.islice(fin, self.limit):
                    line = gensim.utils.to_unicode(line,encoding='utf8', errors='ignore').split("\t")
                    if len(line)<3:
                        continue
                    ngram = line[0]
                    try:
                        year = int(line[1])
                    except ValueError:
                        continue
                    match_count = int(line[2])
                    if year < self.start_year or year > self.end_year:
                        continue
                    ## lower case the ngram, remove pos
                    processed_ngram = [word.split("_")[0] for word in ngram.lower().split()]
                    for x in range(match_count):
                        yield processed_ngram

                        ## read ngram in an iterable
assert gensim.models.word2vec.FAST_VERSION > -1
year_1=1900
year_2=1909
sentences = MySentences("/vast/wj2068/ngram_usa", year_1, year_2)                        

## build word2vec
w2v_model = gensim.models.word2vec.Word2Vec(min_count=100,
                     window=5,
                     vector_size=300,
                     sg=1,
                     hs=0,
                     negative=5,
                     workers=4,
                     batch_words=100000)

## build vocabularies
w2v_model.build_vocab(sentences,progress_per=100000000)

## save model
w2v_model.save('/vast/wj2068/models/10_year/build_vocab/w2vmodel_'+str(year_1)+'_'+str(year_2)+'_build_vocab_full')
