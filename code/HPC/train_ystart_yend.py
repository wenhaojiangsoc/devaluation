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
import glob
import pickle
import numpy as np

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

## loop over epochs and cumulative model save
for i in range(1,6):
    
    ## check if any epoch results exist
    if any("w2vmodel_ng5_" in epoch for epoch in glob.glob("/vast/wj2068/models/10_year/model_"+str(year_1)+"_"+str(year_2)+ "/epoch_"+str(i)+"/*")):
        print("epoch " + str(i) + " already exists!")
        continue
    
    else:
        ## train new model
        w2v_model = gensim.models.word2vec.Word2Vec.load(glob.glob("/vast/wj2068/models/10_year/model_"+str(year_1)+"_"+str(year_2)+ "/epoch_"+str(i-1)+"/*")[0])
        
        w2v_model.train(sentences,
                    total_examples=w2v_model.corpus_count,
                    epochs=1)
                    
        folder = "/vast/wj2068/models/10_year/model_"+str(year_1)+"_"+str(year_2)+ "/epoch_"+str(i)
        
        ## save intermediate model
        if i != 5:
            w2v_model.save(folder + "/w2vmodel_ng5_"+str(year_1)+"_"+str(year_2)+"_epoch_"+str(i)+"_full")
        
        else:
            ## save final model
            w2v_model.save(folder + "/w2vmodel_ng5_"+str(year_1)+"_"+str(year_2)+"_epoch_"+str(i)+"_full")

            ## save vocabulary
            vocab_list = w2v_model.wv.index_to_key
            for i in range(0,len(vocab_list)):
                if vocab_list[i] == '':
                    vocab_list[i] = "thisisanemptytoken"+str(i)
            with open(folder + "/" + str(year_1) + "-vocab.pkl", "wb") as f:
                pickle.dump(vocab_list, f)
                
            ## save vector
            with open(folder + "/" + str(year_1) + "-w.npy", "wb") as f:
                np.save(f, w2v_model.wv.vectors)
