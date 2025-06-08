from typing import Iterable
import numpy as np
import pandas as pd
from sklearn.preprocessing import normalize
from typing import Dict, List, Tuple
from itertools import chain
import requests
from statistics import mode

class Embeddings:
    """
    This class represents a container that holds a collection of words
    and their corresponding word embeddings.
    """

    def __init__(self, words: Iterable[str], vectors: np.ndarray):
        """
        Initializes an Embeddings object directly from a list of words
        and their embeddings.

        :param words: A list of words
        :param vectors: A 2D array of shape (len(words), embedding_size)
            where for each i, vectors[i] is the embedding for words[i]
        """
        self.words = list(words)
        self.indices = {w: i for i, w in enumerate(words)}
        self.vectors = vectors

    def __len__(self):
        return len(self.words)

    def __contains__(self, word: str) -> bool:
        return word in self.words

    def __getitem__(self, words: Iterable[str]) -> np.ndarray:
        """
        Retrieves embeddings for a list of words.

        :param words: A list of words
        :return: A 2D array of shape (len(words), embedding_size) where
            for each i, the ith row is the embedding for words[i]. When
            words are not findable, the vector is an array of NaN
        """
        vectors = []
        
        for a in words:
            if a in self.words:
                vectors.append(self.vectors[self.words.index(a)])
            else:
                empty = np.empty((1,300))
                empty[:] = np.NaN
                vectors.append(empty)
        
        return np.vstack(vectors)

    @classmethod
    def from_file(cls, year: int, self_train) -> "Embeddings":
        """
        Initializes an Embeddings object from a vocabulary.pkl file
        containing words and a w.npy file containing word embeddings 
        in SGNS format.

        :param filename: The name of the file containing the embeddings
        :return: An Embeddings object containing the loaded embeddings
        """
        word = pd.read_pickle(str(year) + "-vocab.pkl")
        embedding = np.load(str(year) + "-w.npy")
        embedding = normalize(embedding, axis=1, norm='l2')
        
        if self_train == False:
            idx = pd.Series(embedding.sum(axis=1)!=0).lt(1).idxmax()
            return cls(list(word[0:idx]),embedding[0:idx])
        else:
            return cls(list(word),embedding)

def load_dimension(filename: str):
    """
    Loads the words for each dimension in a dictionary format. Keys are
    dimension names. Values are specific words of each dimension.
    """

    output = {}
    with open(filename,'r') as input_file:
        for line in input_file:
            line = line.strip('\n')
            if ':' in line:
                category = line.split(' ')[1]
                output[category] = []
            else:
                output[category].append(tuple(line.split(' '))) 
    return output

def cosine_sim(x: np.ndarray, y: np.ndarray) -> np.ndarray:
    """
    Computes the cosine similarity between two matrices of row vectors.

    :param x: A 2D array of shape (m, embedding_size)
    :param y: A 2D array of shape (n, embedding_size)
    :return: An array of shape (m, n), where the entry in row i and
        column j is the cosine similarity between x[i] and y[j]
    """
    ## normalize
    x = x/(x**2).sum(axis=1, keepdims=True)**.5
    y = y/(y**2).sum(axis=1, keepdims=True)**.5
    return x.dot(y.T)


def occ_freq(job_title,year_start,year_end):
    """
    extract the frequency of each word
    """
    
    ## get url
    url =f"https://books.google.com/ngrams/json?content={job_title}&year_start={year_start}&year_end={year_end}&corpus=en-US-2009&smoothing=0&case_insensitive=false"
    resp = requests.get(url)
    time.sleep(1)
    
    ## extract count
    if resp.ok:
        results = json.loads(resp.content)
        if len(results)>0:
            return sum(results[0]['timeseries'])
        else:
            return np.nan
    else:
        return np.nan

def weight_average(array, weights):
    """
    return the weighted average of an array of embedding
    """
    ## create masked array
    masked_data = np.ma.masked_array(array, np.isnan(array))
    average = np.ma.average(masked_data, axis=0, weights=weights)
    result = average.filled(np.nan)
    return result

def centroid(embeddings, dimension, schema_words, weights=False):
    """
    return the centroid of the schema words
    """
    if weights == True:
        weights = [get_freq(i) for i in list(chain(*dimension[schema_words]))]
        weights = [i/np.nansum(weights) for i in weights]
        return weight_average(array = embeddings[list(chain(*dimension[schema_words]))],
                             weights = weights)
    else:
        return np.nanmean(embeddings[list(chain(*dimension[schema_words]))], axis=0)

def get_dimension(embeddings, dimension, category: str):
    """
    Retrieves the centroid of vectors of each category
    """
    
    ## construct the word list for each schema
    schema_words_dict = {
        "gender": ["female-words","male-words"],
        "prestige": ["prestige-words","common-words"],
        "education": ["education-words","uneducation-words"],
        "income": ["affluent-words","poor-words"],
        "moral": ["moral-words","immoral-words"],
        "evaluation": ["good-words","bad-words"],
        "potency": ["strong-words","weak-words"],
        "activity": ["active-words","passive-words"]
    }
    
    schema = centroid(embeddings, dimension, schema_words_dict[category][0]) - \
    centroid(embeddings, dimension, schema_words_dict[category][1])
    schema = schema.reshape((1, 300))
    return(schema)
    
def cosine_sim_average(dimension_vector,embeddings,occupation):
    """
    Retrieves the mean distance of single occupation words to each category
    """
    
    if occupation.str.split(", ",expand=True).shape[1] == 4:
        occupation_1 = occupation.str.split(", ",expand=True)[0]
        occupation_2 = occupation.str.split(", ",expand=True)[1]
        occupation_3 = occupation.str.split(", ",expand=True)[2]
        occupation_4 = occupation.str.split(", ",expand=True)[3]
        d = {'occupation_1': pd.Series(np.concatenate(cosine_sim(dimension_vector, embeddings[occupation_1]))), 
             'occupation_2': pd.Series(np.concatenate(cosine_sim(dimension_vector, embeddings[occupation_2]))), 
             'occupation_3': pd.Series(np.concatenate(cosine_sim(dimension_vector, embeddings[occupation_3]))),
             'occupation_4': pd.Series(np.concatenate(cosine_sim(dimension_vector, embeddings[occupation_4])))}
        df = pd.DataFrame(data=d)
        output = np.nanmean(df, axis=1)
        output[output==0] = np.nan
        return pd.Series(output)
    
    else:
        occupation_1 = occupation.str.split(", ",expand=True)[0]
        d = {'occupation_1': pd.Series(np.concatenate(cosine_sim(dimension_vector, embeddings[occupation_1])))}
        df = pd.DataFrame(data=d)
        output = np.nanmean(df, axis=1)
        output[output==0] = np.nan
        return pd.Series(output)
    

def cosine_sim_average_weight(dimension_vector,embeddings,occupation,
                             weight_1,weight_2,weight_3,weight_4):
    """
    Retrieves the mean distance of single occupation words to each category
    with weights proportional to their occurrence in Ngram or COCHA
    """
    
    if occupation.str.split(", ",expand=True).shape[1] == 4:
        occupation_1 = occupation.str.split(", ",expand=True)[0]
        occupation_2 = occupation.str.split(", ",expand=True)[1]
        occupation_3 = occupation.str.split(", ",expand=True)[2]
        occupation_4 = occupation.str.split(", ",expand=True)[3]
        d = {'occupation_1': pd.Series(np.concatenate(cosine_sim(dimension_vector, embeddings[occupation_1]))), 
             'occupation_2': pd.Series(np.concatenate(cosine_sim(dimension_vector, embeddings[occupation_2]))), 
             'occupation_3': pd.Series(np.concatenate(cosine_sim(dimension_vector, embeddings[occupation_3]))),
             'occupation_4': pd.Series(np.concatenate(cosine_sim(dimension_vector, embeddings[occupation_4])))}
        df = pd.DataFrame(data=d)
        
        ## append weight
        df['weight_1'] = weight_1
        df['weight_2'] = weight_2
        df['weight_3'] = weight_3
        df['weight_4'] = weight_4
        df.replace(mode(weight_4), np.nan, inplace=True)
        
        ## weighted mean
        df.iloc[:, 4:] = df.iloc[:, 4:].fillna(0)
        row_sums = df.iloc[:, 4:].sum(axis=1)
        df.iloc[:, 4:] = df.iloc[:, 4:].div(row_sums, axis=0)
        weighted_values = df.iloc[:, :4].values * df.iloc[:, 4:].values
        values_mask = ~np.isnan(df.iloc[:, :4].values)
        weights_mask = ~np.isnan(df.iloc[:, 4:].values)
        combined_mask = values_mask & weights_mask
        masked_weighted_values = np.where(combined_mask, weighted_values, 0)
        masked_weights = np.where(combined_mask, df.iloc[:, 4:].values, 0)
        sum_weighted_values = np.nansum(masked_weighted_values, axis=1)
        sum_weights = np.nansum(masked_weights, axis=1)
        output = np.divide(sum_weighted_values, sum_weights, where=sum_weights!=0)
        output[output==0] = np.nan
        return pd.Series(output)
