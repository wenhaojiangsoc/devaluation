from typing import Dict, List, Tuple
import itertools
import numpy as np
from embeddings import Embeddings


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

def get_closest_words(embeddings: Embeddings, vectors: np.ndarray,
                      k: int = 1) -> List[List[str]]:
    """

    Finds the top k words whose embeddings are closest to a given vector
    in terms of cosine similarity.

    :param embeddings: A set of word embeddings
    :param vectors: A 2D array of shape (m, embedding_size)
    :param k: The number of closest words to find for each vector
    :return: A list of m lists of words, where the ith list contains the
        k words that are closest to vectors[i] in the embedding space,
        not necessarily in order
    """
    full_vectors = embeddings.vectors
    top_index = (-cosine_sim(vectors,full_vectors)).argsort()[:,0:k]
    return np.array(embeddings.words)[top_index].tolist()


# This type alias represents the format that the testing data should be
# deserialized into. An analogy is a tuple of 4 strings, and an
# AnalogiesDataset is a dict that maps a relation type to the list of
# analogies under that relation type.
AnalogiesDataset = Dict[str, List[Tuple[str, str, str, str]]]


def load_analogies(filename: str) -> AnalogiesDataset:
    """

    Loads testing data for 3CosAdd from a .txt file and deserializes it
    into the AnalogiesData format.

    :param filename: The name of the file containing the testing data
    :return: An AnalogiesDataset containing the data in the file. The
        format of the data is described in the problem set and in the
        docstring for the AnalogiesDataset type alias
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
        
    
