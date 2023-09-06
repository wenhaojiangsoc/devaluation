# The Cultural Devaluation of Feminization Work

Repository for replication codes and data for the paper *The Cultural Devaluation of Feminized Work*, currently under peer review.

## Overview about the Repository

This repository hosts intermediate data and the codes that I used to generate the main results of the paper. Because of the copyright restriction, I do not include the raw Google Ngram, the Corpus of Historical American English (COHA) or the Corpus of Contemporary American English (COCA).

The original Google Ngram data (American English), Version 2, however, are publicly available via:\
[Google Ngram and Books, Version 2](http://storage.googleapis.com/books/ngrams/books/datasetsv2.html)

COHA and COCA can be purchased from:\
[Corpus of Historical American English](https://www.english-corpora.org/coha/) \
[Corpus of Contemporary American English](https://www.english-corpora.org/coca/)

### Data

There are three kinds of data included to reproduce most of the results in the paper.

* `embedding vectors`: All embedding vectors I trained using word2vec (*gensim 4.3.0*) by decade for the three corpora.
* `dimension and mapping`: The dimension words I used to create gender, prestige, and other cultural dimensions and subspace. I also include the mapping crosswalk that converts each 1950 COC-based occupation title into single-word occupations.
* `census and merged`: Cleaned data with variables from IPUMS (Decennial Census) that were used in the study. I also include the final merged dataset with text-based measures and census-based statistics used in the final main analysis.


