# The Cultural Devaluation of Feminization Work

Repository for replication codes and data for the paper *The Cultural Devaluation of Feminized Work*, forthcoming in *American Sociological Review*.

## Overview about the Repository

This repository hosts intermediate data and the codes that I used to generate the main results of the paper. Because of the copyright restriction, I do not include the raw Google Ngram, the Corpus of Historical American English (COHA) or the Corpus of Contemporary American English (COCA).

The original Google Ngram data (American English), Version 2, however, are publicly available via:\
[Google Ngram and Books, Version 2](http://storage.googleapis.com/books/ngrams/books/datasetsv2.html)

COHA and COCA can be purchased from:\
[Corpus of Historical American English](https://www.english-corpora.org/coha/) \
[Corpus of Contemporary American English](https://www.english-corpora.org/coca/)

### Data

There are three kinds of data included to reproduce the main results.

* `embedding vectors`: Embedding vectors I trained using word2vec (*gensim 4.3.0*) by decade for Google Ngram and COHA and COCA (combined as COCHA). Due to the file size limit, the embeddings are uploaded to the project folder in [Open Science Framework](https://osf.io/xyqah/files/osfstorage).
* `dimension and mapping`: The dimension words I used to create gender, prestige, and other cultural dimensions and subspace. I also include the mapping crosswalk that converts each 1950 COC-based occupation title into single-word occupations.
* `census and merged`: Cleaned data with variables from IPUMS (Decennial Census) that were used in the study. I also include the final merged dataset with text-based measures and census-based statistics for main analysis. You can directly use the merged file with name `corpus.csv` to generate results.

### Code

I include three sets of codes I used to generate the main results.

* `measures` folder:

    * `distance.ipynb` and `embeddings.py` were used to construct semantic subspace and generate occupation-specific measures of gender typing, prestige, and other cultural properties. 
    * `3CosAdd.py` can be used to search for the closest word to solves the word analogy problem.
    
* `analysis` folder:

    * `occ1950.R` generates the cleaned Census-based measures of e.g., percent female and other covariates.
    * `main.R` offers results and figures that appear in the main text.
    * `robustness.R` provides results that appear in the robustness check section and in Appendix.
    * `function.R` provides the key steps to implement the decomposition as introduced by [Ishimaru (2022)](
    https://doi.org/10.48550/arXiv.2103.12374), including both scenarios when controls are and are not present. This can be used to conduct any other TWFE-based analysis with a continuous treatment, when the treatment effect is temporally heterogeneous.
    
* `HPC` folder:

    * `python` folder includes codes that read vocabularies in Google Ngram and train the word2vec model.
    * `batch` folder gives all batch files I used in HPC application. You will need to customize it for your own needs.
    * If you want to create your own embeddings using these codes, you will need to create a separate folder called `models` to store intermediate checkpoints. Some HPC machines may impose a 7-day limit in running jobs, and saving checkpoints for heavy jobs is necessary as in my case.

