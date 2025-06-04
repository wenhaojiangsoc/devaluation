# The Cultural Devaluation of Feminization Work

Repository for replication codes and data for the paper *The Cultural Devaluation of Feminized Work* published in *American Sociological Review*.

## Overview about the Repository

This repository hosts intermediate data and the codes that I used to generate the main results of the paper. Because of the copyright restriction, I do not include the raw Google Ngram, the Corpus of Historical American English (COHA) or the Corpus of Contemporary American English (COCA).

The original Google Ngram data (American English), Version 2, is publicly available via:\
[Google Ngram and Books, Version 2](http://storage.googleapis.com/books/ngrams/books/datasetsv2.html). A newer Version 3 is available via [Version 3](http://storage.googleapis.com/books/ngrams/books/datasetsv3.html).

COHA and COCA can be purchased from:\
[Corpus of Historical American English](https://www.english-corpora.org/coha/) \
[Corpus of Contemporary American English](https://www.english-corpora.org/coca/)

### Data

There are three kinds of data included to reproduce the main results.

* `embedding vectors`: Embedding vectors I trained using word2vec (*gensim 4.3.0*) by decade for Google Ngram and COHA and COCA (combined as COCHA). Due to the file size limit, the embeddings are uploaded to the dropbox folder [COCHA](https://www.dropbox.com/scl/fi/53zl0zwrnowpb4zqa9em8/COCHA.zip?rlkey=snsc07jo3bjfmk0u9mauvkd04&st=zffvsmn1&dl=0) and [Ngram](https://www.dropbox.com/scl/fi/wtrhnrywb5xpj0zepzvn1/Ngram.zip?rlkey=5t21hczhojakse4zgkzn511yh&st=qd29mwc5&dl=0).
* `dimension and mapping`: The dimension words I used to create gender, symbolic value, and other cultural dimensions and subspace. I also include the mapping crosswalk that converts each 1950 COC-based occupation title into single-word occupations.
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

    * `read_ystart_yend.py` reads vocabularies in Google Ngram. This step separates the reading and training process in *gensim*, which may be necessary if you have a limited time window of training. The output should be saved in a folder named `epoch_0`, meaning untrained read-only embedding. `ystart` and `yend` are the starting and ending year of the publications to be included in training.
    * `train_ystart_yend.py` trains the *word2vec* model for 5 times (my default). The initial embedding will be read from the output in `epoch_0`, and the following outputs from the training process will be saved sequentially to `epoch_1` till `epoch_5`. Only the output in `epoch_5` will be used.

