# The Cultural Devaluation of Feminization Work

Repository for replication codes and data for the paper *The Cultural Devaluation of Feminized Work* published in *American Sociological Review*.

## Overview about the Repository

This repository hosts intermediate data and the codes that I used to generate the results of the paper. Due to copyright restriction, I do not include the raw Google Ngram, the Corpus of Historical American English (COHA) or the Corpus of Contemporary American English (COCA).

The original Google Ngram data (American English), Version 2, is publicly available via:\
[Google Ngram and Books, Version 2](http://storage.googleapis.com/books/ngrams/books/datasetsv2.html). A newer [Version 3](http://storage.googleapis.com/books/ngrams/books/datasetsv3.html) is now also available.

COHA and COCA can be purchased from:\
[Corpus of Historical American English](https://www.english-corpora.org/coha/) \
[Corpus of Contemporary American English](https://www.english-corpora.org/coca/)

### Data

There are four kinds of data included to reproduce the main results.

* `embedding vectors`: Embedding vectors I trained using *word2vec* (*gensim 4.3.0*) by decade for Google Ngram and COHA and COCA (combined as COCHA). Due to the file size limit, the decade-specific embeddings are uploaded to the dropbox folder [COCHA](https://www.dropbox.com/scl/fi/53zl0zwrnowpb4zqa9em8/COCHA.zip?rlkey=snsc07jo3bjfmk0u9mauvkd04&st=zffvsmn1&dl=0) and [Ngram](https://www.dropbox.com/scl/fi/wtrhnrywb5xpj0zepzvn1/Ngram.zip?rlkey=5t21hczhojakse4zgkzn511yh&st=qd29mwc5&dl=0).
* `dimension and mapping`: The dimension words I used to create gender, symbolic value, and other cultural dimensions and subspace, with reference to previous canonical works such as [Garg et al. 2018](https://www.pnas.org/doi/10.1073/pnas.1720347115). I also include the mapping crosswalk that converts each 1950 COC-based occupation title into single-word occupations.
* `census and merged`: Cleaned data with variables from IPUMS (Decennial Census) that were used in the study. I also include the final merged dataset with text-based measures (Ngram and COCHA) and census-based statistics for main analysis. You can directly use the merged file with name `corpus.csv` to generate results.
* `DoT skills`: the additional skill measures based on the newly transcribed Dictionary of Titles from the work of [Althobaiti et al. 2022](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DQW8IP). I did additional cleanings for the 1965, 1977, and 1991 files to harmonize occupation schemes to 1960 Census occupation codes. The final conversion from 1960 to 1950 Census occupation code can be downloaded from IPUMS. If you want the full codes for this process of skill construction and occupation harmonization, please reach out to me and I can share data and codes.

### Code

I include three sets of codes I used to generate the main results as well as all outputs that appear in online supplementary materials.

* `measures` folder:

    * `embeddings.py` provides functions to read word embeddings, construct semantic subspace, and generate occupation-specific measures of gender typing, prestige, and other cultural properties. It also includes the implementation of word count-based weighted measure of occupation cultural meanings.
    * `distance.ipynb` was used to generate occupation-specific measures of cultural meanings. It also supplements the codes for t-SNE visualization that appears in online supplementary materials.
    * `3CosAdd.py` was used to search for the closest word to solves the word analogy problem. Results appear in online supplementary materials.
    
* `analysis` folder:

    * `occ1950.R` generates the cleaned Census-based measures of e.g., percent female and other covariates, based on the raw data downloaded from IPUMS. For a list of samples and variables, please see `occ1950.xml` in `census and merged` in the `data` folder.
    * `main.R` offers results and figures that appear in the main text.
    * `appendix.R` provides results that appear in the robustness check section and in online supplementary materials.
    * `function.R` provides the key steps to implement the decomposition as introduced by [Ishimaru (2022)](
    https://doi.org/10.48550/arXiv.2103.12374), including both scenarios when controls are and are not present. This can be used to conduct any other TWFE-based analysis with a continuous treatment, when the treatment effect is temporally heterogeneous.
    
* `HPC` folder:

    * `read.py` reads vocabularies in Google Ngram. This step separates the reading and training process in *word2vec*, which can be necessary if you have a limited time window of training. The output should be saved in a folder named `epoch_0`, indicating untrained read-only embedding. `ystart` and `yend` are the starting and ending year of the publications to be included in training.
    * `train.py` trains the *word2vec* model for 5 times (my default). The initial embedding will be read from the output in `epoch_0`, and the following outputs from the training process will be saved sequentially to `epoch_1` till `epoch_5`. The output in `epoch_5` will be used as the final embedding.
    * Depending on your machine's computational capacity, High Performance Computing (HPC) platform may be used for the training process. In this case, you will need to customize batch files to submit your modified codes. 
    * As of the time when I trained the Ngram data, only 1 CPU was used throughout the process. This is due to the I/O bottleneck problem causing most other threads to be idle. You are welcome to explore potential solutions to facilitate the training speed. 

