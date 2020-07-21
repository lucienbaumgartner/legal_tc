# Legal Thick Concepts

## program versions
- R version 4.0.2 (2020-06-22)
- Python 3.7.6
- conda 4.8.2

## text data treatment overview
- sentence-tokenization using [tokenizer](https://cran.r-project.org/web/packages/tokenizers/)
- POS-tagging with [spaCy](https://spacy.io/) using and [Anaconda3](https://www.anaconda.com/products/individual) environment in [R](https://www.r-project.org/)
- regex-based text extraction (using [stringi](https://cran.r-project.org/web/packages/stringi/index.html) and [stringr](https://cran.r-project.org/web/packages/stringr/))

# README

## Data
The data is all bulk data that are processed into two corpora, a legal one and a baseline (= reddit comments). The current baseline might be substituted with another, more traditional corpus later in the process.

**TO DO: migrate all the data to external volume**

bulk-data:
- reddit comments; time period: 01.01.2020-30.06.2020, max. 1000 comments per search and day; SOURCE: [pushshift API](https://pushshift.io/api-parameters/) (retrieved 21.07.2020, open data)
- ~~SCOTUS (retrieved: 02.07.2020)~~
- [court opinions](https://legal-dictionary.thefreedictionary.com/Court+Opinion) from Court of Appeals for 1st to 11th circuit; SOURCE: [Court Listener](https://www.courtlistener.com/api/bulk-info/) (retrieved 15.07.2020, open data)

Those are among the most influential courts in the US. They hand over cases to the US Supreme Court.

![Court of Appeals](/res/img/2560px-US_Court_of_Appeals_and_District_Court_map.svg.png)

## Gather Data
To gather the data, run `src/baseline/00-reddit-API.R` for the baseline and `src/legal/00-download-bulk-data.R` followed by `src/legal/00-json-to-rds.R` for the legal data.

## Make Corpora
If you look for new search terms, rerun the respective scripts from prefix `01-.*` on for both `src/legal/` and `src/baseline/` or use the respective assembler scripts (BASH) in those folders (`src/legal/legal_batch_assembler.sh` and `src/baseline/batch_assembler.sh`).

This includes:
- `src/baseline/..`:
    - `01-lookup-to-reduce.R`
    - `02-lookup-to-finalize.R`
- `src/legal/..`:
    - `01-lookup-to-reduce.R`
    - `~~02-lookup-to-finalize.R~`; updated to: `02-lookup-to-finalize-ALT.R`

## Change Reddit Data ex post
### Gather new reddit data selectively
If you already gathered data and just new new data, you can add new queries in `input/dict-add.txt` and (if needed) adjust `src/baseline/00x-reddit-API-redo.R`. Ultimately, open the terminal and execute `$src/baseline: R CMD BATCH 00x-reddit-API-redo.R`. Make sure that you have a stable internet connection. *This will take a while*. The script will be executed in R, so it is possible to open a parallel session in RStudio and work simultaneously. Once the script has ran through, you can find the R console output at `src/baseline/00x-reddit-API-redo.Rout` (you can open this with R).

If you wanna rerun the whole process, including generating a corpus, there is a convenience-script at `src/baseline/redo.sh`.

### Remove obsolete reddit data
For adjectives that do not yield enough hits, you can remove the gathered reddit data with `src/baseline/cleanup.sh`. To specify the undesired data, use the a regex with the adjectives (e.g. `'.*(Factual|Careless|Constitutional).*'`, *case sensitive and upper case needed*). With an executable script, you would use it as follows: `$user/src/baseline: ./cleanup.sh '.*(Factual|Careless|Constitutional).*'`.

## Full List of Dependencies
- dplyr
- spacyr
- tokenizers
- quanteda
- ggplot2
- gtools
- pbapply
- pbmcapply
- stringi
- stringr
- jsonlite
- rvest
- tm
- reticulate
- scales
- car
- ggrepel
