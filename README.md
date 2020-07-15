# legal_tc

## make corpora
If you look for new search terms, rerun the respective scripts from prefix 01-.* on for both `src/legal/` and `src/baseline/` or use the respective assembler scripts (BASH) in those folder.

bulk-data:
- SCOTUS (retrieved: 02.07.2020)

## reddit data
The reddit data has been collected using the pushshift API. The data spans over 08. July - 100d.

### Gather new reddit data selectively
If you already gathered data and just new new data, you can add new queries in `input/dict-add.txt` and (if needed) adjust `src/baseline/00x-reddit-API-redo.R`. Ultimately, open the terminal and execute `$src/baseline: R CMD BATCH 00x-reddit-API-redo.R`. Make sure that you have a stable internet connection. *This will take a while*. The script will be executed in R, so it is possible to open a parallel session in RStudio and work simultaneously. Once the script has ran through, you can find the R console output at `src/baseline/00x-reddit-API-redo.Rout` (you can open this with R).

If you wanna rerun the whole process, including generating a corpus, there is a convenience-script at `src/baseline/redo.sh`.

### Remove obsolete reddit data
For adjectives that do not yield enough hits, you can remove the gathered reddit data with `src/baseline/cleanup.sh`. To specify the undesired data, use the a regex with the adjectives (e.g. `'.*(Factual|Careless|Constitutional).*'`, *case sensitive and upper case needed*). With an executable script, you would use it as follows: `$user/src/baseline: ./cleanup.sh '.*(Factual|Careless|Constitutional).*'`.
