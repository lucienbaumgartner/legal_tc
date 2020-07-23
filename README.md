README
================
Lucien Baumgartner
7/23/2020

  - [WIP: To Dos](#wip-to-dos)
  - [Data](#data)
  - [Hypotheses](#hypotheses)
      - [Global Context Effects](#global-context-effects)
      - [Within Context Effects](#within-context-effects)
      - [Differences of Within Context Effects Across
        Contexts](#differences-of-within-context-effects-across-contexts)
  - [Script Workflow](#script-workflow)
      - [Gather Data](#gather-data)
      - [Make Corpora](#make-corpora)
      - [Change Reddit Data ex post](#change-reddit-data-ex-post)
          - [Gather new reddit data
            selectively](#gather-new-reddit-data-selectively)
          - [Remove obsolete reddit data](#remove-obsolete-reddit-data)
  - [Text Data Treatment Overview](#text-data-treatment-overview)
  - [Setup Config](#setup-config)
      - [Software Versions](#software-versions)
      - [Full List of Dependencies](#full-list-of-dependencies)

## WIP: To Dos

  - [ ] migrate all Court Listener bulk data to external volume

## Data

The data is all bulk data that are processed into two corpora, a legal
one and a baseline (= reddit comments). The current baseline might be
substituted with another, more traditional corpus later in the process.

bulk-data:

  - reddit comments; time period: 01.01.2020-30.06.2020, max. 1000
    comments per search and day; SOURCE: [pushshift
    API](https://pushshift.io/api-parameters/) (retrieved 21.07.2020,
    open data)
  - [court
    opinions](https://legal-dictionary.thefreedictionary.com/Court+Opinion)
    from Court of Appeals for 1st to 11th circuit; SOURCE: [Court
    Listener](https://www.courtlistener.com/api/bulk-info/) (retrieved
    15.07.2020, open data)

The Court of Appeals are among the most influential courts in the US.
They hand over cases to the US Supreme Court and thus are a ‘legal
bottleneck’. We focus on a time period from 1980-2020 which encompasses
XXX raw documents.

![Court of
Appeals](res/img/2560px-US_Court_of_Appeals_and_District_Court_map.svg.png)

## Hypotheses

### Global Context Effects

  - H1: The effect of context on the *absolute* sentiment value of
    conjoined adjectives will be significantly smaller for the legal
    context compared to the baseline
      - H1a: The interaction between context and positive polarity of
        the target adjective will be positive, and the effect size will
        be significantly lower for the legal context compared to the
        baseline.
    
      - H1b: The interaction between context and negative polarit for
        the target adjective will be negative, and the effect size will
        be significantly higher for the legal context compared to the
        baseline.

### Within Context Effects

  - H2a: The absolute average sentiment of the conjoined adjectives are
    significantly different for the categories of target adjectives
    (epistemic, legal, moral) *within the legal context*. We expect the
    following hierarchy:
    `\overline{|y|}*\beta_{epistemic}<\overline{|y|}*\beta_{legal}<\overline{|y|}*\beta_{moral}`.

  - H2b: We expect the same for the baseline.

### Differences of Within Context Effects Across Contexts

<img src="https://render.githubusercontent.com/render/math?math=e^{i%20\pi}%20=%20-\overline{1}">

![formula](https://render.githubusercontent.com/render/math?math=e\\overline%7By%7D)
- H5: Based on H1 and H2a & H2b we expect the following effect relations
to be significant: - H5a: `\overline{|y|}*I(\beta_{epistemic},
\beta{legal}<\overline{|y|}*I(\beta_{epistemic}, \beta{baseline}` - H5b:
`\overline{|y|}*I(\beta_{legal},
\beta{context:legal}<\overline{|y|}*I(\beta_{legal},
\beta{context:baseline}` - H5c:
![formula](https://render.githubusercontent.com/render/math?math=\\overline%7B%7Cy%7C%7D*\(\\beta_%7Bmoral%7D*\\beta%7Bcontext:legal%7D\)%3C\\overline%7B%7Cy%7C%7D*\(\\beta_%7Bmoral%7D*\\beta%7Bcontext:baseline%7D\))

## Script Workflow

### Gather Data

To gather the data, run `src/baseline/00-reddit-API.R` for the baseline
and `src/legal/00-download-bulk-data.R` followed by
`src/legal/00-json-to-rds.R` for the legal data.

### Make Corpora

If you look for new search terms, rerun the respective scripts from
prefix `01-.*` on for both `src/legal/` and `src/baseline/` or use the
respective assembler scripts (BASH) in those folders
(`src/legal/legal_batch_assembler.sh` and
`src/baseline/batch_assembler.sh`).

This includes: - `src/baseline/..`: - `01-lookup-to-reduce.R` -
`02-lookup-to-finalize.R` - `src/legal/..`: - `01-lookup-to-reduce.R` -
`02-lookup-to-finalize.R`; updated to: `02-lookup-to-finalize-ALT.R`

### Change Reddit Data ex post

#### Gather new reddit data selectively

If you already gathered data and just new new data, you can add new
queries in `input/dict-add.txt` and (if needed) adjust
`src/baseline/00x-reddit-API-redo.R`. Ultimately, open the terminal and
execute `$src/baseline: R CMD BATCH 00x-reddit-API-redo.R`. Make sure
that you have a stable internet connection. *This will take a while*.
The script will be executed in R, so it is possible to open a parallel
session in RStudio and work simultaneously. Once the script has ran
through, you can find the R console output at
`src/baseline/00x-reddit-API-redo.Rout` (you can open this with R).

If you wanna rerun the whole process, including generating a corpus,
there is a convenience-script at `src/baseline/redo.sh`.

#### Remove obsolete reddit data

For adjectives that do not yield enough hits, you can remove the
gathered reddit data with `src/baseline/cleanup.sh`. To specify the
undesired data, use the a regex with the adjectives
(e.g. `'.*(Factual|Careless|Constitutional).*'`, *case sensitive and
upper case needed*). With an executable script, you would use it as
follows: `$user/src/baseline: ./cleanup.sh
'.*(Factual|Careless|Constitutional).*'`.

## Text Data Treatment Overview

  - sentence-tokenization using
    [tokenizer](https://cran.r-project.org/web/packages/tokenizers/)
  - POS-tagging with [spaCy](https://spacy.io/) using and
    [Anaconda3](https://www.anaconda.com/products/individual)
    environment in [R](https://www.r-project.org/)
  - regex-based text extraction (using
    [stringi](https://cran.r-project.org/web/packages/stringi/index.html)
    and [stringr](https://cran.r-project.org/web/packages/stringr/))

## Setup Config

### Software Versions

  - R version 4.0.2 (2020-06-22)
  - Python 3.7.6
  - conda 4.8.2

### Full List of Dependencies

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
