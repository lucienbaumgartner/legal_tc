txtparsed_adj <- pbmclapply(1:length(txtparsed), function(INDEX){
  # z = sentence
  z <- txtparsed[[INDEX]]
  # the lookup has two elements:
  # .lookup[1] == TARGET; .lookup[2] == CCONJ
  .lookup <- c(df$TARGET[INDEX], df$CCONJ[INDEX])
  # lookup the ADJ
  windex <- grep(.lookup[1], z)
  # if the word afterwards is the corresponding AND/BUT OR
  # comma followed by the corresponding AND/BUT
  windex <- windex[!is.na(z[windex + 2])]
  if(!any(is.na(windex)|is.null(windex))){
    # if <TARGET is followed by CCONJ> OR <TARGET is followed by comma and then CCONJ>
    # REV:: add: OR <TARGET is followed by comma, ADV and then CCONJ>
    # REV:: ( z[windex + 1] == ',' & ( (z[windex + 2] == .lookup[2]) | (names(z[windex + 2]) == 'ADV' & z[windex + 3] == .lookup[2]) ) ) 
    if(any( z[windex + 1] == .lookup[2] | (z[windex + 1] == ',' & z[windex + 2] == .lookup[2]) )){
      # loop over indexes (all instances of TARGET in z)
      # pop = z, y = windex[i] (i is not a thing tho)
      tmp <- lapply(windex, function(y, pop=z){
        # we check if there is a comma
        # if there is one, the incremental parameter p is 1
        # otherwise it is none
        # p gets added to windex (y)
        p <- ifelse(pop[y + 1] == ',' & pop[y + 2] == .lookup[2], 1, 0)
        # if TARGET + 2 + p (0|1) is NOT longer than the length of sentence
        # REV:: replace < with <=
        # Q: why is it +2? Are we disciminating for MAXIMUM LENGTH here?
        # A: if so, we have to add an if-statement that is less disciminatory
        if(y + 2 + p < length(pop)){
          # if <TARGET + 1 + p == CCONJ> (so: check(<TARGET + ,? + CCONJ>)) AND
          # [[<TARGET + ,? + CCONJ + ADJ>[[... (names(pop[y + 2 + p]) == 'ADJ') OR
          # <TARGET + ,? + CCONJ + ADV + ADJ>]]
          # The following ifelse actually is a good way to operationalize the probem of outrunning indexes
          # ifelse('try-error'%in%class(try(names(pop[y + 3 + p]) == 'ADJ')), F, names(pop[y + 3 + p]) == 'ADJ')
          # we should write a function for this
          # REV:: function(look.position = 3, start.index = y, entity = pop, look.for = 'ADJ', increment = p) ifelse('try-error'%in%class(try(names(entity[start.index + look.position + increment]) == look.for)), FALSE, names(entity[start.index + look.position + increment]) == look.for)
          if(pop[y + 1 + p] == .lookup[2] & ( names(pop[y + 2 + p]) == 'ADJ' | ( names(pop[y + 2 + p]) == 'ADV' & ifelse('try-error'%in%class(try(names(pop[y + 3 + p]) == 'ADJ')), F, names(pop[y + 3 + p]) == 'ADJ') ) ) ){
            
            if(y-1>0){
              pop <- pop[(y-1):length(pop)]
            }else{
              pop <- c(NA, pop[y:length(pop)])
            }
            pop <- tryCatch({pop[1:grep('ADJ', names(pop))[2]]}, error = function(e) NULL)
            if(!is.null(pop)){
              pop <- tibble(TARGET = pop[2], CCONJ = pop[3 + p], ADV = ifelse(names(pop[4 + p]) == 'ADV', pop[4 + p], NA),
                            ADJ = ifelse(is.na(ADV), pop[4 + p], pop[5 + p]), modifier = ifelse(names(pop[1]) == 'ADV', pop[1], NA),
                            comma = p)
            }
            return(pop)
          }
          
        }
      })
      tmp <- do.call(rbind, tmp)
      return(tmp)
    }
  }
}






, mc.cores=4)


library(stringr)
library(stringi)
syntax.regex <- '(ADV\\s)?ADJ\\s(PUNCT\\s)?CCONJ\\s(ADV\\s)?ADJ'
make_regex <- function(INDEX){
  TARGET = paste0('\\\\b',df$TARGET[INDEX], '\\\\b')
  LEMMA = paste0(txtparsed[[INDEX]], collapse = ' ')
  SYNTAX = paste0(names(txtparsed[[INDEX]]), collapse = ' ')
  SYNTAX <- unlist(str_extract(SYNTAX, syntax.regex))
  SYNTAX <- unique(SYNTAX)
  tmp <- str_replace_all(SYNTAX, c(
    'ADV' = '\\\\w+',
    'CCONJ' = 'and',
    'PUNCT' = '\\\\,'
  ))
  regex1 <- sub('ADJ', TARGET, tmp)
  regex1 <- str_replace_all(regex1, c(
    ' ' = '\\\\s',
    'ADJ' = '\\\\w+'
    ))
  regex2 <- stri_replace_last(tmp, replacement = TARGET, regex = 'ADJ')
  regex2 <- str_replace_all(regex2, c(
    ' ' = '\\\\s',
    'ADJ' = '\\\\w+'
  ))
  match1 <- unlist(str_extract_all(LEMMA, regex1))
  match2 <- unlist(str_extract_all(LEMMA, regex2))
  MATCH <- unlist(c(match1, match2))
  tryCatch(tmp <- lapply(MATCH, function(y){
    POS <- spacy_parse(y, pos = T)
    if(paste0(POS$pos, collapse = ' ') %in% SYNTAX){
      ADJ <- filter(POS, pos == 'ADJ' & !lemma == df$TARGET[INDEX])$lemma
      TARGET_mod <- filter(POS, (pos == 'ADJ' & lemma == df$TARGET[INDEX]) | pos == 'ADV')
      TARGET_mod <- TARGET_mod$lemma[TARGET_mod$pos == 'ADV' & TARGET_mod$token_id == TARGET_mod$token_id[TARGET_mod$pos == 'ADJ'] - 1]
      TARGET_mod <- ifelse(identical(TARGET_mod, character(0)), NA, TARGET_mod)
      
      ADV <- filter(POS, (pos == 'ADJ' & !lemma == df$TARGET[INDEX]) | pos == 'ADV')
      ADV <- ADV$lemma[ADV$pos == 'ADV' & ADV$token_id == ADV$token_id[ADV$pos == 'ADJ'] - 1]
      ADV <- ifelse(identical(ADV, character(0)), NA, ADV)
      
      first <- filter(POS, pos == 'ADJ')
      first <- ifelse(first$token_id[first$lemma == df$TARGET[INDEX]] < first$token_id[!first$lemma == df$TARGET[INDEX]], 1, 0)
      dta <- tibble(match = y, ADJ, TARGET_mod, ADV, first)
      return(dta)
    }
    }), warning = function(e) print(INDEX))
  tmp <- do.call(rbind, tmp)
  return(tmp)
}
system.time(p <- pbmclapply(1:length(txtparsed), make_regex, mc.cores=4))
system.time(p <- pbmclapply(1:10, make_regex, mc.cores=4))

reps <- sapply(p, function(x) ifelse(is.null(nrow(x)), 0, nrow(x)))
df <- df[rep(1:nrow(df), reps),]
p <- do.call(rbind, p)
p <- bind_cols(df, p)


