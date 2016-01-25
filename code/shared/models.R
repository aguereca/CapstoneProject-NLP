library(data.table)
library(dplyr)
library(digest)
library(bitops)

### >>> DEVELPMENT FUNCTIONS

sentence_maker2 <- function(size, top=10) {
    last_word <- '<s>'
    sentence <- c(last_word)
    for (i in seq(1:size)) {
        #last_word <- sample(as.character(all_unigrams[token1 %in% filter(all_bigrams, token1==last_word)[1:top]$token2, token1]), 1)
        last_word <- sample(filter(all_bigrams, token1==last_word)$token2[1:top], 1)
        sentence <- c(sentence, last_word)
    }
    paste(sentence, collapse=' ')
}

sentence_maker3 <- function(start, size, top=10) {
    pre_last_word <- '<s>'
    last_word <- start
    sentence <- c(pre_last_word, last_word)
    for (i in seq(1:size)) {
        new_top <- top
        options <- filter(all_trigrams, token1==pre_last_word, token2==last_word)$token3
        op_len <- length(options)
        if (op_len < top) {new_top <- op_len}
        if (op_len > 0) {
            tmp <- sample(options[1:new_top], 1)
            pre_last_word <- last_word
            last_word <- tmp
            sentence <- c(sentence, last_word)
        }
    }
    paste(sentence, collapse=' ')
}

sentence_maker4 <- function(start1, start2, size, top=10) {
    pre_pre_last_word <- '<s>'
    pre_last_word <- start1
    last_word <- start2
    sentence <- c(pre_pre_last_word, pre_last_word, last_word)
    for (i in seq(1:size)) {
        new_top <- top
        options <- filter(all_quadgrams, token1==pre_pre_last_word, token2==pre_last_word, token3==last_word)$token4
        op_len <- length(options)
        if (op_len < top) {new_top <- op_len}
        if (op_len > 0) {
            tmp <- sample(options[1:new_top], 1)
            pre_pre_last_word <- pre_last_word
            pre_last_word <- last_word
            last_word <- tmp
            sentence <- c(sentence, last_word)
        }
    }
    paste(sentence, collapse=' ')
}

get_bigrams_dev <- function(pre_tokens, how_many, cut_metric=2) {
    return(head(all_bigrams %>%
                    filter( token1==pre_tokens[1], t2_cont_metric > cut_metric) %>%
                    select(c(token2, cont_metric)) %>%
                    mutate(token2, cont_metric=cont_metric * 0.001), how_many))
}

get_trigrams_dev <- function(pre_tokens, how_many, cut_metric=2) {
    return(head(all_trigrams %>%
                    filter(token1==pre_tokens[1], token2==pre_tokens[2], t3_cont_metric > cut_metric) %>%
                    select(c(token3, cont_metric)) %>%
                    mutate(token3, cont_metric=cont_metric * 0.01), how_many))
}

get_quadgrams_dev <- function(pre_tokens, how_many, cut_metric=2) {
    return(head(all_quadgrams %>%
                    filter(token1==pre_tokens[1], token2==pre_tokens[2], token3==pre_tokens[3], 
                           t4_cont_metric > cut_metric) %>%
                    select(c(token4, cont_metric)), how_many))
}

word_predictions_dev <- function(pre_tokens, how_many=100, cut_metric=2) {
    options = data.table()
    len <- length(pre_tokens)
    if (len > 2) {
        options <- rbind(options, get_quadgrams_dev(pre_tokens, how_many, cut_metric), use.names=F)
    }
    if (len > 1) {
        options <- rbind(options, get_trigrams_dev(pre_tokens[(len-1):len], how_many, cut_metric), use.names=F)
    }
    if (len > 0) {
        options <- rbind(options, get_bigrams_dev(pre_tokens[len], how_many, cut_metric), use.names=F)
    }
    colnames(options) <- c('word', 'metric')
    options <- options %>% 
        group_by(word) %>% 
        summarise(metric=mean(metric)) %>%
        arrange(desc(metric))
    return(options)
}

### <<<< DEVELPMENT FUNCTIONS

### >>>> FINAL FUNCTIONS

get_ngram_pred <- function(dset, sel_token) {
    return(dset %>% 
               filter(token==sel_token) %>% 
               select(c(word, metric)))
}

word_predictions <- function(pre_tokens) {
    options = data.table()
    len <- length(pre_tokens)
    if (len > 2) {
        options <- rbind(options, get_ngram_pred(sub_quadgrams, cksum(paste(pre_tokens, collapse=' '))), use.names=F)
    }
    if (len > 1) {
        options <- rbind(options, get_ngram_pred(sub_trigrams, cksum(paste(pre_tokens[(len-1):len], collapse=' '))), use.names=F)
    }
    if (len > 0) {
        options <- rbind(options, get_ngram_pred(sub_bigrams, cksum(paste(pre_tokens[len], collapse=' '))), use.names=F)
    }
    if (len == 0) {
        options <- rbind(options, select(sub_unigrams, c(word=token, metric))[1:100,], use.names=F)
    }
    options <- options %>% 
        group_by(word) %>% 
        summarise(metric=mean(metric)) %>%
        arrange(desc(metric))
    return(options)
}

sentence_maker_final <- function(start_str='', size=20, top=10) {
    sentence <- c('<s>', unlist(strsplit(start_str, split=' ')))
    for (i in seq(1:size)) {
        new_top <- top
        pre_tokens <- sentence
        if (length(sentence) > 2) {
            pre_tokens <- sentence[(length(sentence)-2):length(sentence)]
        }
        options <- word_predictions(pre_tokens)$word
        op_len <- length(options)
        if (op_len < top) {new_top <- op_len}
        if (op_len > 0) {
            sentence <- c(sentence, sample(options[1:new_top], 1))
        }
    }
    paste(sentence, collapse=' ')
}

### <<<< FINAL FUNCTIONS