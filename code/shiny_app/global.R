library(wordcloud)

# NOTE: Run first: cp -r code/shared code/shiny_app/.
source('shared/common.R', chdir=T)
source('shared/tokenizer.R', chdir=T)
source('shared/models.R', chdir=T)

# Load ngram datasets
sub_unigrams <- readRDS("data/en_US.sub_unigrams.rds")
sub_bigrams <- readRDS("data/en_US.sub_bigrams.rds")
sub_trigrams <- readRDS("data/en_US.sub_trigrams.rds")
sub_quadgrams <- readRDS("data/en_US.sub_quadgrams.rds")