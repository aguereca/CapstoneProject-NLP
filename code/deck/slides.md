<style>
.small-code pre code {
  font-size: 0.9em;
}
</style>

Coursera's Data Science Capstone - NLP
========================================================
author: Armando Guereca
date: Jan 24, 2015

Introduction
========================================================

Objective is to generate a prediction algorithm to predict
next word typed by user, following key tasks where performed:

- Data cleaning: Tokenization of data sources (R parallel processing)
- Ngrams: Geneator of (1-4)gram frequencies for **FULL** datasources, 
          respecting sentence separators. (Fast, UNIX based)
- Language model: Created **OWN** algorithm inspired on
     'Kneser-Ney' and 'Stupid Backoff'
- Shiny app: 'Next word' and 'word completion' predictions,
     includes a random sentence generator.


Tokenization and Ngram models
========================================================

- Tokenizer expands english contractions (I'm -> i am)
- Includes profanity filter (disabled on current app)
- R parallel, tokenize ~600Mb of text on minutes
- Generate file of 'sentences' to prevent cross-sentence ngrams
- (1-2)gram frequencies all FULL data generated on ~1hr 
- Fast pre-processing pipeline allowed us to rerun it as needed
- At modeling phase, only words on dictionary (~350K on list) are keep
- Full details available at file: **1_getting_cleaning_data.Rmd**

Language Modeling
========================================================

Researched of common language models, mainly 
Kneser-Ney (*KN*) and Stupid Backoff (*SB*).
Special intereset on *Google's Stupid Backoff* hability of
approach *KN* results on a single-pass and low cost trainig.
Generated new algorithm that like *SB* is based on scores
instead of probabilities but that gives special importance 
to the 'continuation-probability' of *KN*, extending it forward.

Example: "a case of beer"
Given input "a", "case"" is predicted based on how many bigrams
are end with it; Given "a case", "of" is predicted given on how
many trigrams end with "case of"; Given "a case of", "beer" is
predicted based on a metric of trigrams that end with "case of beer".

Is pending to generate a quantitative statistical analysis
of this new algorithm vs *KN* and *SB* to estimate it real acuracy.

Shiny app
========================================================

Available at: https://aguereca.shinyapps.io/NLP-Capstone/

- Detailed instructions on left bar panel. User type text on input box 
  and top 10 predicted words are shown as options.
- User can select a predicted word and text is updated as well
  as new predictions are generated
- While user is typing a word 'completion' predictions
  shown relevant predictions
- Word cloud depicts with different size and colors the relative
  relevance of predictions
- Also is posible to let the algorithm generate 50 word sentences,
  initialized by user's input, we call it: Rant generator ;)
- Source code: https://github.com/aguereca/DevDataProducts
