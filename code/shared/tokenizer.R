library(doParallel)

source('contractions.R', chdir=T)
source('profanity.R', chdir=T)

# Defined patterns to eliminate noisy tokens...
patterns = c()
# ...non printable characters
patterns <- c(patterns, '([^[:print:]])')
# ...emails
patterns <- c(patterns, '(([a-z0-9_\\.-]+)@([\\da-z\\.-]+)\\.([a-z\\.]{2,6}))')
# ...web links
patterns <- c(patterns, '((https?://)?([\\da-z\\.-]+)\\.([a-z\\.]{2,6})([/\\w \\.-]*)*/?)')
# ...IP addresses
#patterns <- c(patterns, '((?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?))')
# ...hex values
patterns <- c(patterns, '(#{1}([a-f0-9]{6}|[a-f0-9]{3}))')
# ...remaining numbers and double spaces (This takes care also of IP addresses)
patterns <- c(patterns, '([0-9]+)|([[:space:]])')
# ...hyphenated words
patterns <- c(patterns, '((?<=[[:alpha:]])-(?=[[:alpha:]]))')
# ...empty symbol at beggining
patterns <- c(patterns, '(^ø)')
all_noisy_patterns <- paste(patterns, collapse='|')

tokenize <- function(text) {
    # Lowercase to normalize tokens
    text <- tolower(text)
    # Resolve contractions
    text <- expand_contractions(text)
    # Remove profanity
    #text <- remove_profanity(text, all_profanity_patterns)
    # Replace them by space
    text <- gsub(all_noisy_patterns, ' ', text, perl=TRUE)
    # ... collapse double spaces
    text <- trimws(gsub(' +', ' ', text))
    # Result:
    grep(' ', trimws(unlist(strsplit(text, split=paste('[[:punct:]]')))), value=TRUE)
}

# Tokenizer
# params:
# @file: Path to source filename
# @output: Path of file to write tokenized data
# @n_lines: Number of lines to read from source file, Default to all.
# @verbose: Print verbose info of doParallel proceses
tokenizer <- function(file, output, n_lines=-1, verbose=F) {
    my_cluster <- makeCluster(detectCores() - 1)   # Leave at least one core to OS
    doParallel::registerDoParallel(my_cluster)
    
    #all_profanity_patterns <- allProfanityPatterns()
    all_lines = readLines(file, n_lines, encoding='UTF-8')
    results <- foreach(i=1:length(all_lines), 
                       .export=c('contractions', 'expand_contractions', 'tokenize', 'all_noisy_patterns'),
                       .inorder=FALSE, .verbose=verbose) %dopar% {
                           tokenize(all_lines[i])
                       }
    # Write data on output file
    writeLines(unlist(results), output, sep='\n')
    stopCluster(my_cluster)
}