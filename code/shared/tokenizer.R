library(doParallel)

source('contractions.R', chdir=T)
source('profanity.R', chdir=T)

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
                       .export=c('contractions', 'expand_contractions'),
                       .inorder=FALSE, .verbose=verbose) %dopar% {
        line = all_lines[i]
        # Lowercase to normalize tokens
        line <- tolower(line)
        # Resolve contractions
        line <- expand_contractions(line)
        # Remove profanity
        #line <- remove_profanity(line, all_profanity_patterns)
        
        # Eliminate noisy tokens...
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
        
        # Replace them by space
        line <- gsub(paste(patterns, collapse='|'), ' ', line, perl=TRUE)
        # ... collapse double spaces
        line <- trimws(gsub(' +', ' ', line))
        
        grep(' ', trimws(unlist(strsplit(line, split=paste('[[:punct:]]')))), value=TRUE)
    }
    # Write data on output file
    writeLines(unlist(results), output, sep='\n')
    stopCluster(my_cluster)
}