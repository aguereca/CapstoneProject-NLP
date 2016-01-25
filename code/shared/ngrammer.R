library(doParallel)
library(RWeka)
library(tm)

###  --> BEGIN

## Approach 1: Using RWeka and tm packages to generate TermDocumentMatrix(es)

# 1.- On R, run function 'ngramifyAll'. This will generate a TermDocumentMatrix for each ngram size
#     and media source file.

# Runtime: This is the slowest approach, on a MacBookPro 2013 with 16 GB took ~41hr only to generate
#       the TDM for the 'blogs' ngrams.


# NGrammer
# params:
# @vdata: Character vector of documents
# @gram_length: Length of n-gram. Default: 3
# @line_delim: Include sentence delimitators. ('<s>', '</s>'). Default: true
#
# Returns: tm::TermDocumentMatrix 
ngrammer <- function(vdata, gram_length=3, line_delim=TRUE) {
    # Sets the default number of threads to use to prevent RWeka errors
    # Reference: http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
    options(mc.cores=1)
    XGramTokenizer <- function(x) {
        if (line_delim) x <- paste('<s>', x, '</s>', sep=' ')
        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = gram_length, max = gram_length))
    }
    return(DocumentTermMatrix(Corpus(VectorSource(vdata)),
                              control=list(tokenize=XGramTokenizer)))
}

ngramifyAll <- function(n_lines=-1) {
    my_cluster <- makeCluster(detectCores() - 1)   # Leave at least one core to OS
    doParallel::registerDoParallel(my_cluster)
    
    all_stats <- foreach(i=1:3, .export=c('sourcesMedia', 'filePath', 'finalDataDir', 'ngrammer',
                                          'DocumentTermMatrix', 'Corpus', 'VectorSource'),
                         .verbose = TRUE) %dopar% {
    #for (i in seq(3)) {
        media <- sourcesMedia[i]
        all_lines = readLines(filePath(paste(media,'lines', sep='.'), 'en_US'), n_lines, encoding='UTF-8')
        stats <- c()
        for (j in seq(4)) {
            #foreach(j=1:4) %dopar% {
            dest_file <- filePath(paste(media, j,'tdm.rds', sep='.'), 'en_US')
            if (!file.exists(dest_file)) {
                # Time process to have a reference duration
                timers <- paste(media, system.time({
                    tdm <- ngrammer(all_lines, j)
                    saveRDS(tdm, dest_file)
                }), sep=' - ')
                rm(tdm)
                gc()
                print(c(stats, paste("RDS saved:", dest_file, sep = ' '), timers))
            }
        }
        rm(all_lines)
        gc()
    }
    print(all_stats)
    stopCluster(my_cluster)
    gc()
}

###   <-- END


###   --> BEGIN

## Approach 2: Parallelized unix script using associative arrays to generate ngrams.
# Reference: http://stackoverflow.com/questions/14410478/can-ngrams-be-generated-in-bash

# Runtime: It takes ~29hr on a MacBook Pro 2013, using 7 cores. Only for 'blogs' ngrams.

# This may not work 'as-is' in all systems, is mainly instructional reference
Comment(
`
# Example of how to generate ngrams of 'blogs' with this approach.

# 0.- Set current directory to: 
cd data/final/en_US

# 1.-  Add sentence separators to tokenized data, and split it into files of 500K lines
cat en_US.blogs.lines | sed -e 's/$/ <\\s>/g' | sed -e 's/^/<s> /g' > s.blogs.lines && split -a 1 -l 500000 s.blogs.lines s.blogs.lines.p && rm s.blogs.lines

# 2.- Using script 'ngram.sh' run this command: (It takes ~28hr on a MacBook Pro 2013, hence uses 7 cores)
find . -name "s.blogs.lines.p?" | xargs -n 1 -P 7 -I % time -p sh -c "time -p cat % | ./ngram.sh 1 > %.tf.1" && 
find . -name "s.blogs.lines.p?" | xargs -n 1 -P 7 -I % time -p sh -c "time -p cat % | ./ngram.sh 2 > %.tf.2" && 
find . -name "s.blogs.lines.p?" | xargs -n 1 -P 7 -I % time -p sh -c "time -p cat % | ./ngram.sh 3 > %.tf.3" && 
find . -name "s.blogs.lines.p?" | xargs -n 1 -P 7 -I % time -p sh -c "time -p cat % | ./ngram.sh 4 > %.tf.4"

# 3.- Aggregate ngram counts of all split files into one by ngram length:
sort -rn s.blogs.lines.p?.tf.1 | awk '{arr[$2]+=$1} END {for (i in arr) {print arr[i], i}}' | sort -rn > s.blogs.tokens.tf.1
sort -rn s.blogs.lines.p?.tf.2 | awk '{arr[$2" "$3]+=$1} END {for (i in arr) {print arr[i], i}}' | sort -rn > s.blogs.tokens.tf.2
sort -rn s.blogs.lines.p?.tf.3 | awk '{arr[$2" "$3" "$4]+=$1} END {for (i in arr) {print arr[i], i}}' | sort -rn > s.blogs.tokens.tf.3
sort -rn s.blogs.lines.p?.tf.4 | awk '{arr[$2" "$3" "$4" "$5]+=$1} END {for (i in arr) {print arr[i], i}}' | sort -rn > s.blogs.tokens.tf.4

# 4.- Remove temporal ngram counts of split files:
rm s.blogs.lines.p?.tf.?

# Final ngram counts will be in the files: 
#     s.blogs.tokens.tf.1, s.blogs.tokens.tf.2, s.blogs.tokens.tf.3, s.blogs.tokens.tf.4
# Data on result files might need minor cleaning to remove ngrams not matching required length,
# commands for that are ommited since we ended using Approach #3.
# We validated correctness of this method by comparing ngram counts with the others. 

`)

###   --> END


###   --> BEGIN

## Approach 3: Unix script based on 'paste' to generate ngram files. [FASTEST]
# Reference: http://gibrown.com/2013/01/26/unix-bi-grams-tri-grams-and-topic-modeling/

# Runtime: Using only 1 core, takes ~1.25hr for 'blogs' ngrams. Same time on all sources using 3 cores.
#          Can be optimized to run in ~15 min per source with further parallelization.

# This may not work 'as-is' in all systems, is mainly instructional reference
Comment(
`
# 0.- Set current directory to: 
cd data/final/en_US

# 1.- Run following command, for each media source (update 'mymedia' variable for each):
mymedia=blogs &&
cat en_US.$mymedia.lines.train | sed -e 's/$/ <\\s>/g' | sed -e 's/^/<s> /g' | sed G | tr ' ' '\n' > s.$mymedia.lines && 
time -p cat s.$mymedia.lines | sort | uniq -c | sort -rn > en_US.$mymedia.1grams && 
time -p paste -d ',' <(cat s.$mymedia.lines ) <(cat s.$mymedia.lines | tail -n+2) | grep -v -e "^," | grep -v -e ",$" | tr ',' ' ' | sort | uniq -c | sort -rn > en_US.$mymedia.2grams && 
time -p paste -d ',' <(cat s.$mymedia.lines ) <(cat s.$mymedia.lines | tail -n+2) <(cat s.$mymedia.lines | tail -n+3) | grep -v -e "^," | grep -v -e ",$" | tr ',' ' ' | sort | uniq -c | sort -rn > en_US.$mymedia.3grams && 
time -p paste -d ',' <(cat s.$mymedia.lines ) <(cat s.$mymedia.lines | tail -n+2) <(cat s.$mymedia.lines | tail -n+3) <(cat s.$mymedia.lines | tail -n+4) | grep -v -e "^," | grep -v -e ",$" | tr ',' ' ' | sort | uniq -c | sort -rn > en_US.$mymedia.4grams && 
rm s.$mymedia.lines && unset mymedia

# 2.- Ngram frequencies for each media can be found in files like: blogs.1grams, blogs.2grams, blogs.3grams, blogs.4grams 

## 3.- Ignore space as token on unigrams
all_media=("blogs" "news" "twitter") && for media in ${all_media[@]}; do sed -i '/[ ]$/d' en_US.$media.1grams; done && unset all_media
##all_media=("blogs" "news" "twitter") && for media in ${all_media[@]}; do sed -i '/[>| ]$/d' en_US.$media.1grams; done && unset all_media

# 4.- Remove tokens with sencence separators in wrong place, on all of ngram frecuency files.
#     (ngrams including sentence separators should have only <s> at beginning or <\s> at end, and no both consecutive)
find . -name 'en_US.*.?grams' | xargs -n 1 -P 7 -I % sed -i '/>  </d' %

# 5.- Merge ngram frequencies of all sources into one file by ngram size
sort -rn en_US.*.1grams | awk '{arr[$2]+=$1} END {for (i in arr) {print arr[i], i}}' | sort -rn > en_US.all_sources.1grams
sort -rn en_US.*.2grams | awk '{arr[$2" "$3]+=$1} END {for (i in arr) {print arr[i], i}}' | sort -rn > en_US.all_sources.2grams
sort -rn en_US.*.3grams | awk '{arr[$2" "$3" "$4]+=$1} END {for (i in arr) {print arr[i], i}}' | sort -rn > en_US.all_sources.3grams
sort -rn en_US.*.4grams | awk '{arr[$2" "$3" "$4" "$5]+=$1} END {for (i in arr) {print arr[i], i}}' | sort -rn > en_US.all_sources.4grams

`)

###   --> END