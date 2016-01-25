

awk 'length > max_length { max_length = length; longest_line = $0 } END { print max_length }' ./data/final/en_US/en_US.blogs.txt


# Sets the default number of threads to use
  options(mc.cores=1)




cat test.tkns | sed -e 's/$/<\\s>/g' | sed -e 's/^/<s>/g' | less

cat test.tkns | sed -e 's/$/ <\\s>/g' | sed -e 's/^/<s> /g' | ./ngram.sh 2 | grep '.* .*' | sort -r | less


time -p cat test.tkns | sed -e 's/^/<s> /g' | sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/ <\\s> /g' | ./ngram.sh 1 > blogs.1grams



cat en_US.blogs.tokens | sed -e 's/$/ <\\s>/g' | sed -e 's/^/<s> /g' > s.blogs.tokens && split -a 1 -l 500000 s.blogs.tokens s.blogs.tokens.p && rm s.blogs.tokens

cat en_US.blogs.tokens | split -a 1 -l 500000 s.blogs.tokens s.blogs.tokens.p

sed 'NUMq;d' file



find . -name "s.blogs.tokens.p?" | xargs -n 1 -P 7 -I % time -p sh -c "time -p cat % | ./ngram.sh 1 > %.tf.1" && find . -name "s.blogs.tokens.p?" | xargs -n 1 -P 7 -I % time -p sh -c "time -p cat % | ./ngram.sh 2 > %.tf.2" && find . -name "s.blogs.tokens.p?" | xargs -n 1 -P 7 -I % time -p sh -c "time -p cat % | ./ngram.sh 3 > %.tf.3" && find . -name "s.blogs.tokens.p?" | xargs -n 1 -P 7 -I % time -p sh -c "time -p cat % | ./ngram.sh 4 > %.tf.4"

rm blogs.tokens.p*


sort -rn s.blogs.tokens.p?.tf.1 | awk '{arr[$2]+=$1} END {for (i in arr) {print i,arr[i]}}' | sort -rn -k2 > s.blogs.tokens.tf.1




cat en_US.blogs.tokens | sed -e 's/$/ <\\s>/g' | sed -e 's/^/<s> /g' | sed G | tr ' ' '\n' > s.blogs.tokens

time -p paste -d ',' <(cat test.tokens ) <(cat test.tokens | tail -n+2) | grep -v -e "^," | grep -v -e ",$" | tr ',' ' ' | sort | uniq -c | sort -rn > test.bigrams



cat en_US.news.tokens | sed -e 's/$/ <\\s>/g' | sed -e 's/^/<s> /g' | sed G | tr ' ' '\n' > s.news.tokens && time -p paste -d ',' <(cat s.news.tokens ) <(cat s.news.tokens | tail -n+2) | grep -v -e "^," | grep -v -e ",$" | tr ',' ' ' | sort | uniq -c | sort -rn > news.2grams && time -p paste -d ',' <(cat s.news.tokens ) <(cat s.news.tokens | tail -n+2) <(cat s.news.tokens | tail -n+3) | grep -v -e "^," | grep -v -e ",$" | tr ',' ' ' | sort | uniq -c | sort -rn > news.3grams && time -p paste -d ',' <(cat s.news.tokens ) <(cat s.news.tokens | tail -n+2) <(cat s.news.tokens | tail -n+3) <(cat s.news.tokens | tail -n+4) | grep -v -e "^," | grep -v -e ",$" | tr ',' ' ' | sort | uniq -c | sort -rn > news.4grams && rm s.news.tokens


cat en_US.twitter.tokens | sed -e 's/$/ <\\s>/g' | sed -e 's/^/<s> /g' | sed G | tr ' ' '\n' > s.twitter.tokens && time -p paste -d ',' <(cat s.twitter.tokens ) <(cat s.twitter.tokens | tail -n+2) | grep -v -e "^," | grep -v -e ",$" | tr ',' ' ' | sort | uniq -c | sort -rn > twitter.2grams && time -p paste -d ',' <(cat s.twitter.tokens ) <(cat s.twitter.tokens | tail -n+2) <(cat s.twitter.tokens | tail -n+3) | grep -v -e "^," | grep -v -e ",$" | tr ',' ' ' | sort | uniq -c | sort -rn > twitter.3grams && time -p paste -d ',' <(cat s.twitter.tokens ) <(cat s.twitter.tokens | tail -n+2) <(cat s.twitter.tokens | tail -n+3) <(cat s.twitter.tokens | tail -n+4) | grep -v -e "^," | grep -v -e ",$" | tr ',' ' ' | sort | uniq -c | sort -rn > twitter.4grams && rm s.twitter.tokens


grep -v -e '>.*>' news.4grams | less
grep -v '<' all_sources.4grams | less

grep -E "^ *1 " en_US.all_sources.4grams | wc -l

all_media=("blogs" "news" "twitter") && for media in ${all_media[@]}; do sed -i '/[>| ]$/d' $media.1grams; done && unset all_media


Line with issue: 615492

>>> Of the low freq ngrams... Which last words are low freq unigrams? 

3:23am

sum 1grams: 95445743



--->
TEST
system.time({
  my_cluster <- makeCluster(detectCores() - 1)   # Leave at least one core to OS
  doParallel::registerDoParallel(my_cluster)
  uni_words <- as.character(all_unigrams[count>1, token1][1:100])
  u_cp_rest <- foreach(i=1:length(uni_words), .inorder=F, .combine=rbind) %dopar% {
    word <- uni_words[i] 
    c(word,
    length(all_bigrams[all_bigrams$token2==word, 1])) 
  }
  stopCluster(my_cluster)
  #all_unigrams$follows <- 1
  all_unigrams[token1%in%u_cp_rest[ ,1]]$follows = as.numeric(u_cp_rest[ ,2])
})
<---

 #####
 O L D
 #####
all_unigrams <- data.table(read.delim(pipe(paste("grep -E -v '[^a-z0-9 <>]' ", filePath('all_sources.1grams', 'en_US'))), sep=' ', col.names = c('count', 'token1'), header=F, stringsAsFactors=F), key="token1")
all_unigrams$follows <- 0
setkey(all_unigrams, 'token1')

cont_table <- table(all_bigrams[token1!='<s>', token2], exclude="<\\s>")
all_unigrams[token1 %in% names(cont_table)]$follows = as.numeric(cont_table)
rm(cont_table)
all_unigrams$cont_metric <- ifelse(all_unigrams$follows==0, log(all_unigrams$count), log(all_unigrams$count)+(1/all_unigrams$follows))
all_unigrams <- arrange(all_unigrams, desc(cont_metric))
saveRDS(all_unigrams, filePath("all_unigrams.rds", "en_US"))

length(filter(all_unigrams, cont_metric<1)$count)

#all_unigrams <- all_unigrams[count>1,]



all_bigrams <- data.table(read.delim(pipe(paste("grep -E -v '[^a-z0-9 <>]' ", filePath('all_sources.2grams', 'en_US'))), sep=' ', col.names = c('count', 'token1', 'token2'), header=F, stringsAsFactors=F))
setkey(all_bigrams, 'token1')

all_bigrams <- merge(all_bigrams, select(all_unigrams, token1,follows), by.x='token2', by.y='token1')
all_bigrams$cont_metric <- ifelse(all_bigrams$follows==0, log(all_bigrams$count), log(all_bigrams$count)+(1/all_bigrams$follows))
all_bigrams <- arrange(all_bigrams, desc(cont_metric))
saveRDS(all_bigrams, filePath("all_bigrams.rds", "en_US"))


#--- Problem of hashtags and misspelings
head(filter(all_bigrams, token1=='something'))

wordlist <- data.table(read.delim(pipe(paste("grep -E -v '[^a-z]' ", filePath('wordlist.txt', 'en_US'))), sep=' ', col.names = c('word'), header=F, stringsAsFactors=F))

misspells <- data.table(setdiff(all_unigrams[,token1], wordlist$word))
colnames(misspells) <- c('word')
misspells$follows <- all_unigrams[token1 %in% misspells[1], follows]
length(misspells[follows>1, word])

found <- data.table(c(intersect(all_unigrams[,token1], wordlist$word), '<s>'))
colnames(found) <- c('word')
#---


# Clean freq tables
all_unigrams <- all_unigrams[token1 %in% found$word,]
setkey(all_unigrams, 'token1')
all_unigrams <- arrange(all_unigrams, desc(cont_metric))
saveRDS(all_unigrams, filePath("all_unigrams.rds", "en_US"))

all_bigrams <- filter(all_bigrams, token1 %in% found$word, token2 %in% found$word)
all_bigrams <- filter(all_bigrams, cont_metric >= 0.5)
setkey(all_bigrams, 'token1')
all_bigrams <- arrange(all_bigrams, desc(cont_metric))
saveRDS(all_bigrams, filePath("all_bigrams.rds", "en_US"))

View(filter(all_bigrams, token1=='something'))



all_trigrams <- data.table(read.delim(pipe(paste("grep -E -v '[^a-z0-9 <>]' ", filePath('all_sources.3grams', 'en_US'))), sep=' ', col.names = c('count', 'token1', 'token2', 'token3'), header=F, stringsAsFactors=F))
all_trigrams <- filter(all_trigrams, token1 %in% found$word, token2 %in% found$word, token3 %in% found$word)

all_trigrams <- merge(all_trigrams, select(all_unigrams, token1,follows), by.x='token3', by.y='token1')
all_trigrams$cont_metric <- ifelse(all_trigrams$follows==0, log(all_trigrams$count), log(all_trigrams$count)+(1/all_trigrams$follows))
all_trigrams <- filter(all_trigrams, cont_metric >= 0.5)
setkey(all_trigrams, 'token2')
setkey(all_trigrams, 'token1')
all_trigrams <- arrange(all_trigrams, desc(cont_metric))
saveRDS(all_trigrams, filePath("all_trigrams.rds", "en_US"))

View(filter(all_trigrams, token1=='something', token2=='else'))



all_quadgrams <- data.table(read.delim(pipe(paste("grep -E -v '[^a-z0-9 <>]' ", filePath('all_sources.4grams', 'en_US'))), sep=' ', col.names = c('count', 'token1', 'token2', 'token3', 'token4'), header=F, stringsAsFactors=F))
all_quadgrams <- filter(all_quadgrams, token1 %in% found$word, token2 %in% found$word, token3 %in% found$word, token4 %in% found$word)

all_quadgrams <- merge(all_quadgrams, select(all_unigrams, token1,follows), by.x='token4', by.y='token1')
all_quadgrams$cont_metric <- ifelse(all_quadgrams$follows==0, log(all_quadgrams$count), log(all_quadgrams$count)+(1/all_quadgrams$follows))
all_quadgrams <- filter(all_quadgrams, cont_metric >= 0.5)
setkey(all_quadgrams, 'token3')
setkey(all_quadgrams, 'token2')
setkey(all_quadgrams, 'token1')
all_quadgrams <- arrange(all_quadgrams, desc(cont_metric))
saveRDS(all_quadgrams, filePath("all_quadgrams.rds", "en_US"))

View(filter(all_quadgrams, token1=='something', token2=='else', token3=='to'))

<<<
OLD
<<<

