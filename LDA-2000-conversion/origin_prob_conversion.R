setwd('~/Dropbox/myPersonality/submit_version/PLOS ONE/revision/script/LDA-2000-conversion/')

origin<-read.csv('2000topics.top20freqs.keys.csv',header = F, fill=FALSE,row.names=NULL, stringsAsFactors = FALSE)
origin<-origin[,-1]

origin_word_table<-origin[,seq(1,ncol(origin),2)] #separate out the topic words
origin_freq_table<-origin[,seq(2,ncol(origin),2)] #separate out the topic frequencies

#conditional probability of probability(topic|word) requires total number of words for denominator
origin_total_words<-sum(sum(freq_table, na.rm=TRUE), na.rm=TRUE)

origin_corpus<-unique(flatten(origin_word_table)) #unique list of all words

#create an empty list that's the same size as the corpus list
origin_corpus_freqs<-integer(length(origin_corpus))

# conditional probability of p(topic|word) is calculated as p(topic and word)/p(word)
# p(word) is calculated as the sum( word frequency across all topics)/origin_total_words

#to obtain the first part of the p(word) calculation, loop through each word in the corpus...
for (i in seq(1,length(origin_corpus))){
  # record the total number of times that word is used (across all topics)
  origin_corpus_freqs[i]<-sum(origin_freq_table[origin_word_table==origin_corpus[i]])
}

#to calculate p(word)
origin_corpus_p_w<-origin_corpus_freqs/origin_total_words

#simple way to copy a table of the same dimensions
origin_prob_table<-origin_freq_table

#to calculate p(topic|word), we can loop through the word table, and use the word to search the corpus probabilities list - p(word), and use the loop indexes to select the word frequency - to create p(topic and word) 
for (topic_index in seq(1,nrow(origin_word_table))){
 
  topic<-origin_word_table[topic_index,]
  words<-topic[!is.na(topic)]
  for (word_index in seq(1,length(words))){
    freq<-origin_freq_table[topic_index,word_index] #obtain the frequency for this word in this topic
    p_t_and_w<-freq/origin_total_words #compute p(topic and word)
    word<-words[word_index] #get the actual word
    p_w<-origin_corpus_p_w[origin_corpus==word] #use the word to look up the p(word) computed earlier
    p_t_w<-p_t_and_w/p_w # compute the p(topic|word) as p(topic and word)/p(word)
    origin_prob_table[topic_index, word_index]<-p_t_w #store the result
    
  }
}
