library(topicmodels)
require(magrittr)
require(tm)
library(RTextTools)

#merging tables status + status count + swl
statusswl <- read.csv("user_all_status_swl.csv", header = T, fill=TRUE,row.names=NULL)
swl_count <- read.csv("user_status_numbers_swl.csv", header = T, fill=TRUE,row.names=NULL)
swl_status <- merge(statusswl, swl_count, by.x="user_id", by.y="userid", all.x=TRUE)
swl<- read.csv("swl.csv", header = T, fill=TRUE,row.names=NULL)
swl_status2 <- merge(swl_status, swl, by.x="user_id", by.y="userid", all.x=TRUE)
swl_status2 <- swl_status2[complete.cases(swl_status2),]
swl_status2$X.x<- NULL
swl_status2$X.y<- NULL

t2 <- aggregate(status~user_id, swl_status2, FUN= paste, collapse=' ')

sam <- t2
ss<- sam$status
ss<- tolower(iconv(ss,"ISO-8859-1","UTF-8"))
ss <- gsub("\\d", "", ss)
ss <- gsub("\\W", " ", ss)
ss <- gsub("[[:punct:]]"," ", ss)
ss <- gsub("bobsnewline", "", ss)


#source https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 200

#Run LDA using Gibbs sampling

dtm <- create_matrix(ss, language="english", removeNumbers=TRUE, stemWords=FALSE, weighting=weightTf)
dtm<- removeSparseTerms(dtm, .99)

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]  
ldaOut <-LDA(dtm.new,k, method= "Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))


#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,20))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma,20)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities4.csv"))


#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))
