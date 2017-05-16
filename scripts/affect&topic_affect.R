# sentence with love
require(qdap)
require(plyr)
require(stringr)
require(twitteR)

# import positive and negative words 
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")

#sentiment score function
score.sentiment = function(sentences, pos, neg, .progress='none')
{
        require(plyr)
        require(stringr)
        
        # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
        # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
        scores = laply(sentences, function(sentence, pos, neg) {
                
                # clean up sentences with R's regex-driven global substitute, gsub():
                sentence = gsub('[[:punct:]]', '', sentence)
                sentence = gsub('[[:cntrl:]]', '', sentence)
                sentence = gsub('\\d+', '', sentence)
                # and convert to lower case:
                sentence = tolower(sentence)
                
                # split into words. str_split is in the stringr package
                word.list = str_split(sentence, '\\s+')
                # sometimes a list() is one level of hierarchy too much
                words = unlist(word.list)
                
                # compare our words to the dictionaries of positive & negative terms
                pos.matches = match(words, pos)
                neg.matches = match(words, neg)
                
                # match() returns the position of the matched term or NA
                # we just want a TRUE/FALSE:
                pos.matches = !is.na(pos.matches)
                neg.matches = !is.na(neg.matches)
                
                # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
                score = sum(pos.matches) - sum(neg.matches)
                
                return(score)
        }, pos, neg, .progress=.progress )
        
        scores.df = data.frame(score=scores, text=sentences)
        return(scores.df)
}


# import positive and negative smileys
pos.emo = readLines("pos_emo.txt")
neg.emo = readLines("neg_emo.txt")

#smiley sentiment function
emo.sentiment = function(sentences, pos.emo, neg.emo, .progress='none')
{
        require(plyr)
        require(stringr)
        
        # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
        # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
        scores = laply(sentences, function(sentence, pos.emo, neg.emo) {
                
                # clean up sentences with R's regex-driven global substitute, gsub():
        
                # and convert to lower case:
                sentence = tolower(sentence)
                
                # split into words. str_split is in the stringr package
                word.list = str_split(sentence, '\\s+')
                # sometimes a list() is one level of hierarchy too much
                words = unlist(word.list)
                
                # compare our words to the dictionaries of positive & negative terms
                pos.matches = match(words, pos.emo)
                neg.matches = match(words, neg.emo)
                
                
                # match() returns the position of the matched term or NA
                # we just want a TRUE/FALSE:
                pos.matches = !is.na(pos.matches)
                neg.matches = !is.na(neg.matches)
                
                # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
                score = sum(pos.matches) - sum(neg.matches)
                
                return(score)
        }, pos.emo, neg.emo, .progress=.progress )
        
        scores.df = data.frame(score=scores, text=sentences)
        return(scores.df)
}

#some users did SWL twice, here we aggregate the mean score for these users

swl1 <- aggregate(swl ~ user_id, data=swl_status2, mean)
swl_status2 <- merge(swl1, swl_status2, by.x="user_id", by.y="user_id", all.x=TRUE)
swl_status2$swl.y <- NULL
swl_status2 <- swl_status2[!duplicated(swl_status2),]


##sum up the affect of each user
result_all <- score.sentiment(swl_status2$status, pos, neg)
swl_status2$result.sent = result_all$score
t <- aggregate(result.sent ~ user_id, data=swl_status2, sum)
swl_senti <- merge(t, swl, by.x="user_id", by.y="userid", all.x=TRUE)
cor.test(swl_senti$swl,swl_senti$result.sent)
sentiment <- swl_senti[, c("user_id","result.sent")]
swl_status3 <- swl_status2
swl_status3$result.sent = result_all$score
#0.1588095 correlation with SWL

##affect score for all users using the word sentiment list + smiley sentiment list
result_emo <- emo.sentiment(swl_status2$status, pos.emo, neg.emo)
swl_status2$result.sent2 = result_emo$score
t2 <- aggregate(result.sent2 ~ user_id, data=swl_status2, sum)
swl_senti_emo <- merge(t2, swl_senti, by.x="user_id", by.y="user_id", all.x=TRUE)
swl_senti_emo<- transform(swl_senti_emo, senti_all = result.sent+result.sent2)
swl_senti_emo <- merge(swl_senti_emo, swl_count, by.x="user_id", by.y="userid", all.x=TRUE)
swl_senti_emo<- transform(swl_senti_emo, senti_all2 = senti_all/count)
swl_senti_emo <- swl_senti_emo[swl_senti_emo[,7]>40,]
cor.test(swl_senti_emo$swl,swl_senti_emo$senti_all2)
#0.2084735  corrlation with SWL



### merge affect score with SWL score so that we can do correlation
swl_status3 <- swl_status2
fea_all2$Prediction1 <- predict(fit1, fea_all2,OOB=TRUE)
prediction <- fea_all2[,c("userid","Prediction1","swl")]
swl_status3 <- transform(swl_status3, sen.total = result.sent+result.sent2)

sent_status <- merge(swl_status3, prediction, by.x="user_id", by.y="userid", all.x=TRUE)
sent_status <- sent_status[!duplicated(sent_status$status),]

t_sen <- aggregate(sen.total ~ user_id, data=sent_status, mean)
sent <- merge(swl_status3, t_sen, by.x="user_id", by.y="user_id", all.x=TRUE)
sent<- sent[!duplicated(sent$user_id),]
sent<- sent[complete.cases(sent),]
sent <- merge(prediction, sent, by.x="userid", by.y="user_id", all.x=TRUE)

#statistics of this sample 
summary(sent$sen.total.y)
length(which(sent_status$sen.total==0))
length(which(sent_status$sen.total>0))
length(which(sent_status$sen.total<0))


#affect score correlate with self-report SWL
cor(sent$swl,sent$sen.total.y)
#0.21

#affect score correlate with machine predicted SWL
cor(sent$Prediction1,sent$sen.total.y)
#0.45

#topic affect score
#mathematics, mean
text <- grep("(?=.*mathematics)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
#mean  -0.05

objective <- fea_all2[,c("userid","senti_all2")]

text <- grep("(?=.*mathematics)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
text.score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text.score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total) 

#'mathematic' affect score correlate with SWL
#NG not enough cases  -0.21
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total)
#NG not enough cases 0.36
cor.test(text_dataframe2$senti_all2,text_dataframe2$swl.x) 


#talk with friends
text <- grep("(?=.*chat)(?=.*friend)|(?=.*talk)(?=.*friend)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
##'talk with friends'affect score correlate with SWL
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total) 
# 0.16 ***
cor.test(text_dataframe2$swl,text_dataframe2$sen.total)
#0.03NG
#mean = 1.079
cor.test(text_dataframe2$sen.total,text_dataframe2$senti_all2)
cor (text_dataframe2$swl,text_dataframe2$Prediction1)

#talk about homework
text <- grep("(?=.*homework)|(?=.*assignment)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
#correlate with SWL
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
# 0.18***
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)
# 0.12***


#chores
text <- grep("(?=.*laundry)|(?=.*clean room)|(?=.*groceries)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
#correlate with SWL
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x)
# 0.12**
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)
#0.06NG
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 

#mean 0.76
#people are quite happy before/after they finish the chore,that's when they mention the chores on FB

#having a meal
text <- grep("(?=.* dinner)|(?=.* lunch)|(?=.* breakfast)|(?=.* meal)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
#correlate with SWL
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total) 
#0.14***
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total)
#0.09***
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 
# mean 1.058

#lecture
text <- grep("(?=.* lecture)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
#correlate with SWL
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total) 
#0.18***
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total)
#0.21***
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 
#mean = 0.148

text <- grep("(?=.* talk)(?=.* mom)|(?=.* talk)(?=.* dad)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
#correlate with SWL
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
#0.22**
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)
#0.26***
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 
#mean = 0.1 
#######


#talk about other people
text <- grep("(?=.*she)|(?=.*he)|(?=.*they)|(?=.*them)|(?=.*his)|(?=.*her)|(?=.*herself)|(?=.*himself)|(?=.*themselves)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
#correlate with SWL
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
#0.43
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)
#0.12
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 


#self related
text <- grep("(?=.*I)|(?=.*me)|(?=.*our)|(?=.*we)|(?=.*myself)|(?=.*ourselves)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
#correlate with SWL
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
#0.43
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 

#money
text <- grep("(?=.*money)|(?=.*bill)|(?=.*pay)|(?=.*check)|(?=.*cash)|(?=.*spend)|(?=.*bought)|(?=.*buy)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
#correlate with SWL
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
#
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 

#holiday
text <- grep("(?=.*vacation)|(?=.*holiday)|(?=.*weekend)|(?=.*saturday)|(?=.*vacations)|(?=.*sunday)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
#correlate with SWL
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 

#children
text <- grep("(?=.*my daughter)|(?=.*my children)|(?=.*my child)|(?=.*my kid)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
#correlate with SWL
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
#
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)

cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total)

#work
text <- grep("(?=.*job)|(?=.*work)|(?=.*office)|(?=.*assignment)|(?=.*project)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
#correlate with SWL
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 

text <- grep("(?=.*job)|(?=.*office)|(?=.*assignment)|(?=.*project)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
#
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 

#intimate relationship
text <- grep("(?=.*boyfriend)|(?=.*girlfriend)|(?=.*bf)|(?=.*gf)|(?=.*wife)|(?=.*husband)|(?=.*marriage)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
#
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)
cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 

#religion
text <- grep("(?=.* god )|(?=.*pray)|(?=.*jesus)|(?=.*church)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
#
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)

cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total)

#death
text <- grep("(?=.* death )|(?=.*dead)|(?=.*dying)|(?=.*cancer)|(?=.*sick)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
text_dataframe2<- merge(objective, text_dataframe, by.x="userid", by.y="user_id", all.x=TRUE)
text_dataframe2<- text_dataframe2[complete.cases(text_dataframe2),]
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
#
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)

cor.test(text_dataframe2$senti_all2,text_dataframe2$sen.total) 

#pet
text <- grep("(?=.*cat)|(?=.*dog)|(?=.*pet)|(?=.*puppy)",sent_status$status,perl=TRUE)
text_dataframe<-sent_status[text,]
summary(text_dataframe$sen.total)
text_score <- aggregate(sen.total ~ user_id, data=text_dataframe, mean)
text_dataframe2<- merge(text_score, text_dataframe, by.x="user_id", by.y="user_id", all.x=TRUE)
text_dataframe2 <- text_dataframe2[!duplicated(text_dataframe2$user_id),]
cor.test(text_dataframe2$Prediction1,text_dataframe2$sen.total.x) 
cor.test(text_dataframe2$swl.x,text_dataframe2$sen.total.x)
#


       