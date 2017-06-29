install.packages(c("tm","RTextTools","tidyverse","stringr","lda","textmineR","stringi","glmnet","party","qdap","ridge", "lubridate", "Metrics"))

library(tm)
library(RTextTools)
library(tidyverse)
library(stringr)
library(lda)
library(textmineR)
library(stringi)
library(glmnet)
library(party)
library(qdap)
library(ridge)
library(lubridate)
library(Metrics)

#additional info:
# the original dataset of status updates contained a great many newline characters. 
# This was posing problems with viewing the data in spreadsheet software and at other times so we replaced all newline characters with a pseudo unique word: bobsnewline. 
# This word appears throughout the status update data used in the code below. 
# It does not appear in the LDA topics produced because it's not topic specific (use of newline should not be correlated with many topics)
# We opted to keep it in because we felt that use of punctuation might indicate personality traits that could be analysed separately. 

######################  SESSION 1 create topic table   ########################
#read tables
#read tables (in this version, I've already merged SWL with user posts, and included the date)
statusswl <- read.csv("time_status_swl.csv", header = T, fill=TRUE,row.names=NULL, stringsAsFactors = FALSE)
colnames(statusswl)<-c("X","user_id","swl","status","date")
#restrict to the one month preceding the SWL survey (which we assume is maximum date of the posts)
#this function finds the maximum post date for a user, then selects a month of posts. 
#The user can choose any month counting back from the maximum date using the mn variable. 
#Here we will only use '1', the most recent month. 
#But it could be useful to see how accuracy is affected by the month-period from 2 months or 3 months before the survey date.
fn<-function(ds,us){
  dsus<-subset(ds, user_id==us)
  usmax<-as.Date(max(dsus$date))
  lowerdate<-usmax
  day(lowerdate)<-day(usmax)-30 #subtracting exactly 30 days in each case
  upperdate<-usmax
  print(paste(lowerdate,upperdate))
  dsmn<-subset(dsus, date>=lowerdate & date <upperdate)
  output<-dsmn[,c("user_id","swl","status")]
}

#find the unique list of user ids
users<-unique(statusswl$user_id)
#create an empty data frame to record the output from the loop
statusswl1<-data.frame(user_id=character(),swl=integer(),status=character(),stringsAsFactors = FALSE)

#loop through each user, call the function that selects the 1 month data for that user, adding that result to the result data frame
for (u in users){
  statusswl1<-rbind(statusswl1,fn(statusswl,u))
}


swl_time<- read.csv("time_status_swl.csv", header = T, fill=TRUE,row.names=NULL)
swl<- read.csv("swl.csv", header = T, fill=TRUE,row.names=NULL)

#group status according to userid
statusswl2 <- aggregate(status~user_id, statusswl, FUN= paste, collapse=' ')

#clean data, replace smiley with words
smiles <- data.frame(s=c( ">:(", ":<", ">:", "=(", "=[", "='(",  "^_^",":)","=)","(=" ,"=]","^.^",":(",";)",";-)",":D","XD",":P",";P","<3"),
                     r=c("unhappyface","unhappyface","unhappyface","unhappyface","unhappyface","unhappyface","happyface","happyface","happyface","happyface","happyface","happyface","unhappyface","happyface","happyface","happyface","happyface","happyface","happyface","kiss"))

statusswl2$status %>%
        str_replace_all(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", " ")%>%
        str_replace_all("\\d", " ")%>%
        stri_replace_all_fixed(pattern = smiles$s,replacement = smiles$r,vectorize_all = FALSE)%>%
        str_replace_all("[^[:alnum:]']", " ")%>%
        str_replace_all('\\s\\b[^i]{1}\\s', " ")%>%
        gsub("\\s+", " ",.) -> statusswl2$status

#create dtm
clean2.corpus <- Corpus(VectorSource(statusswl2$status))

dtm2 <- DocumentTermMatrix(clean2.corpus)

#convert dtm into matrix
freqs <- as.data.frame(as.matrix(dtm2))

#load topic list 
topic2000 <- read.csv("2000_emo.csv", header = F, fill=F,row.names=NULL)
topic2001 <- t(topic2000)


#count word frequency according to the topic list, here shows how many topic words occur in each document
p4 <- vector()
for(i in 1:2000) {
        o1 <- vector()
        o1 <- freqs[c(1, match(topic2001[, i], names(freqs), nomatch=0))]
        if(length(o1) > 2) {
                topicCollumn = paste("topic", as.character(i), sep='')
                p4[topicCollumn] <- as.data.frame(rowSums(o1))
        }
}

topic.table <- as.data.frame(p4)
#add userid to p4

topic.id <- as.data.frame(statusswl2[,1])

topic.id[2:1982] <- topic.table[1:1981]
colnames(topic.id)[1] <- "userid"


####################################### SESSION 2 TOPIC TABLE WITH LIWC ####################################

#count the number of words in each user
#topic.id$count <- sapply(strsplit(statusswl2$status, "\\s+"), length)

#remove users have less than 100 words
#topic.id2 <- topic.id[topic.id$count>100,]
#topic.id2 <- topic.id2[complete.cases(topic.id2), ]

#join topics with LIWC data
liwc <- read.csv("liwc.csv", header = T, fill=TRUE,row.names=NULL)
liwc2 <- liwc
liwc2[2:65] <- scale(liwc[,2:65])



######################## SESSION 3 SENTIMENT ####################################


# import positive and negative words
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")
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


###count the number of status in each user
detach("package:plyr", unload=TRUE)
statusswl1 %>% #changed from statusswl to statusswl1 to apply this code to the one-month subset only
        group_by(user_id) %>%
        mutate(count = n()) %>%
        select(user_id,count) %>%
        rename(userid = user_id) %>%
        .[!duplicated(.$userid), ]-> status.count




###########clean data
statusswl1$status %>%
        #       as_vector() %>%
        str_replace_all(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", " ")%>%
        str_replace_all("\\d", " ")%>%
        stri_replace_all_fixed(pattern = smiles$s,replacement = smiles$r,vectorize_all = FALSE)%>%
        str_replace_all("[^[:alnum:]']", " ")%>%
        str_replace_all('\\s\\b[^i]{1}\\s', " ")%>%
        gsub("\\s+", " ",.) -> statusswl1$status

######compute sentiment score
result_all2 <- score.sentiment(statusswl1$status, pos, neg)

statusswl1.senti <- statusswl1
statusswl1.senti[3:4] <- result_all2[1:2]
colnames(statusswl1.senti)[3] <- "sentiment.score"
colnames(statusswl1.senti)[1] <- "userid"

#####join sentiment score with status count for each user

#do once to study the number of posts per user for the one month preceding SWL survey
swl_unq<-swl[!duplicated(swl$user),]
statusswl1.senti %>%
  select(userid, sentiment.score,text) %>%
  left_join(status.count, by = "userid") %>%
  left_join(swl_unq, by = "userid") -> statusswl1.senti2_all

hist(statusswl1.senti2_all$count,100)

statusswl1.senti2_all %>%
  filter(count > 20) -> statusswl1.senti2_filt

#mean sentiment score and swl score of each user + frequency of positive post and negative post
detach("package:plyr", unload=TRUE)
statusswl1.senti2_filt %>% 
  group_by(., userid) %>%
  mutate(neg.freq = sum(sentiment.score < 0)/count) %>%
  mutate(pos.freq = sum(sentiment.score > 0)/count) %>%
  mutate(pos.neg = as.numeric(sum(sentiment.score > 0)/sum(sentiment.score < 0))) %>%
  summarise(., mean.senti=mean(sentiment.score),mean.swl=mean(swl),pos.freq=mean(pos.freq),neg.freq=mean(neg.freq),pos.neg=mean(pos.neg))-> statusswl5

#variable correlation with swl
cor.test(statusswl5$neg.freq,statusswl5$mean.swl)
#-0.2325164  
# one month data (min 10 posts) -0.14 p=8.91e-06
# one month data (min 20 posts), cor=-0.22, p=1.07e-06

statusswl5$pos.neg[!is.finite(statusswl5$pos.neg)] <- NA
cor.test(statusswl5$pos.neg,statusswl5$mean.swl,use = "complete.obs")
#0.1603037
# one month data (min 10 posts) 0.11 p=0.0004
# one month data (min 20 posts), cor=0.165,p=0.0003

cor.test(statusswl5$pos.freq,statusswl5$mean.swl)
#0.07833762
#one month data (min 10 posts) cor=0.07898 p=0.01
#one month data (min 20 posts) cor=0.08, p=0.07

cor.test(statusswl5$mean.senti,statusswl5$mean.swl)
#0.2056476 
#one month data (min 10 posts) cor= 0.127 p=3.63e-05
#one month data(min 20 posts) cor=0.18, p=4.74e-05


#number of positive/negative score
sum(statusswl1.senti2_filt$sentiment.score > 0)
#184831
#one month data (min 10 posts) 8608
#one month data (min 20 posts) 5787

sum(statusswl1.senti2_filt$sentiment.score < 0)
#115915
#one month data (min 10 posts) 6364
#one month data (min 20 posts) 4375

sum(statusswl1.senti2_filt$sentiment.score == 0)
#194203
#One month data (min 10 posts) 11691
#One month data (min 20 posts) 8039

##correlation matrix of the sentiment variables, multicolinearity
statusswl5 %>%
        select(mean.swl,mean.senti,pos.freq,neg.freq,pos.neg)%>%
        cor(.,use = "complete.obs") -> cor.matrix



########################  SESSION 4 JOIN ALL FEATURES  ########################################
#merge all features
statusswl5 %>%
        select(userid,mean.swl,mean.senti,pos.freq,neg.freq,pos.neg)  %>%
        left_join(., topic.id, by = "userid") %>%
        left_join(., liwc2, by = "userid") %>%
        .[, colSums(is.na(.)) != nrow(.)] %>%
        .[complete.cases(.), ] %>%
        .[!duplicated(.$userid), ] -> allFeatures


###################################### SESSION 5 feature selection and prediction ########################
##2000 topics feature selection 

###compute matrix rank 

set.seed(666)
t5 <- allFeatures
ind = sample(2, nrow(t5), replace = TRUE, prob=c(0.7, 0.3))
trainset1 = t5[ind == 1,]
testset1 = t5[ind == 2,]

y = allFeatures$mean.swl
x = allFeatures[3:1988]
x <- as.matrix(x)
cv <- cv.glmnet(x,y,alpha=0.1)

coef(cv,s=0.1)
plot(cv)
cv$lambda.min
c <- coef(cv, s = "lambda.min")


#hand picked those == !0 as features
cv_score <- as.matrix(unlist(c))

#lasso prediction using lda topics
set.seed(666)
t5 <- allFeatures
ind = sample(2, nrow(t5), replace = TRUE, prob=c(0.7, 0.3))
trainset1 = t5[ind == 1,]
testset1 = t5[ind == 2,]

y = trainset1$mean.swl
x = as.matrix(trainset1[3:1985])
cv1 <- cv.glmnet(x,y,alpha=1)
c <- coef(cv1, s = "lambda.min")
cv_score <- as.matrix(unlist(c))

testsetx <- as.matrix(testset1[3:1985])
pred2 <- predict(cv1,testsetx,s=c(0.1,0.05,0.01))
cor.test(pred2[,2], testset1$mean.swl)
rmse(testset1$mean.swl,pred2[,2])
#0.2517533
# one month data, (min 10 posts) 0.193 p=0.0006, RMSE=1.38 (range for SWL in full one month dataset is 1.2 to 6.8)
# one month data (min 20 posts), 0.291, p=0.00017, RMSE=1.33



cor(allFeatures$mean.senti,allFeatures$mean.swl)


######################### SESSION 6 RANDOM FOREST PREDICTION MODEL #############################
set.seed(66236)
t5 <- allFeatures

ind = sample(2, nrow(t5), replace = TRUE, prob=c(0.7, 0.3))
trainset1 = t5[ind == 1,]
testset1 = t5[ind == 2,]


set.seed(23778672)
#model using sentiment, LDA topics, LIWC
fit1 <- cforest(mean.swl ~ mean.senti+pos.neg+neg.freq+topic588+topic1179+topic205+topic1411+topic700+topic605+topic48
                +topic555+topic939+topic855+topic253+topic1715+topic675+topic1440+topic1725+topic638
                +topic171+topic327+topic217+topic1530+topic51+topic1221+topic470+topic505+topic1819+topic379
                +topic1684+topic314+topic754+topic1873+topic38+topic15+topic126+topic815+topic1620+topic111
                +topic1808+topic1038+topic994+topic321+topic846+topic765+topic259+topic1316+topic1980
                +topic1573+topic812+topic1139+topic551+topic1964+topic1079+topic1590+topic1186+topic1095
                +topic1680+topic630+topic409+topic677+topic53+topic1801+topic479+topic1098+topic142+topic938
                +topic863+topic436+topic539+topic1041+topic1508+topic1543+topic950+topic3+topic1787+topic1540
                +topic1549+topic1171+topic603+topic179+topic1772+topic820+topic1235+topic1043+topic120+topic1625
                +topic610+topic1689+topic811+topic1938+topic425+topic1745+topic867+topic1376+topic23+topic1448
                +topic428+topic1575+topic667+topic1708+topic1478+topic150+topic704+topic1423+topic961+topic592
                +topic427+topic162+topic450+topic956+topic1113+topic569+topic1814+topic1010+topic353+topic1150
                +topic524+topic249+topic1886+topic22+topic90+topic455+topic823+topic708+topic1390+topic530
                +topic1252+we+article+work+home+leisure+number+discrep+anger+negemo+friend+negate+swear+hear
                ,data = trainset1,controls=cforest_unbiased(ntree=1000, mtry= 3))

testset1$Prediction1 <- predict(fit1, testset1, OOB=TRUE)
cor.test(testset1$Prediction1,testset1$mean.swl)
rmse(testset1$mean.swl, testset1$Prediction1)

#0.3622124
#0.3562766
# one month data (min 10 posts), cor = 0.17, p=0.0014, RMSE = 1.35
# one month data (min 20 posts), cor = 0.24, p=0.0033, RMSE = 1.41

# prediction using liwc

set.seed(6634258)
fit2 <- cforest(mean.swl ~ funct+pronoun+ppron+i+we+you+shehe+they+ipron+article+verb
                +auxverb+past+present+future+adverb+preps+conj+negate+quant+number+swear+social
                +family+friend+humans+affect+posemo+negemo+anx+anger+sad+cogmech+insight+cause
                +discrep+tentat+certain+inhib+incl+excl+percept+see+hear+feel+bio+body+health
                +sexual+ingest+relativ+motion+space+time+work+achieve+leisure+home+money+relig
                +death+assent+nonfl+filler,
                data = trainset1,controls=cforest_unbiased(ntree=1000, mtry= 2))

testset1$Prediction2 <- predict(fit2, testset1, OOB=TRUE)
cor.test(testset1$Prediction2,testset1$mean.swl)
rmse(testset1$mean.swl,testset1$Prediction2)
#0.25
#one month data (min 10 posts), cor = 0.24, p=1.39e-05, RMSE = 1.33
#one month data (min 20 posts), cor = 0.20, p=0.013, RMSE = 1.42

#prediction using LDA topics 

set.seed(2042237)
fit3 <- cforest(mean.swl ~ topic588+topic1179+topic205+topic1411+topic700+topic605+topic48
                +topic555+topic939+topic855+topic253+topic1715+topic675+topic1440+topic1725+topic638
                +topic171+topic327+topic217+topic1530+topic51+topic1221+topic470+topic505+topic1819+topic379
                +topic1684+topic314+topic754+topic1873+topic38+topic15+topic126+topic815+topic1620+topic111
                +topic1808+topic1038+topic994+topic321+topic846+topic765+topic259+topic1316+topic1980
                +topic1573+topic812+topic1139+topic551+topic1964+topic1079+topic1590+topic1186+topic1095
                +topic1680+topic630+topic409+topic677+topic53+topic1801+topic479+topic1098+topic142+topic938
                +topic863+topic436+topic539+topic1041+topic1508+topic1543+topic950+topic3+topic1787+topic1540
                +topic1549+topic1171+topic603+topic179+topic1772+topic820+topic1235+topic1043+topic120+topic1625
                +topic610+topic1689+topic811+topic1938+topic425+topic1745+topic867+topic1376+topic23+topic1448
                +topic428+topic1575+topic667+topic1708+topic1478+topic150+topic704+topic1423+topic961+topic592
                +topic427+topic162+topic450+topic956+topic1113+topic569+topic1814+topic1010+topic353+topic1150
                +topic524+topic249+topic1886+topic22+topic90+topic455+topic823+topic708+topic1390+topic530
                +topic1252,
                data = trainset1,controls=cforest_unbiased(ntree=1000, mtry= 3))

testset1$Prediction1 <- predict(fit3, testset1, OOB=TRUE)
cor.test(testset1$Prediction1,testset1$mean.swl)
rmse(testset1$mean.swl,testset1$Prediction1)
#0.3270915
#one month data, cor=0.15, p=0.008, RMSE=1.36
#one month data (min 20 posts), cor=0.14, p=0.09, RMSE=1.44

#prediction using sentiment + lda
set.seed(20247)
fit1 <- cforest(mean.swl ~ mean.senti+pos.neg+neg.freq+topic588+topic1179+topic205+topic1411+topic700+topic605+topic48
                +topic555+topic939+topic855+topic253+topic1715+topic675+topic1440+topic1725+topic638
                +topic171+topic327+topic217+topic1530+topic51+topic1221+topic470+topic505+topic1819+topic379
                +topic1684+topic314+topic754+topic1873+topic38+topic15+topic126+topic815+topic1620+topic111
                +topic1808+topic1038+topic994+topic321+topic846+topic765+topic259+topic1316+topic1980
                +topic1573+topic812+topic1139+topic551+topic1964+topic1079+topic1590+topic1186+topic1095
                +topic1680+topic630+topic409+topic677+topic53+topic1801+topic479+topic1098+topic142+topic938
                +topic863+topic436+topic539+topic1041+topic1508+topic1543+topic950+topic3+topic1787+topic1540
                +topic1549+topic1171+topic603+topic179+topic1772+topic820+topic1235+topic1043+topic120+topic1625
                +topic610+topic1689+topic811+topic1938+topic425+topic1745+topic867+topic1376+topic23+topic1448
                +topic428+topic1575+topic667+topic1708+topic1478+topic150+topic704+topic1423+topic961+topic592
                +topic427+topic162+topic450+topic956+topic1113+topic569+topic1814+topic1010+topic353+topic1150
                +topic524+topic249+topic1886+topic22+topic90+topic455+topic823+topic708+topic1390+topic530
                +topic1252,data = trainset1,controls=cforest_unbiased(ntree=1000, mtry= 3))

testset1$Prediction1 <- predict(fit1, testset1, OOB=TRUE)
cor.test(testset1$Prediction1,testset1$mean.swl)
rmse(testset1$mean.swl,testset1$Prediction1)
#0.3405363
#one month dataset (min 10 posts), cor=0.14, p=0.01, RMSE=1.36
#one month dataset (min 20 posts), cor=0.19, p=0.018, RMSE=1.42

############# SESSION 7 predict user behaviors with machine predict swl and self-report swl ###############

#this RF model use OOB to predict all sample, it is used in the later regression model ##############################################################
allFeature2 <- allFeatures
set.seed(2564235)
allFeature2$Prediction <- predict(cforest(mean.swl ~ pos.neg+neg.freq+topic588+topic1179+topic205+topic1411+topic700+topic605+topic48
                                          +topic555+topic939+topic855+topic253+topic1715+topic675+topic1440+topic1725+topic638
                                          +topic171+topic327+topic217+topic1530+topic51+topic1221+topic470+topic505+topic1819+topic379
                                          +topic1684+topic314+topic754+topic1873+topic38+topic15+topic126+topic815+topic1620+topic111
                                          +topic1808+topic1038+topic994+topic321+topic846+topic765+topic259+topic1316+topic1980
                                          +topic1573+topic812+topic1139+topic551+topic1964+topic1079+topic1590+topic1186+topic1095
                                          +topic1680+topic630+topic409+topic677+topic53+topic1801+topic479+topic1098+topic142+topic938
                                          +topic863+topic436+topic539+topic1041+topic1508+topic1543+topic950+topic3+topic1787+topic1540
                                          +topic1549+topic1171+topic603+topic179+topic1772+topic820+topic1235+topic1043+topic120+topic1625
                                          +topic610+topic1689+topic811+topic1938+topic425+topic1745+topic867+topic1376+topic23+topic1448
                                          +topic428+topic1575+topic667+topic1708+topic1478+topic150+topic704+topic1423+topic961+topic592
                                          +topic427+topic162+topic450+topic956+topic1113+topic569+topic1814+topic1010+topic353+topic1150
                                          +topic524+topic249+topic1886+topic22+topic90+topic455+topic823+topic708+topic1390+topic530
                                          +topic1252+we+article+work+home+leisure+number+discrep+anger+negemo+friend+negate+swear+hear,
                                          data = allFeature2,controls=cforest_unbiased(ntree=1000, mtry= 2)),OOB= TRUE)

cor(allFeature2$Prediction,allFeature2$mean.swl)
#0.3273324

########################SESSION 7 REGRESSION MODEL PREDICT USER BEHAVIOR ##############################

####merge with depression
allFeature2 %>% select(userid, mean.senti, Prediction, mean.swl,neg.freq,pos.freq) ->reg


dep <- read.csv("dep.csv", header = TRUE)
dep$sum <- rowSums(dep[8:27])

dep %>% 
        select(userid, sum) %>%
        right_join(., reg, by = "userid") %>%
        .[complete.cases(.$sum),]-> dep.reg
#454

cor.test(dep.reg$mean.swl,dep.reg$sum)
#-0.2594891

cor.test(dep.reg$sum,dep.reg$Prediction)
#-0.1950981 

cor.test(dep.reg$sum,dep.reg$neg.freq)


fit_dep <- lm(sum~ mean.swl, data=dep.reg)
summary(fit_dep)
#0.06733***

fit_dep <- lm(sum~ Prediction, data=dep.reg)
summary(fit_dep)
#0.0432***


set.seed(42451)
ind = sample(2, nrow(dep.reg), replace = TRUE, prob=c(0.5, 0.5))
trainset3 = dep.reg[ind == 1,]
testset3 = dep.reg[ind == 2,]

set.seed(6543)
fit_dep <- linearRidge(sum~ mean.senti+pos.freq+neg.freq, data=trainset3)
testset3$pred3<- predict(fit_dep,testset3)
cor.test(testset3$pred3, testset3$sum)
#0.199658 ***


set.seed(311456)
fit_dep <- linearRidge(sum~ mean.swl+mean.senti+pos.freq+neg.freq, data=trainset3)
summary(fit_dep)
testset3$pred3<- predict(fit_dep,testset3)
cor.test(testset3$pred3, testset3$sum)
#0.2407019  ***

set.seed(343556)
fit_dep <- linearRidge(sum~ Prediction+mean.senti+pos.freq+neg.freq, data=trainset3)
summary(fit_dep)
testset3$pred3<- predict(fit_dep,testset3)
cor.test(testset3$pred3, testset3$sum)
#0.2119759 ***


################################how many status contain less than 4 words?
status.word <- statusswl

status.word$count <- sapply(strsplit(status.word$status, "\\s+"), length)

sum(status.word$count <= 3)
# 50115

sum(status.word$count > 20)

#############################SESSION 8 TIME SERIES ############################
####ploting time 
detach("package:plyr", unload=TRUE)
statusswl.senti2 %>% select(sentiment.score,text) %>%
         rename(status = text) -> status


swl_time %>% select(userid, status, date) -> swl_time2

result_all3 <- score.sentiment(swl_time2$status, pos, neg)   

swl_time2.senti <- swl_time2
swl_time2.senti[4:5] <- result_all3[1:2]

swl_time2.senti$date <- as.Date(swl_time2.senti$date)
plot(swl_time2.senti$date,swl_time2.senti$score)

###plot sentiment
ggplot(swl_time2.senti, aes(date, score)) + geom_smooth()



#####distribution of number of posts by user
###count the number of status in each user
detach("package:plyr", unload=TRUE)
swl_time2.senti %>%
        group_by(userid) %>%
        mutate(count = n()) %>%
        select(userid,count,date) -> swl_time2.count.date

ggplot(swl_time2.count.date, aes(date, count)) + geom_smooth()