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

#aggregate status updates according to userid (3324 documents)
fb_s <- aggregate(status~user_id, swl_status2, FUN= paste, collapse=' ')
#select status
ss<- as.vector(fb_s$status)
#clean data
ss <- tolower(ss) 
ss <- gsub("\\d", "", ss)
ss <- gsub("\\W", " ", ss)
ss <- gsub("[[:punct:]]"," ", ss)
ss <- gsub("bobsnewline", "", ss)

#turn status into corpus
ss <-  Corpus(VectorSource(ss))

#create dtm1 for status
dtm1 <- create_matrix(ss, language="english", removeNumbers=TRUE, weighting=weightTfIdf)
dtm1<- removeSparseTerms(dtm1, .99)

#turn dtm1 into a frequency table
freqs <- as.data.frame(inspect(dtm1))

#join the frequency table with userid
t2[2:8000] <- freqs[1:7999]

#we dont need userid in the next step, so remove them
t3 <- t2[3:8000]

#load term table from another journal paper
topic2000 <- read.csv("2000.csv", header = F, fill=F,row.names=NULL)
topic2001 <- t(topic2000)


#find the word frequency in each topic according to the words on the term table
#there are 20 words under each topic, each topic will have a topic score which is the sum of the 20 words
p4 <- vector()
for(i in 1:2000) {
        o1 <- vector()
        o1 <- t3[c(1, match(topic2001[, i], names(t3), nomatch=0))]
        if(length(o1) > 2) {
                topicCollumn = paste("topic", as.character(i), sep='')
                p4[topicCollumn] <- as.data.frame(rowSums(o1))
        }
}

#add userid to p4, t6 is a table with 1977 topic scores for each user

t6 <- as.data.frame(t2[,1])

t6[2:1978] <- p4[1:1977]
names(t6)[names(t6) == "t2[, 1]"] <- "userid"

#merging swl score with topic score table
swl2 <- merge(swl, swl_count, by.x="userid", by.y="userid", all.x=TRUE)
swl2 <- swl2[complete.cases(swl2),]
swl_topic <- merge(swl2, t6, by.x="userid", by.y="userid", all.x=TRUE)

#remove users have less than 40 status updates
swl_topic <- swl_topic[swl_topic$count>40,]

#some userid did the swl test twice, so I get the mean swl score from them
agg_swl <- aggregate(swl ~userid, data=swl_topic, mean)

#remove the extra swl column and merge the aggregated swl scores with topic score table
swl_topic$swl<- NULL
swl_topic  <- merge(agg_swl, swl_topic, by.x="userid", by.y="userid", all.x=TRUE)

#remove the duplicated id
swl_topic <- swl_topic[!duplicated(swl_topic),]

#merge with LIWC data
liwc <- read.csv("liwc.csv", header = T, fill=TRUE,row.names=NULL)
swl_topic2 <- merge(swl_topic, liwc, by.x="userid", by.y="userid", all.x=TRUE)


#load sentiment score
swl_senti_emo<- read.csv("swl_senti_emo.csv", header = T, fill=TRUE,row.names=NULL)

#select columns from sentiment score
keeps <- c("user_id","senti_all2")
swl_senti_emo2 <- swl_senti_emo[keeps]


#merge topic, liwc with sentiment score
swl_topic2 <- merge(swl_topic2, swl_senti_emo2, by.x="userid", by.y="user_id", all.x=TRUE)
swl_topic2$swl.x <- NULL
swl_topic2$swl <- unlist(swl_topic2$swl)


#swl_topic2[,5:2046]<-scale(swl_topic2[,5:2046]) no need to do scaling in RF
swl_topic2[,2046] <- NULL

library(glmnet)
##2000 topics feature selection 
y = swl_topic2$swl
x = swl_topic2[5:1981]
x <- as.matrix(x)
cv <- cv.glmnet(x,y,alpha=1)

coef(cv,s=0.1)
plot(cv)
cv$lambda.min
c <- coef(cv, s = "lambda.min")

#here I hand picked those == !0 as features
cv_score <- as.matrix(unlist(c))

#setting trainset and testset
t5 <- swl_topic2
ind = sample(2, nrow(t5), replace = TRUE, prob=c(0.7, 0.3))
trainset1 = t5[ind == 1,]
testset1 = t5[ind == 2,]


#let's see the ACC of the trainset
#trainset1$Prediction1 <- predict(fit1,OOB=TRUE)
#cor(trainset1$Prediction1,trainset1$swl)

#0.358

#we can try lasso prediction # 0.31
library(glmnet)
set.seed(999)
y = trainset$swl
x = trainset[5:2046]
x <- as.matrix(x)
cv1 <- cv.glmnet(x,y,alpha=1)

coef(cv1,s=0.1)
plot(cv1)
cv1$lambda.min
c1 <- coef(cv1, s = "lambda.min")

testsetx <- as.matrix(testset[5:2046])
pred2 <- predict(cv1,testsetx,s=c(0.1,0.05,0.01))
cor.test(pred2[,2], testset$swl)

#0.24

library(party)
#affect+LDA topic
set.seed(678)
fit1 <- cforest(swl ~  senti_all2 
                + topic714+topic48+topic205+topic605+topic253+topic1234+topic656+topic151+topic1186+topic217+topic1835
                +topic700+topic173+topic1915+topic15+topic939+topic1071+topic1283+topic1859+topic1862+topic111+topic1834
                +topic470+topic815+topic259+topic409+topic701+topic435+topic812+topic1669+topic1888+topic1873+topic1684
                +topic505+topic797+topic752+topic116+topic1216+topic1064+topic1310+topic769+topic736+topic155+topic1046
                +topic1242+topic346+topic321
                +topic162+topic961+topic1718+topic950+topic667+topic249+topic1575+topic1111+topic1549+topic1681
                +topic448+topic1525+topic1393+topic988+topic1030+topic1861+topic1423+topic1540+topic275+topic1656
                +topic1150+topic1297+topic403+topic577+topic23+topic144+topic430+topic835+topic1903+topic1126+topic1235
                +topic517+topic1723+topic595+topic1366+topic1519+topic1886+topic1625+topic353+topic1201+topic926
                +topic1802+topic708+topic1659+topic809+topic641+topic175+topic1909+topic734,
                data = trainset1,controls=cforest_unbiased(ntree=500, mtry= 1))


testset1$Prediction1 <- predict(fit1, testset1,OOB=TRUE)
cor(testset1$Prediction1,testset1$swl)

#0.39


#only LIWC
set.seed(8)
fit1 <- cforest(swl ~  affect + negemo + future+swear+sad+anger+negate+sexual+death + filler+leisure+ posemo,
                data = trainset1,controls=cforest_unbiased(ntree=500, mtry= 1))


testset1$Prediction1 <- predict(fit1, testset1,OOB=TRUE)
cor(testset1$Prediction1,testset1$swl)

#0.25

#affect only
set.seed(333)
fit1 <- cforest(swl ~  senti_all2,
                data = trainset1,controls=cforest_unbiased(ntree=500, mtry= 1))


testset1$Prediction1 <- predict(fit1, testset1,OOB=TRUE)
cor(testset1$Prediction1,testset1$swl)
#0.17

#LDA topic only 
set.seed(456)


fit1 <- cforest(swl ~  
                + topic714+topic48+topic205+topic605+topic253+topic1234+topic656+topic151+topic1186+topic217+topic1835
                +topic700+topic173+topic1915+topic15+topic939+topic1071+topic1283+topic1859+topic1862+topic111+topic1834
                +topic470+topic815+topic259+topic409+topic701+topic435+topic812+topic1669+topic1888+topic1873+topic1684
                +topic505+topic797+topic752+topic116+topic1216+topic1064+topic1310+topic769+topic736+topic155+topic1046
                +topic1242+topic346+topic321
                +topic162+topic961+topic1718+topic950+topic667+topic249+topic1575+topic1111+topic1549+topic1681
                +topic448+topic1525+topic1393+topic988+topic1030+topic1861+topic1423+topic1540+topic275+topic1656
                +topic1150+topic1297+topic403+topic577+topic23+topic144+topic430+topic835+topic1903+topic1126+topic1235
                +topic517+topic1723+topic595+topic1366+topic1519+topic1886+topic1625+topic353+topic1201+topic926
                +topic1802+topic708+topic1659+topic809+topic641+topic175+topic1909+topic734,
                data = trainset1,controls=cforest_unbiased(ntree=500, mtry= 1))


testset1$Prediction1 <- predict(fit1, testset1,OOB=TRUE)
cor(testset1$Prediction1,testset1$swl)
#0.36

#
set.seed(13)
fit1 <- cforest(swl ~  
                +mytopic1+mytopic2+mytopic3+mytopic4+mytopic5+mytopic6,
                data = trainset1,controls=cforest_unbiased(ntree=500, mtry= 1))


testset1$Prediction1 <- predict(fit1, testset1,OOB=TRUE)
cor(testset1$Prediction1,testset1$swl)

##topic only + affect
set.seed(45)
results<-numeric(0)
for (i in seq(1,5)){
        t5 <- swl_topic2
        ind = sample(2, nrow(t5), replace = TRUE, prob=c(0.7, 0.3))
        trainset1 = t5[ind == 1,]
        testset1 = t5[ind == 2,]
        
        fit1 <- cforest(swl ~  senti_all2
                        + topic714+topic48+topic205+topic605+topic253+topic1234+topic656+topic151+topic1186+topic217+topic1835
                        +topic700+topic173+topic1915+topic15+topic939+topic1071+topic1283+topic1859+topic1862+topic111+topic1834
                        +topic470+topic815+topic259+topic409+topic701+topic435+topic812+topic1669+topic1888+topic1873+topic1684
                        +topic505+topic797+topic752+topic116+topic1216+topic1064+topic1310+topic769+topic736+topic155+topic1046
                        +topic1242+topic346+topic321
                        +topic162+topic961+topic1718+topic950+topic667+topic249+topic1575+topic1111+topic1549+topic1681
                        +topic448+topic1525+topic1393+topic988+topic1030+topic1861+topic1423+topic1540+topic275+topic1656
                        +topic1150+topic1297+topic403+topic577+topic23+topic144+topic430+topic835+topic1903+topic1126+topic1235
                        +topic517+topic1723+topic595+topic1366+topic1519+topic1886+topic1625+topic353+topic1201+topic926
                        +topic1802+topic708+topic1659+topic809+topic641+topic175+topic1909+topic734,
                        data = trainset1,controls=cforest_unbiased(ntree=500, mtry= 1))


        testset1$Prediction1 <- predict(fit1, testset1,OOB=TRUE)
        cor(testset1$Prediction1,testset1$swl)
        results<-c(results,cor)

}
#0.40, 0.39 0.37, 0.42, 0.35 ,0.38 ,0.34 ,0.38 ,0.41, 0.36


#predictive validity
#criteria data merge with testset1  789 cases

#regression table 4 
#hierarchical regression
#FB activities, 57 cases
act <- read.csv("freq.csv", header = TRUE)
swl<- read.csv("swl.csv", header = TRUE)
act_objective <- merge(act, swl, by.x="userid", by.y="userid", all.x=TRUE)
act_objective <- act_objective[complete.cases(act_objective),]
act_objective <- act_objective[-11]
#merge table with a separate testset
act_objective <- merge(act_objective, testset1, by.x="userid", by.y="userid", all.x=TRUE)
act_objective <- act_objective[complete.cases(act_objective),]

#predict number of likes
fit <- lm(n_like~ swl, data=act_objective)
summary(fit)
#0.18
#0.36

fit <- lm(n_like~ swl+senti_all2, data=act_objective)
summary(fit)
#0.18
#0.36

fit <- lm(n_like~ Prediction1, data=act_objective)
summary(fit)
#0.26
#0.31

fit <- lm(n_like~ Prediction1+senti_all2, data=act_objective)
summary(fit)
#0.28
#0.34

# ngroup
fit <- lm(n_group~ swl, data=act_objective)
summary(fit)
#0.06

#predict number of FB groups
fit <- lm(n_group~ swl+senti_all2, data=act_objective)
summary(fit)
#0.07 NG

fit <- lm(n_group~ Prediction1, data=act_objective)
summary(fit)
#0.14 

fit <- lm(n_group~ Prediction1+senti_all2, data=act_objective)
summary(fit)
#0.15

#n_diads
fit <- lm(n_diads~ swl, data=act_objective)
summary(fit)
#0.27

fit <- lm(n_diads~ swl+senti_all2, data=act_objective)
summary(fit)
#0.29

fit <- lm(n_diads~ Prediction1, data=act_objective)
summary(fit)
#0.23

fit <- lm(n_diads~ Prediction1+senti_all2, data=act_objective)
summary(fit)
#0.24

#depression   116 cases
dep <- read.csv("dep.csv", header = TRUE)
dep$sum <- rowSums(dep[8:27])
dep <- dep[!duplicated(dep$userid), ]
dep1 <- dep[,c("userid","sum")]
act_objective <- merge(testset1, dep1, by.x="userid", by.y="userid", all.x=TRUE)
act_objective <- act_objective[complete.cases(act_objective),]

#predict depression score with SWL
fit <- lm(sum~ swl, data=act_objective)
summary(fit)
#0.08
#0.01 NG

##predict depression score with SWL+affect
fit <- lm(sum~ swl+senti_all2, data=act_objective)
summary(fit)
#0.09
#0.03 NG

##predict depression score with machine SWL
fit <- lm(sum~ Prediction1, data=act_objective)
summary(fit)
#0.10
#0.06 p<0.01

##predict depression score with machine SWL+affect
fit <- lm(sum~ Prediction1+senti_all2, data=act_objective)
summary(fit)
#0.10
#0.06  p < 0.05

fit <- lm(sum~ Prediction1+ swl + senti_all2, data=act_objective)
summary(fit)
#0.06  NG


#PILL #275 cases
pill <- read.csv("pill.csv", header = TRUE)
act_objective <- merge(testset1, pill, by.x="userid", by.y="userid", all.x=TRUE)
act_objective <- act_objective[complete.cases(act_objective),]

#predict PILL with SWL
fit <- lm(PILL~ swl, data=act_objective)
summary(fit)
#0.09

##predict PILL with SWL+affect
fit <- lm(PILL~ swl+senti_all2, data=act_objective)
summary(fit)
#0.09

##predict PILL with machine SWL
fit <- lm(PILL~ Prediction1, data=act_objective)
summary(fit)
#0.07

###predict PILL with machine SWL+affect
fit <- lm(PILL~ Prediction1+ senti_all2, data=act_objective)
summary(fit)
#0.08

#combining machine predicted subjective well-being profile and self-reported SWL can better predict life outcome
##predict PILL with self-reported SWL and machine reported SWL
fit <- lm(PILL~ Prediction1+ swl + senti_all2, data=act_objective)
summary(fit)
#0.12

#self-disclosure #317
sdfm <- read.csv("sdfm.csv", header = TRUE)

act_objective <- merge(testset1, sdfm, by.x="userid", by.y="userid", all.x=TRUE)
act_objective <- act_objective[complete.cases(act_objective),]
act_objective$sd_score <- as.numeric(act_objective$sd_score)

#predict self-disclosure with SWL
fit <- lm(sd_score ~ swl, data=act_objective)
summary(fit)
#0.013 NG

##predict self-disclosure with SWL+affect
fit <- lm(sd_score~ swl+senti_all2, data=act_objective)
summary(fit)
#0.02  p <0.05

##predict self-disclosure with machine SWL
fit <- lm(sd_score~ Prediction1, data=act_objective)
summary(fit)
#0.02  p<0.05

###predict self-disclosure with machine SWL+affect
fit <- lm(sd_score~ Prediction1+ senti_all2, data=act_objective)
summary(fit)
#0.02  p<0.05

#predict fair-mindedness 
act_objective$fm_score <- as.numeric(act_objective$fm_score)
#predict self-disclosure with SWL
fit <- lm(fm_score ~ swl, data=act_objective)
summary(fit)
#0.04

##predict self-disclosure with SWL+affect
fit <- lm(fm_score~ swl+senti_all2, data=act_objective)
summary(fit)
#0.05

##predict self-disclosure with machine SWL
fit <- lm(fm_score~ Prediction1, data=act_objective)
summary(fit)
#0.04

###predict self-disclosure with machine SWL+affect
fit <- lm(sd_score~ Prediction1+ senti_all2, data=act_objective)
summary(fit)
#0.02 *


##big five #739
big5 <- read.csv("big5.csv", header = TRUE)
act_objective <- merge(testset1, big5, by.x="userid", by.y="userid", all.x=TRUE)
act_objective <- act_objective[complete.cases(act_objective),]

#predict ope with SWL
fit <- lm(ope~ swl, data=act_objective)
summary(fit)
#0.01   **

##predict ext with SWL+affect
fit <- lm(ext ~ swl+senti_all2, data=act_objective)
summary(fit)
#0.02  **

##predict ope with machine SWL
fit <- lm(ope ~ Prediction1, data=act_objective)
summary(fit)
#0.01  NG

###predict ope with machine SWL+affect
fit <- lm(ext ~ Prediction1+ senti_all2, data=act_objective)
summary(fit)
#0.01  NG

#predict neu 
#predict ope with SWL
fit <- lm(neu~ swl, data=act_objective)
summary(fit)
#0.26  

##predict ope with SWL+affect
fit <- lm(neu ~ swl+senti_all2, data=act_objective)
summary(fit)
#0.26  

##predict ope with machine SWL
fit <- lm(neu ~ Prediction1, data=act_objective)
summary(fit)
#0.06  

###predict ope with machine SWL+affect
fit <- lm(neu ~ Prediction1+ senti_all2, data=act_objective)
summary(fit)
#0.06  

##predit agr
#predict agr with SWL
fit <- lm(agr~ swl, data=act_objective)
summary(fit)
#0.10  

##predict agr with SWL+affect
fit <- lm(agr ~ swl+senti_all2, data=act_objective)
summary(fit)
#0.13

##predict agr with machine SWL
fit <- lm(agr ~ Prediction1, data=act_objective)
summary(fit)
#0.06  *

###predict agr with machine SWL+affect
fit <- lm(neu ~ Prediction1+ senti_all2, data=act_objective)
summary(fit)
#0.06 *