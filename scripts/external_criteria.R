#merging swl score with topic score table
swl <- read.csv("swl.csv", header = T, fill=TRUE,row.names=NULL)
swl_count <- read.csv("swl_count.csv", header = T, fill=TRUE,row.names=NULL)
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

#merging swl score with topic score table
swl <- read.csv("swl.csv", header = T, fill=TRUE,row.names=NULL)
swl_count <- read.csv("swl_count.csv", header = T, fill=TRUE,row.names=NULL)
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


##### external criterion 
require(party)


set.seed(445)

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
testset1$swl <- as.numeric(testset1$swl)
testset1$Prediction1  <- as.numeric(testset1$Prediction1)
cor <- cor(testset1$Prediction1,testset1$swl)

fea_np2$Prediction1 <- predict(fit1, fea_np2,OOB=TRUE)
fea_np2 <- fea_np2[!duplicated(fea_np2$userid), ]
fea_np2$ope <- as.numeric(fea_np2$ope)
fea_np2$con <- as.numeric(fea_np2$con)
fea_np2$ext <- as.numeric(fea_np2$ext)
fea_np2$agr <- as.numeric(fea_np2$agr)
fea_np2$neu <- as.numeric(fea_np2$neu)
fea_np2$Prediction1  <- as.numeric(fea_np2$Prediction1)

#fea_np2 <- merge(fea_np2, t6, by.x="userid", by.y="userid", all.x=TRUE)

cor1 <- cor.test(fea_np2$Prediction1,fea_np2$ope)
cor2 <- cor.test(fea_np2$Prediction1,fea_np2$con)
cor3 <- cor.test(fea_np2$Prediction1,fea_np2$ext)
cor4 <- cor.test(fea_np2$Prediction1,fea_np2$agr)
cor5 <- cor.test(fea_np2$Prediction1,fea_np2$neu)


cor1 <- cor.test(fea_np2$swl,fea_np2$ope)
cor2 <- cor.test(fea_np2$swl,fea_np2$con)
cor3 <- cor.test(fea_np2$swl,fea_np2$ext)
cor4 <- cor.test(fea_np2$swl,fea_np2$agr)
cor5 <- cor.test(fea_np2$swl,fea_np2$neu)

#depression
#machine
dep <- read.csv("dep.csv", header = TRUE)
dep$sum <- rowSums(dep[8:27])
dep <- dep[!duplicated(dep$userid), ]
dep1 <- dep[,c("userid","sum")]
fea_np3 <- merge(fea_np2, dep1, by.x="userid", by.y="userid", all.x=TRUE)
fea_np3 <- fea_np3[complete.cases(fea_np3),]

cor(fea_np3$Prediction1,fea_np3$sum)

#self-disclosure

sdfm <- read.csv("sdfm.csv", header = TRUE)
sd_swl <- merge(sdfm, fea_np2, by.x="userid", by.y="userid", all.x=TRUE)
sd_swl <- sd_swl[complete.cases(sd_swl ),]
sd_swl <- sd_swl[!duplicated(sd_swl),]
sd_swl$sd_score <- as.numeric(sd_swl$sd_score)
sd_swl$fm_score <- as.numeric(sd_swl$fm_score)
cor(sd_swl$Prediction1,sd_swl$sd_score)
cor(sd_swl$Prediction1,sd_swl$fm_score)


#egocentric network
ego <- read.csv("ego.csv", header = TRUE)
ego_swl <- merge(ego, fea_np2, by.x="userid", by.y="userid", all.x=TRUE)
ego_swl <- ego_swl[complete.cases(ego_swl),]
ego_swl$density <- as.numeric(ego_swl$density)
cor.test(ego_swl$Prediction1,ego_swl$density)
cor.test(ego_swl$Prediction1,ego_swl$betweenness)
cor.test(ego_swl$Prediction1,ego_swl$network_size)


#PILL
pill <- read.csv("pill.csv", header = TRUE)
pill_swl <- merge(pill, fea_np2, by.x="userid", by.y="userid", all.x=TRUE)
pill_swl <- pill_swl[complete.cases(pill_swl),]
pill_swl$PILL <- as.numeric(pill_swl$PILL)

cor.test(pill_swl$PILL,pill_swl$Prediction1)
#-0.25

#Activities
act <- read.csv("freq.csv", header = TRUE)
act_swl <- merge(act, fea_np2, by.x="userid", by.y="userid", all.x=TRUE)
act_swl <- act_swl[complete.cases(act_swl),]
act_swl$n_like <- as.numeric(act_swl$n_like)
act_swl$n_status <- as.numeric(act_swl$n_status)

cor.test(act_swl$n_like,act_swl$Prediction1)
cor.test(act_swl$n_status,act_swl$Prediction1)
cor.test(act_swl$n_event,act_swl$Prediction1)
cor.test(act_swl$n_group,act_swl$Prediction1)
cor.test(act_swl$n_tags,act_swl$Prediction1)
cor.test(act_swl$n_diads,act_swl$Prediction1)


