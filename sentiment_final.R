install.packages(c("qdap","plyr","stringr","tidyverse","stringi"))
require(qdap)
require(plyr)
require(stringr)
require(tidyverse)
require(stringi)

################session one: user sentiment score#######################
#merging tables
statusswl <- read.csv("user_all_status_swl2.csv", header = T, fill=TRUE,row.names=NULL)
swl<- read.csv("swl.csv", header = T, fill=TRUE,row.names=NULL)

#group status according to userid
#statusswl2 <- aggregate(status~user_id, statusswl, FUN= paste, collapse=' ')

#clean data, replace smiley with words
smiles <- data.frame(s=c( ">:(", ":<", ">:", "=(", "=[", "='(",  "^_^",":)","=)","(=" ,"=]","^.^",":(",";)",";-)",":D","XD",":P",";P","<3"),
                     r=c("unhappyface","unhappyface","unhappyface","unhappyface","unhappyface","unhappyface","happyface","happyface","happyface","happyface","happyface","happyface","unhappyface","happyface","happyface","happyface","happyface","happyface","happyface","kiss"))



###########
statusswl$status %>%
        #       as_vector() %>%
        str_replace_all(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", " ")%>%
        str_replace_all("\\d", " ")%>%
        stri_replace_all_fixed(pattern = smiles$s,replacement = smiles$r,vectorize_all = FALSE)%>%
        str_replace_all("[^[:alnum:]']", " ")%>%
        str_replace_all('\\s\\b[^i]{1}\\s', " ")%>%
        gsub("\\s+", " ",.) -> statusswl$status

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

####count the number of status in each user
detach("package:plyr", unload=TRUE)
statusswl %>%
        group_by(user_id) %>%
        mutate(count = n()) %>%
        select(user_id,count)%>%
        rename(userid = user_id) %>%
        .[!duplicated(.$userid), ]-> status.count

###sentiment score 

result_all2 <- score.sentiment(statusswl$status, pos, neg)

statusswl.senti <- statusswl
statusswl.senti[3:4] <- result_all2[1:2]
colnames(statusswl.senti)[3] <- "sentiment.score"
colnames(statusswl.senti)[2] <- "userid"

###filter user with less than 30 status
statusswl.senti %>%
        select(userid, sentiment.score,text) %>%
        left_join(status.count, by = "userid") %>%
        left_join(swl, by = "userid") %>%
        filter(count > 30) -> statusswl.senti2



#mean sentiment score ( for computing z score)
mean(statusswl.senti2$sentiment.score)
#0.2705754

sd(statusswl.senti2$sentiment.score)
#1.531071


#z = (X - μ) / σ


statusswl.senti2 %>%
        filter(str_detect(text, '(?=.*chat)(?=.*friend)|(?=.*talk)(?=.*friend)|(?=.*speak)(?=.*friend)|(?=.*tell)(?=.*friend)|(?=.*told)(?=.*friend)')) -> friend
mean(friend$sentiment.score)  
#0.8686594
# 

statusswl.senti2 %>%
        filter(str_detect(text, '(?=.*homework)|(?=.*assignment)|(?=.*paper)|(?=.*study)|(?=.*lecture)|(?=.*homework)|(?=.*assignment)|(?=.*exam)|(?=.*research)|(?=.*science)|(?=.*literature)(?=.*review)|(?=.*quiz)|(?=.*course)|(?=.*graduate)|(?=.*academic)|(?=.*assessment)|(?=.*assay)|(?=.*report)')) -> homework
mean(homework$sentiment) 
# 0.2267165
#z= -0.09


statusswl.senti2 %>%
        filter(str_detect(text, '(?=.*mathematics)|(?=.*maths)|(?=.*algebra)|(?=.*calculation)|(?=.*geometric)|(?=.*calculus)')) -> maths
mean(maths$sentiment) 
#0.04326923
#z=-0.1


statusswl.senti2 %>%
        filter(str_detect(text, '(?=.*my family)|(?=.*my daughter)|(?=.*my children)|(?=.*my child)|(?=.*my kid)|(?=.*my aunt)|(?=.*my uncle)|(?=.*my brother)|(?=.*my sister)|(?=.*my cousin)|(?=.*my grandpa)|(?=.*my grandma)|(?=.*my grandad)|(?=.*my mom)|(?=.*my dad)')) -> family
mean(family$sentiment) 
# 0.6827483
#z= 0.05


statusswl.senti2 %>%
        filter(str_detect(text, '(?=.*vacation)|(?=.*holiday)|(?=.*weekend)|(?=.*saturday)|(?=.*vacations)|(?=.*sunday)')) -> holiday
mean(holiday$sentiment) 
# 0.8663115
#z= 0.24


statusswl.senti2 %>%
        filter(str_detect(text, '(?=.*faith)|(?=.*church)|(?=.*pray)|(?=.*pastor)|(?=.*temple)|(?=.*monk)|(?=.*chapel)|(?=.*choir)|(?=.*spiritual)|(?=.*buddhist)')) -> religion
mean(religion$sentiment) 

#1.146911
#z=0.26

statusswl.senti2 %>%
        filter(str_detect(text, '(?=.*meal)|(?=.*dinner)|(?=.*dine)|(?=.*eat)|(?=.*cook)|(?=.*supper)|(?=.*snack)|(?=.*bread)|(?=.*cake)|(?=.*meat)|(?=.*pasta)|(?=.*rice)|(?=.*soup)|(?=.*steak)|(?=.*chicken)|(?=.*burger)|(?=.*sandwich)')) -> meal
mean(meal$sentiment) 
#0.7214254
##z=0.13



statusswl.senti2 %>%
        filter(str_detect(text, '(?=.*chores)|(?=.*housework)|(?=.*laundry)|(?=.*clean)|(?=.*dishwashing)|(?=.*grocery)|(?=.*weep)|(?=.*mop)|(?=.*vacuum)|(?=.*mow)')) -> chores
mean(chores$sentiment)
#0.03893108
#z=0.13

statusswl.senti2 %>%
        filter(str_detect(text, '(?=.*sick)|(?=.*pain)|(?=.*ill)|(?=.*fever)|(?=.*vomit)|(?=.*die)|(?=.*cold)')) -> sick
mean(sick$sentiment)
#-0.008822938
#z = -0.2

###interrater reliability of the topic wordlist we define
fb_act<- read.csv("fb_activities.csv", header = T, fill=TRUE,row.names=NULL)

kappa2(fb_act, "squared")

# Cohen's Kappa for 2 Raters (Weights: squared)
# 
# Subjects = 97 
# Raters = 2 
# Kappa = 0.361 
# 
# z = 4.57 
# p-value = 4.81e-06 



