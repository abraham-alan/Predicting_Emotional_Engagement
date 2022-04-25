getwd()
setwd("~/ALY_6980_Capstone")
library(readr)
Capstone <- read_csv("NE_PathosAI.as.csv",
        col_types = c("icffcffdccfffffffffffffffff"))
# now they are mostly factors, good for grouping
# convert data to date type
library(lubridate)
Capstone$Date <- mdy_hm(Capstone$Date)
######
names(Capstone)
summary(Capstone)

# 3 NAs in ID
Capstone <- Capstone[!is.na(Capstone$Id),]
colSums(is.na(Capstone))
# no NAs for emotion and driver or EE, positive terms has only 736 NAs and Neg terms has
# 5993, Dimensions has only 8750. The rest are all mostly NAs. Satisfied, Recommend,taste, 
# switched have over 1000 non-NAs.

table(Capstone$other_buy_not_buy)
#not that much info there

#check out freq of various dimensions
dd <- as.data.frame(table(Capstone$dimensions_detected))

summary(Capstone)

library(tidyverse)
# assign each emotion a number. Negative is -5:-1 positive is 1-4
Capstone <- Capstone %>% 
  mutate(Emotion_Value = if_else(Emotion == "disgust", -5,
                                 if_else(Emotion %in% "angry", -4,
                                 if_else(Emotion %in% "fear", -2, 
                                         if_else(Emotion %in% "sad", -1,
                                                 if_else(Emotion == "frustration", -3,
                                                         if_else(Emotion %in% "happy", 2,
          if_else(Emotion %in% "excited", 4,
          if_else(Emotion %in% "surprised", 3,0)))))))))
table(Capstone$Emotion_Value)

plot(Capstone$Date, Capstone$Emotion_Value)
sum(!is.na(Capstone$`recommend/not recommend`))
#1278 non-na's





table(Capstone$`recommend/not recommend`)

#create field for positive and neg. recommendations
Capstone$NotRecommended <-  str_detect(Capstone$`recommend/not recommend`,
                                    "avoid|cannot|not|never|wouldnt")
table(Capstone$NotRecommended)
Capstone$Recommended <- str_detect(Capstone$`recommend/not recommend`,
                                   "recommend|appreciated|positive|preferred")
table(Capstone$Recommended)

Capstone <- Capstone %>% 
  mutate(Recommend = if_else(NotRecommended == TRUE, -1,
                               if_else(Recommended == TRUE, 1,0)))
Capstone$Recommend[is.na(Capstone$Recommend)] <- 0

#remove extra columns
Capstone$NotRecommended <- NULL
Capstone$Recommended <- NULL
table(Capstone$Recommend)
qplot(Capstone$Recommend)

#create field for positive and neg. Satisfied
Capstone$Satisfied <- str_detect(Capstone$other_satisfied_not_satisfied,
                                 "best|stars|great|satis")
Capstone$Satisfied <- if_else(Capstone$Satisfied == TRUE,1,-1)
Capstone$Satisfied[is.na(Capstone$Satisfied)] <- 0
table(Capstone$Satisfied)

# #get separate df with just those that responded
Rec <- Capstone[Capstone$Recommend %in% c(0,1),]
# cor(Rec$Emotion_Value, Rec$Recommend)
# qplot(Rec$Emotion_Value, Rec$Recommend)
# 
# #look at correlation matrix for all numeric values
# cor(subset(Rec, select = c(`Emotional Engagement`, Emotion_Value, Recommend)))
# # high correlation of positive emotion = .58!!
# table(Rec$other_satisfied_not_satisfied)
# #create numeric variable for satisfied
# Rec$Satisfied <- str_detect(Rec$other_satisfied_not_satisfied, "best|stars|great|satis")
# Rec$Satisfied <- if_else(Rec$Satisfied == TRUE,1,0)

write.csv(Rec, file = "CapstoneAdjusted.csv")
write.csv(Capstone, file = "Capstone.New.csv")


# #let's combine the columns with multiple NAs
# library(tidyverse)
# Capstone9 <- Capstone %>% 
#   unite("combined", other_buy_not_buy:`recommend/not recommend`,
#         sep = "/",
#         na.rm = TRUE,
#         remove = FALSE)
# Capstone9 <- Capstone9[!(Capstone9$combined == ""),]
# colSums(is.na(Capstone))
# table(Capstone1$combined)
# summary(Capstone)
# Capstone1 <- Capstone %>% separate(dimensions_detected,
#                                     c("s","t","v" ,"w","x","y","z"), sep = "," )

lapply(Capstone[,12:26], unique)

#convert "other_buy..." to either 1 or 0
Capstone$other_buy_not_buy <- as.character(Capstone$other_buy_not_buy)
Capstone$other_buy_not_buy[is.na(Capstone$other_buy_not_buy)] <- "missing"
Capstone$buy <- if_else(Capstone$other_buy_not_buy == "missing",0,1)

table(Capstone$buy)
#worked!

table(Capstone$shape)
#too few data

table(Capstone$smell)
sum(!is.na(Capstone$smell))
#create field for positive and neg. Smell
Capstone$SmellGood <-  str_detect(Capstone$smell,
"good|wonderful|cake|vanilla|goodness|fresh|banana|pleasant|great|fine|sweeter|neutral|candy|pancake|breast|nice|better|milder")

table(Capstone$SmellGood)
Capstone <- Capstone %>% 
  mutate(SmellGood = if_else(SmellGood == TRUE, 1,
                             if_else(SmellGood == FALSE, -1,0)))
Capstone$SmellGood[is.na(Capstone$SmellGood)] <- 0

#promotion
table(Capstone$promotion)
#all seem positive so if not NA then assign a 1 otherwise assign 0
Capstone$Promotion <- if_else(is.na(Capstone$promotion), 0,1)
table(Capstone$Promotion)

# we can use for our model: taste, delivery, promotion, satisfied, recommend,smell,
table(Capstone$customer_journey_delivery)
# negative terms:expired damaged slow issue exposed late

Capstone$delivery <-str_detect(Capstone$customer_journey_delivery,
"expired|damaged|slow|issue|exposed|late")
table(Capstone$delivery)
Capstone$delivery <- if_else(Capstone$delivery == TRUE, 1,
                                              if_else(Capstone$delivery == FALSE,
                                                      -1, 0))
Capstone$delivery[is.na(Capstone$delivery)] <- 0
table(Capstone$delivery)

# now preprocess for taste
table(Capstone$taste)
#positive terms: okay like love better breastmilk great enjoy delicious
#nutrients good strawberry apple flavor chocolate prefer tasty sweet nice yummy berry 
#cake fresh banana grape fruit great mango peach best wonderful delightful
Capstone$Tastes <- str_detect(Capstone$taste,
"okay|like|love|better|breast|great|enjoy|delicious|nutrients|good|strawberry|apple|flavor|chocolate|prefer|tasty|sweet|nice|yummy|berry|cake|fresh|banana|grape|fruit|great|mango|peach|best|wonderful|delightful")
Capstone$Tastes <- if_else(Capstone$Tastes == TRUE, 1, -1)
Capstone$Tastes[is.na(Capstone$Tastes)] <- 0
table(Capstone$Tastes)

colnames(Capstone)
# subset for just relevant columns
Caps <- Capstone[,c(1:8,28:35,11)]

#extract features from Driver
table(Caps$Driver)
names(Caps)
Caps <-  Caps %>% separate(Driver,
                      c("a","b","c","d"), sep = "," )
lapply(Caps[,7:10], table)
a <- unique(Caps[["a"]])
b <- unique(Caps[["b"]])
c <- unique(Caps[["c"]])
d <- unique(Caps[["d"]])
abcd <- unique(c(a,b,c,d))
abcd
# "outcome"              "well_being"           "positivity"           "channel_satisfaction"
# "care"                 "value_for_money"      "convenience"          "fairness"            
# "freshness"            "timeliness"           "ease_of_access"       "packaging"           
# "waiting_time"         "trust"                "knowledge"                              
# only 15 items
?pivot_wider
vignette("pivot")
Caps1 <- Caps %>% pivot_wider(names_from = a, values_from = Id)
Caps2 <- Caps %>% pivot_wider(names_from = a, values_from = Id, values_fn = length)
Caps3 <- Caps %>% pivot_wider(names_from = b, values_from = Id, values_fn = length)
Caps4 <- Caps %>% pivot_wider(names_from = c, values_from = Id, values_fn = length)
Caps5 <- Caps %>% pivot_wider(names_from = d, values_from = Id, values_fn = length)
Caps2 <- Caps2[,19:33]
Caps3 <- Caps3[,19:32]
Caps4 <- Caps4[,19:28]
Caps5 <- Caps5[,19:22]
Caps1 <- Caps1[,1:18]
colSums(is.na(Caps5))
CapsAll <- cbind(Caps1,Caps2, Caps3, Caps4, Caps5)
#move dimensions detected to last row
CapsAll <- CapsAll[,c(1:17,19:61,18)]
names(CapsAll)
#combine and concantonate all common columns
CapsAll1 <- CapsAll %>% 
  unite("outcome", c(19,41),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)
CapsAll1 <- CapsAll1 %>% 
  unite("positivity", c(21,37,54),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)

CapsAll1 <- CapsAll1 %>% 
  unite("value_for_money", c(24,37,46,55),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)
CapsAll1 <- CapsAll1 %>% 
  unite("well_being", c(18,34,45,54),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)

CapsAll1 <- CapsAll1 %>% 
  unite("fairness", c(28,40),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)
CapsAll1 <- CapsAll1 %>% 
  unite("timeliness", c(26,37,44,50),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)
CapsAll1 <- CapsAll1 %>% 
  unite("ease_of_access", c(30,36,43),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)
CapsAll1 <- CapsAll1 %>% 
  unite("trust", c(32,34),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)
CapsAll1 <- CapsAll1 %>% 
  unite("knowledge", c(29,37,41),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)
CapsAll1 <- CapsAll1 %>% 
  unite("convenience", c(22,34),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)
CapsAll1 <- CapsAll1 %>% 
  unite("waiting_time", c(31,36,39),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)
CapsAll1 <- CapsAll1 %>% 
  unite("freshness", c(25,34,38),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)
CapsAll1 <- CapsAll1 %>% 
  unite("packaging", c(27,34,36),
        sep = "/",
        na.rm = TRUE)
names(CapsAll1)
#remove unneeded columns
CapsAll1[,33:35] <- NULL
names(CapsAll1)
CapsAll1[,6:8] <- NULL
table(CapsAll1$trust)
# missing NAs, blanks instead
lapply(CapsAll1,table)
colSums(is.na(CapsAll1))
#convert blank spaces to 0
CapsAll1[,15:29][CapsAll1[,15:29] == ""| CapsAll1[,15:29] == " "] <- 0
colSums(is.na(CapsAll1))
#convert to numeric
CapsAll1[,6:29] <- lapply(CapsAll1[,6:29], as.numeric)
#convert all 2s to 1s for consistency
CapsAll1[,13:27][CapsAll1[,13:27] == 2] <- 1

#creat df with complete cases "cc" for dimensions detected. Rows
# where no dimensions are detected have little value
CapsAllcc <- CapsAll1[!is.na(CapsAll1$dimensions_detected),]

#replace NAs
CapsAll1[,6:29][is.na(CapsAll1[,6:29])] <- 0
colSums(is.na(CapsAll1))
CapsAllcc[,6:29][is.na(CapsAllcc[,6:29])] <- 0
colSums(is.na(CapsAllcc))

# all columns now ready!!
#remove date and product
CapsAll1 <- CapsAll1[,-c(3:4)]
CapsAllcc <- CapsAllcc[,-c(3:4)]


#remove columns not involved in model
CapsAll1 <- CapsAll1[,-c(1,28)]
CapsAllcc <- CapsAllcc[,-c(1,28)]

#round EE to nearest whole number to enable fewer factor levels
CapsAllcc$`Emotional Engagement` <- round(CapsAllcc$`Emotional Engagement`)
#convert to factor
CapsAllcc$`Emotional Engagement` <- factor(CapsAllcc$`Emotional Engagement`)

lapply(CapsAll1, class)
#remove low frequency variables
#take out buy, wellbeing, trust, waiting time, ease_of, knowledge, fairness,packaging
CapsAllcc <- CapsAllcc[,-c(7,12,21:26)]
CapsAllcc$MANUFACTURER <- NULL
CapsAllcc$Emotion <- NULL

#let's see correlations
names(CapsAllcc)
library(corrplot)
CapsAllcc$`Emotional Engagement` <- as.numeric(CapsAllcc$`Emotional Engagement`)
corrplot(cor(CapsAllcc), method = 'shade')
cors <- sapply(CapsAllcc, cor, y =CapsAllcc$`Emotional Engagement`)
cors
# variables with correlation over .03
# Emotion_value
# Satisfied
# SmellGood
# delivery
# Promotion
# outcome
# value_for_money
# channel_satisfaction
# timeliness
# freshness
# convenience

#############
#make decision tree
tre.caps <- rpart(CapsAllcc$`Emotional Engagement`~ ., data= CapsAllcc)
tre.caps$cptable
plot(as.party(tre.caps), main = "Decision Tree for Emotional Engagement")
fancyRpartPlot(tre.caps, caption = "Decision Tree for Emotional Engagement") # uses emotion_value and value_for_money



#now create model

set.seed(1234)

sampling <- sample(c(TRUE, FALSE), nrow(CapsAllcc), replace = TRUE, prob = c(0.7, 0.3))
sample.caps.train <- CapsAllcc[sampling,]
sample.caps.test <- CapsAllcc[!sampling,]
names(sample.caps.train)
#create y variable
train.y <- sample.caps.train[,1]
test.y <- sample.caps.test[,1] 
# create x variables
train.x <- sample.caps.train[,-1]
test.x <- sample.caps.test[,-1]

library(cvms)
library(caret)
library(class)
library(RSNNS)

set.seed(1234)

for (i in 1:10) {
caret.knn <- knn(train.x, test.x, train.y ,
               k=i, use.all = FALSE)}
print(caret.knn)

RSNNS::confusionMatrix(caret.knn, test.y)
#        predictions
# targets    1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
#       4    0   0   0   1   0   1   0   0   0   0   0   0   0   0   0
#       5    0   0   0   0   2   5   1   1   6   2   1   0   1   0   0
#       6    0   0   1   2  17  54  28  22  38  15   4   2   0   0   0
#       7    0   0   1   1  10  34  33  13  33  11  10   3   0   1   0
#       8    0   0   1   0   6  13  18  66  82  19   5   4   0   0   0
#       9    1   2   3  15  25  78  68 273 611 107  30  15   3   1   1
#       10   0   0   0   0   3   9   9  12  23  31  10   6   2   0   0
#       11   0   0   0   1   2   1   6   6  10   5  10   3   0   0   0
#       12   0   0   0   0   0   0   0   0   2   0   0   0   0   0   0
# very poor


# #normalize for Deep NN
# train.x <- normalizeData(train.x)
# test.x <- normalizeData(test.x)
# train.y <- as.numeric(train.y)
# ?mlp
# set.seed(42)
# tic <- proc.time()
# digits.model <- mlp(as.matrix(train.x),
#                     train.y, 
#                     size = 40,             
#                     learnFunc = "Rprop",             
#                     shufflePatterns = FALSE,
#                     maxit = 80)
# print(proc.time() - tic)
# 
# plotIterativeError(digits.model)
# 
# yhat.caps <- predict(digits.model ,newdata= test.x)
# 
# table(yhat.caps)
# confusionMatrix(test.y ,yhat.caps)
####### did not work!

library(rpart)
library(randomForest)

rf_model <- caret::train(`Emotional Engagement`~.,
                  data = sample.caps.train,
                  method = "rf",
                  trControl= trainControl())

names(CapsAllcc)
table(sample.caps.train$`Emotional Engagement`)
# I need to remove levels 9 and 10 from the EE data becuase they are old factor levels
# are missing in the current substted sample, causing errors
CapsAllcc <- CapsAllcc[!CapsAllcc$`Emotional Engagement` %in% c(9,10),]
table(sample.caps.train$`Emotional Engagement`) #worked!
# need to convert to numeric to delete old factor levels and reconvert 
# to factor again
sample.caps.train$`Emotional Engagement` <- as.numeric(sample.caps.train$`Emotional Engagement`)
sample.caps.train$`Emotional Engagement` <- factor(sample.caps.train$`Emotional Engagement`)
rf_model$results

# mtry     RMSE    Rsquared     MAE
#    2 1.445880 0.14285315 1.074297  
plot(varImp(rf_model))

#############
# multiple linear regression
sample.caps.train$`Emotional Engagement` <-
  as.numeric(sample.caps.train$`Emotional Engagement`)

mlr.model <- lm(`Emotional Engagement`~., data = sample.caps.train)

summary(mlr.model)
# round results to better compare with EE
yhat <- round(predict(mlr.model, test.x, type = 'response'))
table(yhat,test.y)
# very poor. predicted way too many 8s and 9s
library(ISLR)
install.packages("ISLR")
ISLR::