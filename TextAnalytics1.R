#Rowan Vasquez

#What types of research are most likely to receive higher levels of funding?
#Let's use text analytics to analyze the words in the research abstracts

#The dataset utilized for these visualizations is available at the following URL:
#http://www.research.gov/research-portal/appmanager/base/desktop?_nfpb=true&_eventName=viewQuickSearchFormEvent_so_rsr


library(tm)
library(SnowballC)
library(caTools) #for splitting dataset
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(reshape) #change var names


data.dir <- "/Users/rowanvasquez/Documents/Personal\ Research\ Projects/Datasets/" 
rs.file <- paste0(data.dir, "ResearchAwards-2013.csv") 

if(FALSE){
rs <- read.csv(rs.file, stringsAsFactors = FALSE)

str(rs)
summary(rs)
#head(rs.full)
rs.save.file <- paste0(data.dir, "ResearchAwards-2013-full.Rda" )

save(rs, file = rs.save.file)
}



rs.save.file <- paste0(data.dir, "ResearchAwards-2013-full.Rda" )

load(rs.save.file)




#######################
#Cleaning the Variables
######################

#renaming vars

rs <- rename(rs, c(Abstract.at.Time.of.Award = "Abstract"))
rs <- rename(rs, c(Estimated.Total.Award.Amount = "Amount"))

str(rs)

#removing everything that is not a digit
rs$Amount <- gsub('\\D', '', rs$Amount)

head(rs$Amount)

rs[,c("Amount")]<- as.numeric(rs[,c("Amount")])

range(rs$Amount, na.rm = TRUE)
mean(rs$Amount, na.rm= TRUE)




####################
#Creating the Corpus
####################

ab.corpus <- Corpus(VectorSource(rs$Abstract))
ab.corpus[[1]]

#convert to lowercase
ab.corpus <- tm_map(ab.corpus, tolower)

#remove punctuation
ab.corpus <- tm_map(ab.corpus, removePunctuation)

#remove stop words
ab.corpus <- tm_map(ab.corpus, removeWords, stopwords("english"))

#stem words
ab.corpus <- tm_map(ab.corpus, stemDocument)
ab.corpus[[1]]

###############
#Bag of Words
###############

ab.frequencies <- DocumentTermMatrix(ab.corpus)
ab.corpus

ab.sparse <- removeSparseTerms(ab.frequencies, 0.90)
#Keep terms that appear in 5% or more of the abstracts

ab.sparse

ab.sparse <- as.data.frame(as.matrix(ab.sparse))

#make column names appropriate for R
colnames(ab.sparse) <- make.names(colnames(ab.sparse))



#Add DV
ab.sparse$Amount <- rs$Amount

str(ab.sparse)
summary(ab.sparse)

ab.sparse.save.file <- paste0(data.dir, "ResearchAwards-2013-ab-sparse.Rda" )

save(ab.sparse, file = ab.sparse.save.file)


ab.sparse.save.file <- paste0(data.dir, "ResearchAwards-2013-ab-sparse.Rda" )

load(ab.sparse.save.file)

str(ab.sparse)

#Removing NA values
ab.sparse <- na.omit(ab.sparse)

summary(ab.sparse$Amount)


#Creating a new DV that describes whether or not the research received a high amount of funding
#(i.e. it is in the upper decile)
upper.quart.amount <-  quantile(ab.sparse$Amount, prob=c(0.90)) 
upper.quart.amount
ab.sparse$highAmount <- as.numeric(ab.sparse$Amount >= upper.quart.amount)
table(ab.sparse$highAmount)



#split into training and testing set
set.seed(23232)
ab.sparse.split <- sample.split(ab.sparse$highAmount, SplitRatio = 0.7) 
ab.sparse.train <- subset(ab.sparse, ab.sparse.split == TRUE)
ab.sparse.test <- subset(ab.sparse, ab.sparse.split == FALSE)

str(ab.sparse.train)
str(ab.sparse.test)

##################
#Models
#################


#Baseline Model
#No research project is a highly funded project
baseline.accur <- table(ab.sparse.test$highAmount)[1] / (table(ab.sparse.test$highAmount)[1] + table(ab.sparse$highAmount)[2])
baseline.accur
#72.5%

#Logit1


logit1 <- glm(highAmount~.-Amount, data = ab.sparse.train, family = "binomial")
summary(logit1)
pred.logit1 <- predict(logit1, newdata = ab.sparse.test)

summary(pred.logit1)

confus.logit1 <- confusionMatrix(as.numeric(pred.logit1 >= 0.50), ab.sparse.test$highAmount )
confus.logit1

accur.logit1.improv <- confus.logit1$overall[1] - baseline.accur 
accur.logit1.improv
#17.12% improvement


#CART Model
table(ab.sparse.train$highAmount)

cart1 <- rpart(highAmount~. -Amount, data = ab.sparse.train)
prp(cart1)

pred.cart1 <- predict(cart1, newdata = ab.sparse.test)

confus.cart1 <- confusionMatrix(as.numeric(pred.cart1 >= 0.50), ab.sparse.test$highAmount)
confus.cart1

accur.cart1.improv <- confus.cart1$overall[1] - baseline.accur
accur.cart1.improv
#17.42% improvement

#Random Forests
rf1 <- randomForest(highAmount~. - Amount, data = ab.sparse.train)

pred.rf1 <- predict(rf1, newdata = ab.sparse.test)
confus.rf1 <- confusionMatrix(pred.rf1, ab.sparse.test$highAmount)
confus.rf1

accur.rf1.improv <- confus.rf1$overall[1] - baseline.accur
accur.rf1.improv
