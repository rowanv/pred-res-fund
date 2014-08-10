#Rowan Vasquez

#What types of research are most likely to receive higher levels of funding?
#Let's use text analytics to analyze the words in the research abstracts

#Part 2 - Cross Validation

#The dataset utilized for this analysis is available at the following URL:
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
ab.sparse.save.file <- paste0(data.dir, "ResearchAwards-2013-ab-sparse.Rda" )

load(ab.sparse.save.file)

#Removing NA values
ab.sparse <- na.omit(ab.sparse)

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

############
#Logit1
###########

logit1 <- glm(highAmount~.-Amount, data = ab.sparse.train, family = "binomial")
summary(logit1)
pred.logit1 <- predict(logit1, newdata = ab.sparse.test)

summary(pred.logit1)

confus.logit1 <- confusionMatrix(as.numeric(pred.logit1 >= 0.50), ab.sparse.test$highAmount )
confus.logit1

accur.logit1.improv <- confus.logit1$overall[1] - baseline.accur 
accur.logit1.improv
#17.12% improvement

?summary.glm()

#Statistically significant terms for logit1
coef.logit1 <- as.data.frame(summary(logit1)$coefficients)
str(coef.logit1)
summary(coef.logit1)
coef.logit1

stat.signif <- subset(coef.logit1, coef.logit1[4] <=0.01)
stat.signif



#p values and variables that are statistically significant
stat.signif.var <- data.frame(Variable = row.names(stat.signif), PVal = stat.signif[4], pseudoFreq = as.numeric(c(1:nrow(stat.signif))))
stat.signif.var

#Let's create a word cloud that shows the terms that are statistically significant
#for the logit model

#Changing P-Values to pseudo-frequencies for word cloud
#pval <0.0001, freq = 15
#pval <0.001, freq = 10
#pval <0.01, freq = 5

#Remove the (Intercept) var
stat.signif.var <- stat.signif.var[2:nrow(stat.signif),]

pseudo.freq <- function(stat.signif.var){
	var.name <- stat.signif.var[1]
	pval <- stat.signif.var[2]
	ps.freq <- stat.signif.var[3]
	

	for(i in 1:nrow(stat.signif.var)){
		if(pval[i,] <= 0.0001){
			ps.freq[i,] <- 15
		}
		else if (pval[i,] <= 0.001){
			ps.freq[i,] <- 10
		}
		else if (pval[i,] <= 0.01){
			ps.freq[i,] <- 5
		}
		
	}
	stat.signif.var[3] <- ps.freq
	return(stat.signif.var)
}

stat.signif.var <- pseudo.freq(stat.signif.var)
stat.signif.var

wordcloud1 <- wordcloud(stat.signif.var[,1], stat.signif.var[,3], rot.per = 0.35, use.r.layout=FALSE ,colors = brewer.pal(3, "Oranges"))

#The word cloud is available at http://www.totiusmundi.com/QuantPortfolio/QuantPortfolio.html

#The statistically significant terms for the logit model are:
#applic, collabor, continu, cours, develop, enabl, human, includ, interdisciplinari,
#learn, nation, number, observ, particular, physic, present, product, program,
#public, research, result, school, scienc, three, train, various
