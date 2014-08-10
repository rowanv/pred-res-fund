#Rowan Vasquez

#What types of research are most likely to receive higher levels of funding?
#Let's use text analytics to analyze the words in the research abstracts

#We've previously looked at Research Projects for 2013. We're now going to repeat the analysis
#for 2011 and 2012. We aim to determine whether the types of research most likely 
#to receive higher levels of funding changes over the years.

library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(reshape) #change var names
library(wordcloud)






clean_data <- function(rs){
	#Rename vars
	rs <- rename(rs, c(Abstract.at.Time.of.Award = 'Abstract'))
	rs <- rename(rs, c(Estimated.Total.Award.Amount = 'Amount'))
	#Remove everything that is not a digit
	rs$Amount <- gsub('\\D', '', rs$Amount)
	rs[,c("Amount")]<- as.numeric(rs[,c("Amount")])
	
	return(rs)
}

#creates a Document Term Matrix
textanalytics_preprocessing <- function(column1){
	#converts to lowercase, remove punctuation, removes stemwords,
	#stems words, eliminates extra whitespace
	
	#Remove punctuation -- avoids a tm bug
	column1 <- iconv(column1, to = 'UTF-8')
	column1 <- gsub("[^[:alnum:]///' ]", "", column1)
	column1 <- lapply(column1, tolower)
	column1 <- lapply(column1, removePunctuation)
	column1 <- lapply(column1, removeWords, c('amp', 'ampgt', 'gt'))
	column1 <- lapply(column1, removeWords, stopwords('english'))
	column1 <- lapply(column1, stemDocument)
	column1 <- lapply(column1, stripWhitespace)
	#remove trailing spaces
	column1 <- gsub(" $","", column1, perl=T)

	
	return(column1)
}

create_dtm <- function(rs) {
	ab.corpus <- Corpus(VectorSource(rs$Abstract))
	ab.corpus[[1]]
	





	#remove punctuation
	ab.corpus <- tm_map(ab.corpus, removePunctuation)

	#remove stop words
	ab.corpus <- tm_map(ab.corpus, removeWords, stopwords("english"))


	#stem words
	ab.corpus <- tm_map(ab.corpus, stemDocument)
	ab.corpus[[1]]
	


	#Bag of Words

	ab.frequencies <- TermDocumentMatrix(ab.corpus)
	ab.corpus

	ab.sparse <- removeSparseTerms(ab.frequencies, 0.90)
	#Keep terms that appear in 10% or more of the abstracts

	ab.sparse

	ab.sparse <- as.data.frame(as.matrix(ab.sparse))

	#make column names appropriate for R
	colnames(ab.sparse) <- make.names(colnames(ab.sparse))



	#Add DV
	ab.sparse <- as.data.frame(t(ab.sparse)) #transposing
	ab.sparse$Amount <- rs$Amount

	str(ab.sparse)

	
	#Removing NA values
	ab.sparse <- na.omit(ab.sparse)
	upper.quart.amount <-  quantile(ab.sparse$Amount, prob=c(0.90)) 
	upper.quart.amount
	ab.sparse$highAmount <- as.numeric(ab.sparse$Amount >= upper.quart.amount)

	
	
	return(ab.sparse)
}

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

calculate_statistic_signif <- function(logit1){
	

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



	stat.signif.var <- pseudo.freq(stat.signif.var)
	stat.signif.var
	return(stat.signif.var)
}
split_datasets <- function(ab.sparse){
	#split into training and testing set
	set.seed(23232)
	ab.sparse.split <- sample.split(ab.sparse$highAmount, SplitRatio = 0.7) 
	ab.sparse.train <- subset(ab.sparse, ab.sparse.split == TRUE)
	ab.sparse.test <- subset(ab.sparse, ab.sparse.split == FALSE)
	return(list(train = ab.sparse.train, test = ab.sparse.test))
}



create_wordcloud <- function(stat.signif.var){
	wordcloud1 <- wordcloud(stat.signif.var[,1], stat.signif.var[,3], rot.per = 0.35, use.r.layout=FALSE ,colors = brewer.pal(3, "Oranges"))
	return(wordcloud1)
}





#########################
#Reading in the Data
#########################


data.dir <- '/Users/rowanvasquez/Documents/Data\ Science/Personal\ Research\ Projects/Research\ Funding/'


rs.file2013 <- paste0(data.dir, 'ResearchAwards-2013.csv')
rs.file2012 <- paste0(data.dir, 'ResearchAwards-2012.csv')
rs.file2011 <- paste0(data.dir, 'ResearchAwards-2011.csv')

rs.2013 <- read.csv(rs.file2013, stringsAsFactors = F)
rs.2012 <- read.csv(rs.file2012, stringsAsFactors = F)
rs.2011 <- read.csv(rs.file2011, stringsAsFactors = F)

str(rs.2013)
str(rs.2012)
str(rs.2011)

#########################
#Cleaning the Datasets
#########################

rs.2013 <- clean_data(rs.2013)
rs.2012 <- clean_data(rs.2012)
rs.2011 <- clean_data(rs.2011)

rs.2013$Abstract <- textanalytics_preprocessing(rs.2013$Abstract)
rs.2012$Abstract <- textanalytics_preprocessing(rs.2012$Abstract)
rs.2011$Abstract <- textanalytics_preprocessing(rs.2011$Abstract)

ab.sparse2013 <- create_dtm(rs.2013)
ab.sparse2012 <- create_dtm(rs.2012)
ab.sparse2011 <- create_dtm(rs.2011)


##########################
#Data Analysis
##########################

#Splitting datasets
split2013 <- split_datasets(ab.sparse2013)
ab.sparse2013.train <- split2013$train
ab.sparse2013.test <- split2013$test
split2012 <- split_datasets(ab.sparse2012)
ab.sparse2012.train <- split2012$train
ab.sparse2012.test <- split2012$test
split2011 <- split_datasets(ab.sparse2011)
ab.sparse2011.train <- split2011$train
ab.sparse2011.test <- split2011$test

#Training logit models

logit2013.1 <- glm(highAmount~.-Amount, data = ab.sparse2013.train, family = "binomial")
summary(logit2013.1)
pred.logit2013.1 <- predict(logit2013.1, newdata = ab.sparse2013.test)

confus.logit2013.1 <- confusionMatrix(as.numeric(pred.logit2013.1 >= 0.50), ab.sparse2013.test$highAmount)
confus.logit2013.1



logit2012.1 <- glm(highAmount~.-Amount, data = ab.sparse2012.train, family = "binomial")
summary(logit2012.1)
pred.logit2012.1 <- predict(logit2012.1, newdata = ab.sparse2012.test)

confus.logit2012.1 <- confusionMatrix(as.numeric(pred.logit2012.1 >= 0.50), ab.sparse2012.test$highAmount)
confus.logit2012.1


logit2011.1 <- glm(highAmount~.-Amount, data = ab.sparse2011.train, family = "binomial")
summary(logit2011.1)
pred.logit2011.1 <- predict(logit2011.1, newdata = ab.sparse2011.test)

confus.logit2011.1 <- confusionMatrix(as.numeric(pred.logit2011.1 >= 0.50), ab.sparse2011.test$highAmount)
confus.logit2011.1


#Calculating the statistically significant terms
coef.logit2013.1 <- as.data.frame(summary(logit2013.1)$coefficients)
str(coef.logit2013.1)
summary(coef.logit2013.1)
coef.logit2013.1



stat.signif2013 <- calculate_statistic_signif(logit2013.1)
stat.signif2012 <- calculate_statistic_signif(logit2012.1)
stat.signif2011 <- calculate_statistic_signif(logit2011.1)

create_wordcloud(stat.signif2013)
create_wordcloud(stat.signif2012)
create_wordcloud(stat.signif2011)


