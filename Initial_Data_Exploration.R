#Rowan Vasquez

#Linear Regression Analysis of Research Spending & Results Dataset

#The dataset utilized for these visualizations is available at the following URL:
#http://www.research.gov/research-portal/appmanager/base/desktop?_nfpb=true&_eventName=viewQuickSearchFormEvent_so_rsr

library(ggplot2)
if(FALSE){

data.dir <- "/Users/rowanvasquez/Documents/Personal\ Research\ Projects/Datasets/" 
rs.file <- paste0(data.dir, "ResearchAwards-2013.csv") 

rs.full <- read.csv(rs.file)

str(rs.full)
summary(rs.full)
head(rs.full)

keepcols <- c("Estimated.Total.Award.Amount", "Agency", "Federal.Award.ID.Number", "Program", "Awardee.Street", "Awardee.State", "Awardee.ZIP", "Awardee.County", "Awardee.Country", "Awardee.Cong..District", "Primary.Organization.Name", "Primary.ZIP", "Primary.City", "Primary.State", "Primary.County", "Primary.Country", "Primary.Cong..District", "Publications.Produced.as.a.Result.of.this.Research")

rs <- rs.full[,keepcols] #abridged data set

rm(rs.full) #remove the full data set



save(rs, file = "ResearchAwards-2013-small.Rda")
}

load("ResearchAwards-2013-small.Rda")

summary(rs)
str(rs)
head(rs)



colnames(rs)[1] <- c("award.amount")

head(rs$award.amount)

#removing everything that is not a digit
rs$award.amount <- gsub('\\D', '', rs$award.amount)

head(rs$award.amount)

rs[,c("award.amount")]<- as.numeric(rs[,c("award.amount")])

range(rs$award.amount, na.rm = TRUE)
mean(rs$award.amount, na.rm= TRUE)



#Histogram
binsize <- diff(range(rs$award.amount, na.rm = TRUE))/1000
ggplot(rs, aes(x = award.amount)) + geom_histogram(binwidth = binsize)

# we can see that the majority of the observations are clustered below $1 million.

#Let's look at the box plot
boxplot(rs$award.amount)
#it looks like we have a handful of outliers
outliers = boxplot(rs$award.amount, plot=FALSE)$out

str(outliers)
#extracting outliers
rs<- rs[!(rs$award.amount %in% outliers),]

str(rs)
#get rid of around 600 outlier observations, leaves 12313 observations

ggplot(rs, aes(x = award.amount)) + geom_histogram() + ggtitle("Histogram of Research Award Amounts")
boxplot(rs$award.amount, main = "Boxplot of Research Award Amounts")
