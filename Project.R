library(flexdashboard) # Dashboard package
library(highcharter) # Interactive data visualizations
library(plotly) # Interactive data visualizations
library(viridis) # Color gradients
library(tidyverse) # Metapackge
library(countrycode) # Converts country names/codes
library(rjson) # JSON reader
library(crosstalk) # Provides interactivity for HTML widgets
library(DT) # Displaying data tables
library(cluster) 
library(factoextra) # clustering algorithms & visualization
library(fpc)
library(arulesViz)
library(ISLR)
library(class)
library(ggplot2)
library(arules)
library(arulesViz)
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(crossval)
library(gplots)
library(Metrics)
library(corrplot)

set.seed(123)

# Read in data. 
data <- read.csv('insurance.csv')
dim(data)
data$ORIGIN <- NULL
str(data)
count<-table(data$CARAVAN)
count
colors=c("yellow","blue")
col=colors
pie(count,main = "CUSTOMERS OF CARAVAN POLICY",col=colors)
box()

df <- cor(data[75:86],data[75:86],method="spearman")
data.cor = cor(data)
data.cor = cor(data, method = c("spearman"))
data.cor
corrplot(df)

classLabelFreq <- data.frame(data$CARAVAN)
classLabelFreq$df.CARAVAN <- as.factor(data$CARAVAN)

#Class label Distribution Plot 
ggplot(classLabelFreq,aes(x=df.CARAVAN)) + geom_bar() + labs(x="CARAVAN")

#Size of each factor level 
length(classLabelFreq[classLabelFreq$data.CARAVAN=="0",])
length(classLabelFreq[classLabelFreq$data.CARAVAN=="1",])


customerrange <- data.frame(data$MOSHOOFD,data$CARAVAN)
customerrange$data.MOSHOOFD <- as.factor(customerrange$data.MOSHOOFD)
customerrange$data.CARAVAN <- as.factor(customerrange$data.CARAVAN)

#Plot customers category
plot<-ggplot(customerrange,aes(x=reorder(data.MOSHOOFD,data.MOSHOOFD,function(x)-length(x)),fill=data.CARAVAN),col=colors)
plot<-plot + geom_bar() 
plot<-plot + labs(x="Customer Category")
plot                         

#When Caravan is true
carainsu <- data[data$CARAVAN==1,]
carainsu$MOSHOOFD <- as.factor(carainsu$MOSHOOFD)
carainsu$MOSTYPE <- as.factor(carainsu$MOSTYPE)

#Plot of Customer who wants caravan insurance
plot<-ggplot(carainsu,aes(x=reorder(MOSHOOFD,MOSHOOFD,function(x)-length(x))))
plot<-plot + geom_bar()
plot<-plot + labs(x="Customer who wants caravan")
plot

#Max and Min
mainCustType = table(data$MOSHOOFD)
names(which.max(mainCustType))
names(which.min(mainCustType))

a<-table(data$APLEZIER)
a


#clustering
wss <- (nrow(data)-1)*sum(apply(data,2,var))
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(data, centers = 5, nstart = 25)
k3 <- kmeans(data, centers = 5, nstart = 25)
str(k2)
str(k3)
is.na(data)
fviz_cluster(k2, data = data)
k2
fviz_cluster(k3, data = data)
k3


#association
data.disc <- as.data.frame(lapply(data, function(x) discretize(x, breaks = c(1, 500, 1000, 1500, 2000, 2500, 3000, 3500, 3821,5000,8000), method = "fixed")))
data.rules <- apriori(data.disc, parameter=list(support = 0.0025, confidence = 1.00))
# # number of rules found
length(data.rules)
# # inspect first 10 rules
inspect(head(data.rules))
# # inspect last 10 rules
inspect(tail(data.rules))
plot(data.rules, method = "two-key plot")

#head(Football)
dim(data)         #[1] 380 481
#summary(Football)

#decision trees
m1 <- rpart(
  formula = CARAVAN ~ .,
  data    = data,
  method  = "anova"
)



m1
rpart.plot(m1)
plotcp(m1)


'''
kmeansfo <- pamk(data)
kmeansfo$nc

table(k3$cluster, data$MOSTYPE)
attach(mtcars)
plot(wt, mpg, main="data",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
'''

m2 <- rpart(
  formula = CARAVAN ~ .,
  data    = data,
  method  = "anova", 
  control = list(cp = 0, xval = 10)
)
m2
rpart.plot(m2)

plotcp(m2)
abline(v = 12, lty = "dashed")
m2$cptable


m3 <- rpart(
  formula = CARAVAN ~ .,
  data    = data,
  method  = "anova", 
  control = list(minsplit = 10, maxdepth = 12, xval = 10)
)

m3$cptable



# Decision Tree Classification

# Importing the dataset
dataset = read.csv('insurance.csv')
dataset = dataset[87]

# Encoding the target feature as factor
data$CARAVAN = factor(data$CARAVAN, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data$CARAVAN, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)



############

df <- data
set.seed(99)

tr <- df[sample(row.names(df), size = round(nrow(df)*0.5)),]
te <- df[!(row.names(df) %in% row.names(tr)), ]
tr1 <- tr
te1  <- te


te2$CARAVAN <- rep(0,nrow(te2))
tr1$CARAVAN = as.factor(tr1$CARAVAN)
fit1 <- rpart(formula=CARAVAN ~ .,data=tr1,control=rpart.control(minsplit=20, minbucket=1, cp=0.008))
m1 <- rpart(
  formula = CARAVAN ~ .,
  data    = tr1,
  method  = "anova"
)



m1

rpart.plot(m1)
plotcp(m1)

m2 <- rpart(
  formula = CARAVAN ~ .,
  data    = te1,
  method  = "anova"
)



m2
rpart.plot(m2)
plotcp(m2)

fit1

gc()


fancyRpartPlot(fit1)
dim(x)
