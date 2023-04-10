library(tidyverse)
library(dplyr)

df <- read.csv("/content/Layoffs.csv")

head(df,10)
summary(df)

#DATA CLEANING
mean(df$total_laid_off)
is.null(df)
sum(is.na(df))
sapply(df, function(x) sum(is.na (x)))
df  <- replace(df, is.na(df), 0)
sapply(df, function(x) sum(is.na (x)))
mean(df$total_laid_off)
is.numeric(df$total_laid_off)
boxplot(df$total_laid_off,df$percentage_laid_off,df$funds_raised)
str(df)
unique(df$location)
count(df,industry)
count(df,location)
df1 <- df[,colSums(is.na(df))<nrow(df)]
head(df1,5)

#Checking for duplicate values
duplicated(df1)
colnames(df1)[4] = "laid_off"
colnames(df1)[5] = "percentage"
head(df1)

#EDA
top = df1[order(df1$laid_off),]
top = tail(top,5)
top

barplot(top$laid_off, names.arg = top$company, xlab = "Company",
        ylab = "Total Laid Off",main = "Top 5 Companies",col = "#0072B2")


barplot(top$laid_off, names.arg = top$industry, xlab = "Industry",
        ylab = "Total Laid Off",main = "Top 5 Industries",col = "#CC79A7")

install.packages("wordcloud")
library(wordcloud,wordcloud2)

#suppressWarnings()  
comp = df1$company
word_freq = table(comp)
wordcloud(words = names(word_freq), freq = word_freq, 
          min.freq = 15, random.order = FALSE, colors = rainbow(length(word_freq)))
options(warn = -1)

#Correlation Analysis
cor(df1$laid_off,df1$funds_raised)
#Calculating Correlation Matrix as 3x3 matrix(x,y)
install.packages("corrplot")
library(ggplot2)
library(dplyr)
library(corrplot)

cor_matrix <- cor(df1 %>% select_if(is.numeric))
cor_matrix

#Plotting coorelation Matrix
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "red", tl.srt = 40,addrect=2)

#Converted correlation matrix into dataframe for Manipulation
cor_df <- as.data.frame(as.table(cor_matrix))
head(cor_df,5)

names(cor_df) <- c("var1", "var2", "correlation")
head(cor_df,5)

#Potting Matrix Heatmap
ggplot(cor_df, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "pink", mid = "blue", high = "white", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Matrix 2D Grid")  

#FEATURE ENGINEERING
sapply(df1, class)    #to check for DataTypes

#Creating new features from existing features
df2 <- df1 %>%
  mutate(NewFeature= df1$funds_raised + df1$laid_off)
head(df2,5)                 

#Applying Logarithmic function
df1$log_laid_off <- log(df1$laid_off)
head(df1$log_laid_off,1)

#Applying Standard scaling
df1$standardized_laid_off <- scale(df1$laid_off)       #Leave Columns Unchanged
head(df1$standardized_laid_off,2,1)

library(caret)
head(df2,5)

#Remove Unnecesary Features
#df2 <- df2 %>% 
#select(-c(df2$laid_off, df2$funds_raised))

#Wordcloud for NewFeature
set.seed(100)
wordcloud(words = df2$company,  freq = df2$NewFeature, min.freq = 3,max.words=250, random.order=FALSE, rot.per=0.50, colors=brewer.pal(5, "Dark2"))

unique(df$location)
df1$location = factor(df1$location, levels = c("London", "SF Bay Area", "Bengaluru", "Singapore", "Sao Paulo", "Chicago", "Stockholm", "New York City", "Berlin", "Tel Aviv", "Boston", "Burlington", "Los Angeles", "Jakarta", "Sacramento", "Buenos Aires", "Melbourne", "Waterloo", "Lagos", "Dubai", "Gurugram", "Phoenix", "Gothenburg", "Toronto", "Dublin", "Seattle", "Nairobi", "Dover", "Hamburg", "San Diego", "Logan", "Tallin", "Lehi", "Columbus", "Copenhagen", "Vancouver", "Oslo", "Pittsburgh", "Montreal", "San Luis Obispo", "Jerusalem", "Austin", "New Delhi", "Belo Horizonte", "Salt Lake City", "Bangkok", "Raleigh", "Portland", "Bristol", "Washington D.C.", "Indianapolis", "Stamford", "Curitiba", "Mumbai", "Boulder", "Sydney", "Detroit", "Ottawa", "Ferdericton", "Dakar", "Florianópolis", "Philadelphia", "Hong Kong", "Beijing", "Vienna", "Atlanta", "Dallas", "Spokane", "Chennai", "Reno", "Helsinki", "Malmo", "Kuala Lumpur", "Bend", "Mexico City", "Cincinnati", "Miami", "Moscow", "Shanghai", "Non-U.S.", "Nashville", "Las Vegas", "Edinburgh", "Madison", "Amsterdam", "Santa Fe", "Denver", "Ahmedabad", "Joinville", "Zurich", "Missoula", "Minneapolis", "Guadalajara", "Blumenau", "Milwaukee", "Ann Arbor", "Lisbon", "Munich"),
                      labels = as.integer(0:97))
df1$location

df1$industry = factor(df1$industry,levels = c('Finance','Product','Food','HR','Security','Real Estate','Transportation','Legal','Marketing','Retail','Media','Crypto','Education','Other','Consumer','Healthcare','Infrastructure','Data','Sales','Fitness','Support','Logistics','Recruiting','Construction','Aerospace','Travel','Energy'),
                      labels = as.integer(0:26))
df1$industry
unique(df2$stage)
df1$stage = factor(df1$stage, levels = c('Series B','Series F','Unknown','Series D','Series C','Private Equity','Acquired','IPO','Series E','Series A','Series J','Series H','Series G','Seed','Series I'),
                   labels = as.integer(0:14))
df1$stage

tail(df1)
head(df1)

#Spli Dataset Into Train and Test Set.
n_obs <- nrow(df2)
split <- round(n_obs * 0.7)
train <- df2[1:split,]
test <- df2[(split + 1):nrow(df2),]

dim(train)
dim(test)


#K-Means Clustering
library("NbClust")
wssplot <- function(df1, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

df_scaled <- scale(df2$laid_off[])
head(df_scaled)

nc <- NbClust(df_scaled, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

fit.km <- kmeans(df2$laid_off, 3, nstart=25)
fit.km$size
fit.km$centers

aggregate(df[-1], by=list(cluster=fit.km$cluster),mean)


#Random Forest
library(randomForest)
model_eval<-randomForest(formula= laid_off~.,data = train)
model_eval

pred.rf <- predict(model_eval,test)
rmse.rf <- sqrt(sum(((pred.rf) - test$laid_off)^2)/
                  length(test$laid_off))
c(RMSE = rmse.rf, pseudoR2 = mean(model_eval$rsq))

plot(pred.rf,test$laid_off, xlab = "stage", ylab = "laid_off", pch = 3)
predicted_model_eval<-predict(model_eval,test)
plot(model_eval, main="RandomForest Model")
varImpPlot(model_eval,sort=TRUE,main = "Varable Importance Plot - randomForest")
glimpse(df2)

#Linear Regression
#Building the Model
model_eval<-lm(laid_off~stage,data = train)
summary(model_eval)
#Prediction on test set
pred.lm <- predict(model_eval,test)
results <- cbind(pred.lm,test$laid_off)
rmse.lm <- sqrt(sum((pred.lm - test$laid_off)^2)/length(test$stage))
c(RMSE = rmse.lm, R2 = summary(model_eval)$r.squared)
plot(df2$stage, df2$laid_off, main = "Linear Regression", 
     xlab = "stage",ylab = "laid_off",xlim = c(0,10))
abline(model_eval)


#Naive Bayes
install.packages("e1071")
library(e1071)

nb<-naiveBayes(formula= laid_off~stage, data = train)
predicted_nb= predict(nb,test)

plot(predicted_nb, test$laid_off , main="Naive Bayes", xlab="Stage",ylab="laid_off")
