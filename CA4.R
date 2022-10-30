library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)


mall_customers = read.csv("Mall_Customers.csv")
head(df)

mall_customers <- rename(mall_customers,c('SpendingScore'='Spending.Score..1.100.'))
mall_customers <- rename(mall_customers,c('AnnualIncome'='Annual.Income..k..'))
str(mall_customers)
head(mall_customers)

is.null(mall_customers$AnnualIncome)
is.null(mall_customers$SpendingScore)
is.null(mall_customers$Age)
sum(is.na(mall_customers))

summary(mall_customers)



#Kmeans(annual Income vs Spending Score)
k_df_2 <- mall_customers[,4:5]
set.seed(123)
ks <- 1:10
tot_within_ss <- sapply(ks, function(k) {
  cl <- kmeans(k_df_2, k, nstart = 10)
  cl$tot.withinss
})
# plot "the elbow method"
plot(ks, tot_within_ss, type = "b",
     xlab="Number of clusters k",
     ylab="Total within-clusters sum of squares")

cl <- kmeans(k_df_2,5, nstart = 10)

ggplot(k_df_2, aes(x = AnnualIncome, y = SpendingScore)) + 
  geom_point(stat = "identity", aes(color = as.factor(cl$cluster))) +
  scale_color_discrete(name=" ",labels=c(paste0("Cluster",1:5))) +
  ggtitle("Mall Customer Segments", subtitle = "K-means Clustering") + ylab("Spending Score") + xlab("Annual Income (k$)")




#Kmeans (age vs Spending Score)
k_df_1 <- mall_customers[,c(3,5)]
set.seed(123)

ks <- 1:10
tot_within_ss <- sapply(ks, function(k) {
  cl <- kmeans(k_df_1, k, nstart = 10)
  cl$tot.withinss
})
# plot "the elbow method"
plot(ks, tot_within_ss, type = "b",
     xlab="Number of clusters k",
     ylab="Total within-clusters sum of squares")




cl <- kmeans(k_df_1, 4, nstart = 10)

ggplot(k_df_1, aes(x = Age, y = SpendingScore)) + 
  geom_point(stat = "identity", aes(color = as.factor(cl$cluster))) +
  scale_color_discrete(name=" ",labels=c(paste0("Cluster",1:4))) +
  ggtitle("Mall Customer Segmens", subtitle = "K-means Clustering") + ylab("Spending Score")
