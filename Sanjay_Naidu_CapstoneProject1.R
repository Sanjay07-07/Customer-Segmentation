############################################################
# Customer Segmentation Project (Capstone Project 1)
# By Sanjay Naidu
# Goal: The aim of this project is to identify segments/clusters of customers
#       based on common characteristics or patterns. 
#       In this project, we will make use of K-means clustering.
###########################################################

#Loading Required Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(DataExplorer)
library(lubridate)
library(heatmaply)
library(dlookr)
library(highcharter)
library(factoextra)
library(scales)
library(ClusterR)
library(cluster)
library(caret)

#Loading the Dataset
cust_data <- read.csv("data.csv")

dim(cust_data) #541,909 rows and 8 columns
str(cust_data) #Checking datatypes
names(cust_data) #View column names
head(cust_data) #Preview Dataset
summary(cust_data)

#Checking Missing Values
colSums(is.na(cust_data)) #Lot of missing values in CustomerID.

#We can remove the NA values and still be left with sufficient dataset for exploration.
#Removing the NA values from CustomerID.
#complete.cases() returns a logical vector indicating which cases are complete, i.e., have no missing values
cust_data <- cust_data[complete.cases(cust_data),] 
dim(cust_data)
#We are now left with 406829 observations.

#Checking Missing Values again to confirm.
colSums(is.na(cust_data))

correlate(cust_data)

#We check for outliers using dlookr package.
#The plot_outlier() visualize outlier information for diagnosing the quality of the numerical data
plot_outlier(cust_data, Quantity, col = "steelblue")
plot_outlier(cust_data, UnitPrice, col = "steelblue")

#Treating Ouliers
for (x in c('Quantity','UnitPrice'))
{
  value = cust_data[,x][cust_data[,x] %in% boxplot.stats(cust_data[,x])$out]
  cust_data[,x][cust_data[,x] %in% value] = NA
}

#Checking Summary for Quantity and UnitPrice
summary(cust_data$Quantity)
summary(cust_data$UnitPrice)
summary(cust_data$CustomerID)

#Negative Quantity denotes cancelled orders indicated by letter "C" infront of the InvoiceNo.
#Checking Quantities of negative values.
neg_quantity <- cust_data %>% 
  filter(Quantity < 0) %>% 
  arrange(Quantity)
head(neg_quantity, 5)

#Replacing negative values in Quantity and UnitPrice with NA
#mutate function is used to create a new variable from a data set. Found in dplyr package
#mutate is similar to transform but it executes the transformation iteratively so that the later transformation can use the columns created by earlier ones.
cust_data <- cust_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))
cust_data <- cust_data %>%
  drop_na() 
dim(cust_data) #We are now left with 338151 observations

#We check if there are any outliers present now.
plot_outlier(cust_data, Quantity, col = "brown")
plot_outlier(cust_data, UnitPrice, col = "brown")


#function provide base mechanisms for defining new function in R.
#We are going to separate date and time components from InvoiceDate Column
#strsplit-> Split the elements of a character vector x into substrings according to the matches to substring split within them.
cust_data$Date <- sapply(cust_data$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
cust_data$Time <- sapply(cust_data$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})

#we create month, year and hour of day columns
cust_data$Month <- sapply(cust_data$Date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][1]})
cust_data$Year <- sapply(cust_data$Date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][3]})
cust_data$Hour <- sapply(cust_data$Time, FUN = function(x) {strsplit(x, split = '[:]')[[1]][1]})

#Converting Date column to proper format
#We can also create day of the week column, using the wday function from the lubridate package.
cust_data$Date <- as.Date(cust_data$Date, "%m/%d/%Y")
str(cust_data)
cust_data$Week <- wday(cust_data$Date, label=TRUE)

#Adding a new column for Cost
cust_data <- cust_data %>% mutate(Cost = Quantity * UnitPrice)

#We are changing the type of important variables into Factors
cust_data$Month <- as.factor(cust_data$Month)
cust_data$Year <- as.factor(cust_data$Year)
levels(cust_data$Year) <- c(2010,2011)
cust_data$Hour <- as.factor(cust_data$Hour)
cust_data$Week <- as.factor(cust_data$Week)
cust_data$Country <- as.factor(cust_data$Country)

#EXPLORATORY DATA ANALYSIS#
#Revenue Summary
#Plot Revenue over time
options(repr.plot.width=10, repr.plot.height=4)
cust_data %>%
  group_by(Date) %>%
  summarise(Revenue = sum(Cost)) %>%
  ggplot(aes(x = Date, y = Revenue)) + 
  geom_line() + 
  geom_smooth(method = 'auto', se = FALSE) + 
  labs(x = 'Date', y = 'Revenue', title = 'Revenue by Date')

#Plot Revenue by Week
cust_data %>%
  group_by(Week) %>%
  summarise(Revenue = sum(Cost)) %>%
  ggplot(aes(x = Week, y = Revenue)) + 
  geom_col(show.legend = FALSE)+
  labs(x = 'Week(Day)', y = 'Revenue', title = 'Revenue by Week')

#Summary of revenue generated on particular weekday
weekday_sum <- cust_data %>%
  group_by(Date, Week) %>%
  summarise(Revenue = sum(Cost), Transaction = n_distinct(InvoiceNo)) %>%
  mutate(Value = (round((Revenue / Transaction),2))) %>%
  ungroup()

#Plot of Transactions by Week
ggplot(weekday_sum, aes(x = Week, y = Transaction)) + 
  geom_boxplot() + 
  labs(x = 'Week(Day)', y = 'Transactions', title = 'Transactions by Week') 

#Plot of Average Order Value by Day of the Week
ggplot(weekday_sum, aes(x = Week, y = Value)) + 
  geom_boxplot() + labs(x = 'Week(Day)', y = 'Average Order Value', title = 'Average Order Value by Week')

#The differences in the amount of revenue on each day of the week is driven by a difference in the no. of transactions, rather than the average order value.

#Data suggest people are more ready to buy on Thursday and fewer transactions on Sunday.
ggplot(weekday_sum, aes(Transaction, fill = Week)) + 
  geom_density(alpha = 0.4)

#Country Summary
#n_distinct <- faster and more concise equivalent of length(unique(x))
SummaryofCountry <- cust_data %>%
  group_by(Country) %>%
  summarise(Revenue = sum(Cost), Transaction = n_distinct(InvoiceNo)) %>%
  mutate(Value = (round((Revenue / Transaction),2))) %>%
  ungroup() %>%
  arrange(desc(Revenue))
head(SummaryofCountry, 10)

#Sorting best five countries by revenue
Best5Countries <- cust_data %>%
  filter(Country == 'United Kingdom' | Country == 'Germany' | Country == 'France' | Country == 'EIRE' | Country == 'Spain' | Country == 'Switzerland')

#dataframe of top 5 countries by revenue
top5 <- Best5Countries %>%
  group_by(Country) %>% 
  summarise(Revenue = sum(Cost), Transaction = n_distinct(InvoiceNo), 
                   Customers = n_distinct(CustomerID)) %>%
  mutate(Value = (round((Revenue / Transaction),2))) %>%
  arrange(desc(Revenue))

#Plot the relation between Country and Revenues
top5 %>% 
  ggplot(aes(x=Country, y=Revenue))+
  geom_bar(stat = 'identity', fill = 'lightgreen') +
  ggtitle('Top 5 Countries by Revenue') +
  xlab('Countries') +
  ylab('Revenue')+
  scale_y_continuous(labels = comma)

#Plot top 5 country revenue summary (Without United Kingdom)
#Netherlands and EIRE are significant sources of revenue
#Germany and France also represent significant opportunities
#we repeat the above step without United Kingdom
#Top five countries in terms of revenue contribution
Best5Countries <- cust_data %>%
  filter(Country == 'Germany' | Country == 'France' | Country == 'EIRE' | Country == 'Spain' | Country == 'Switzerland')
top5 <- Best5Countries %>%
  group_by(Country) %>%
  summarise(Revenue = sum(Cost), Transaction = n_distinct(InvoiceNo), 
                   Customers = n_distinct(CustomerID)) %>%
  mutate(Value = (round((Revenue / Transaction),2))) %>%
  arrange(desc(Revenue))

top5 %>% 
  group_by(Country) %>%
  summarise(Revenue = sum(Revenue)) %>% 
  hchart('treemap', hcaes(x = 'Country', value = 'Revenue', color = 'Revenue')) %>%
  hc_title(text=" Top 5 Countries by Revenue (excluding United Kingdom)")
top5
#Customer Segmentation
#Using CustomerID to find the difference between customers
cust_segmentation <- cust_data %>%
  group_by(CustomerID) %>%
  summarise(Revenue = sum(Cost), Transaction = n_distinct(InvoiceNo)) %>%
  mutate(Value = (round((Revenue / Transaction),2))) %>%
  ungroup() %>%
  arrange(desc(Revenue))

head(cust_segmentation)

#Summarize customers with high revenues/sales
cust_segmentation_2 <- cust_data %>%
  group_by(CustomerID, InvoiceNo) %>%
  summarise(Revenue = sum(Cost), Transaction = n_distinct(InvoiceNo)) %>%
  mutate(Value = (round((Revenue / Transaction),2))) %>%
  ungroup() %>%
  arrange(Revenue) %>%
  mutate(CummulativeSum = cumsum(Revenue))

head(cust_segmentation_2)

#It seems many of the large transactions are refunded
#we sum the revenue
cust_segmentation_2 <- cust_data %>%
  group_by(InvoiceNo, CustomerID, Country, Date, Month, Year, Hour, Week) %>%
  summarise(TotalCost = sum(Cost)) %>%
  mutate(recent = Sys.Date() - Date) %>% #How many days lapsed from the current date.
  ungroup()

cust_segmentation_2$recent <- as.character(cust_segmentation_2$recent)
cust_segmentation_2$RecentDays <- sapply(cust_segmentation_2$recent, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
cust_segmentation_2$RecentDays <- as.integer(cust_segmentation_2$RecentDays)
head(cust_segmentation_2, 5)

#The data frame can provide us with the order value and date & time information for each transaction, that can be grouped by customer
cust_segmentation_3 <- cust_segmentation_2 %>%
  group_by(CustomerID, Country) %>%
  summarise(Orders = n_distinct(InvoiceNo), Revenue = sum(TotalCost), MeanRevenue = round(mean(TotalCost), 2),
            MedianRevenue = median(TotalCost), MostDays = names(which.max(table(Week))),
            MostHours = names(which.max(table(Hour))), recentDays = min(RecentDays))%>%
  ungroup()
head(cust_segmentation_3)

#We filter orders greater than 1 and revenue greater than 50 pounds
#Our data frame that gives us a list of repeat customers and tells us their country, how many orders they have made, total revenue and average order value as well as the day of the week and the time of the day they most frequently place orders.
cust_segmentation_summary <- cust_segmentation_3 %>%
  filter(Orders > 1, Revenue > 50)
head(cust_segmentation_summary, 5)
dim(cust_segmentation_summary) # We remain with a small subset (2693)

#From this, we are in a better position to answer a number of questions about our customers that we could use to target specific marketing materials, promotions and offers.
Target_Customer <- cust_segmentation_summary %>%
  select(recentDays, Revenue, MeanRevenue, MedianRevenue, Orders) %>%
  as.matrix()
rownames(Target_Customer) <- cust_segmentation_summary$CustomerID
head(Target_Customer)

#Generating Heatmap
#By analyzing customer cluster, we discover groups of customers that behave in similar ways. 
#This level of customer segmentation is useful in marketing to these groups of customers appropriately. 
#A marketing campaign that works for a group of customers that places low value orders frequently may not be appropriate for customers who place sporadic, high value orders for example.
options(repr.plot.width=20, repr.plot.height=14)
heatmap(scale(Target_Customer), cexCol = 0.7)
#Recency: It refers to the number of days before the reference date when a customer made the last purchase. Lesser the value of recency, higher is the customer visit to a store.

#Clustering using K-Means

set.seed(100)

#We find the optimal number for k
wss <- 0
for (j in 1:15) {
  km.out <- kmeans(Target_Customer, centers = j, nstart = 20)
  wss[j] <- km.out$tot.withinss
}

wss_df <- data.frame(num_cluster = 1:15, wgss = wss)

ggplot(data = wss_df, aes(x=num_cluster, y= wgss)) + 
  geom_line(color = "lightgrey", size = 2) + 
  geom_point(color = "green", size = 4)

km <- kmeans(scale(Target_Customer[,1:5]), 4, nstart = 1)# Performing kmeans with 4 clusters. nstart > 1 is often recommended.

dd <- cbind(Target_Customer, cluster = km$cluster)
head(dd)

#cluster sizes
km$size

#cluster means
km$centers

aggregate(Target_Customer, by=list(cluster=km$cluster), max)

#We plot the clusters
fviz_cluster(km, data=as.data.frame(Target_Customer)[, -6],
             ellipse.type = "convex")




























































































































































































































































