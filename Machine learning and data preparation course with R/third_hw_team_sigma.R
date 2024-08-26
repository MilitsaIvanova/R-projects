###Continue the data preprocessing from HW 2

## HW 2####
##Import RFM data###
library(readxl)
Invoices <- read_excel("uni/Masters/Предварителна обработка на данни/Invoices.xlsx")
View(Invoices)

##create aux table##
column_names=names(Invoices)
column_types=sapply(Invoices, class)
missing_values=sapply(Invoices, function(x) sum(x == 0, na.rm = TRUE))

aux_table=data.frame(Names=column_names,
                     Type=column_types,
                     NA_values=missing_values)

##adjust the column types##
install.packages('lubridate')
library('lubridate')
Invoices$Date=dmy(Invoices$Date)
Invoices$Payment=factor(Invoices$Payment)

##add the new columns month and year##
month_vector=month(Invoices$Date)
year_vector=year(Invoices$Date)
Invoices$Month<-month_vector
Invoices$Year<-year_vector

##Summary and boxplot of Sales##
summary(Invoices$Sales)
boxplot(Invoices$Sales)

##identifying outliers##
Invoices$ZScore<-(Invoices$Sales -mean(Invoices$Sales))/sd(Invoices$Sales)
df_outliers <- Invoices[abs(Invoices$ZScore)>3,]
print(df_outliers$`Client Num`)

##most purchased products##
products_purchased<-colSums(Invoices[, c('Product 1','Product 2','Product 3','Product 4','Product 5','Product 6','Product 7','Product 8')])
print(products_purchased)
#the most purchased product is Product 1

## Homework 3####

#Задача 1: Въведете три нови характеристики, които отразяват a) колко често поръчва всеки от клиентите на компанията (frequency), b) за каква сумарна стойност (monetary value) и c) преди колко дни е направена последната поръчка (recency). Визуализирайте по подходящ начин тази информация. Предложете кратък коментар на забелязаните от вас закономерности (patterns). За целта използвайте език, разбираем за бизнеса.  

#a)

unique_clients=as.numeric(table(Invoices['Client Num']))

#b)
sum_by_client <- aggregate(Sales ~ `Client Num`, data=Invoices,FUN=sum)

#c)
last_order_date= aggregate(Date ~ `Client Num`, data=Invoices,FUN=max)
last_order_date$`Client Num` =NULL #remove the Client Num column so that we don't duplicate it later when we combine the results

current_date=as.Date('2023-11-07')

last_order_date$days_since_last_order= as.numeric(difftime(current_date,last_order_date$Date, units = 'days'))

#combining the results in one table

clients_patterns=cbind(sum_by_client, last_order_date)
clients_patterns$num_of_purchases=unique_clients


boxplot(clients_patterns[,2], col="pink", main="Sales", ylab="Sales")
summary(clients_patterns['Sales'])


boxplot(clients_patterns[,4], col="blue", main="Recency", ylab="Recency")
summary(clients_patterns['days_since_last_order'])

boxplot(clients_patterns[,5], col="green", main="Number of purchases", ylab="Number of purchases")
summary(clients_patterns['num_of_purchases'])

#Задача 2: Обобщете информацията за приходите от продажби на месечна база и представете графично месечните стойности. Предложете кратък коментар на забелязаните от вас закономерности (patterns). За целта използвайте език, разбираем за бизнеса. 

library(dplyr)

monthly_summary = Invoices %>% 
  group_by(Year_Month= format(Date, "%Y-%m")) %>% 
  summarise(Total_Sales= sum(Sales))

library(ggplot2)
library(scales)
# Create a line graph
ggplot(monthly_summary, aes(x = Year_Month, y = Total_Sales, group = 1)) +
  geom_line() + 
  scale_y_continuous(labels = label_number())
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Year and Month", y = "Total Sales", title = "Monthly Sales Summary")





