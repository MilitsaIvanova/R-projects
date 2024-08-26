##RFM data preparation####

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

##customized pretty graphs from HW 1####
library(ISLR)
data("Auto")
View(Auto)


subset_cars_equals_eight= Auto[Auto$cylinders==8 & Auto$year<=80,]
subset_cars_equals_eight=subset_cars_equals_eight[,c('mpg','weight',"acceleration","name")]

#mpg graphs
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(subset_cars_equals_eight[,1],main= 'Miles per galon for cars with 8 cylinders',
     col = 'pink',xlab = 'miles per galon',xlim = c(8,22),breaks = 20) 
boxplot(subset_cars_equals_eight[,1], col = 'pink',main = "Box plot summary",ylab='miles per galon')
plot(subset_cars_equals_eight[,1],ylab='miles per galon')
