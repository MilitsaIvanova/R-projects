#HOMEWORK 1####

#Problem 1
#Нов dataset, където цилиндрите да са точно равни на 8 и годината да е <= 80
#Да фигурират само колоните mpg, weight, acceleration, name
#За променливите mpg, weight да направим боксплот, обикновен плот и хистограма и да сметнем 
#описателните статистики (summary) и интерпретация

#installing and loading ISLR library
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


#как да покажем повече ст-ти на х оста? -> с breaks? 
summary(subset_cars_equals_eight[,1])

#weight graphs

boxplot(subset_cars_equals_eight[,2], col='lightblue')
plot(subset_cars_equals_eight[,2])
hist(subset_cars_equals_eight[,2],main = "Weight of 8 cylinder cars",col='lightblue',xlab = 'weight',xlim=c(3000,5500))
summary(subset_cars_equals_eight[,2])


#Problem 2
#да използваме функцията rnorm да генерираме 3 вектора: вектор а:mean= 0 и sd= 1, 
#b: mean = 0, sd=5, d: mean=0 sd=15; plot() and summary();за 10000 реда
a=rnorm(10000, mean=0, sd=1)
summary(a)
b=rnorm(10000,mean = 0,sd=5)
summary(b)
d=rnorm(10000,mean=0,sd=15)
summary(d)
plot(a)
plot(b)
plot(d)
