#Импортирайте данните Client data. Уверете се, че данните са технически коректни преди да започнете работа. Разделете извадката от клиенти на хомогенни групи като приложите k-means клъстеризация по характеристиките Credit.Card.Possession.Time и Credit.Card.Spendings. Разкажете историята на всеки клъстер.
install.packages('cluster')
library(cluster)

library(readxl)
Client_data <- read_excel(".../Client_data.xlsx")
View(Client_data)

# Cluster analysis
cor(Client_data[,3:4]) #слаба корелация между колоните

km = kmeans(scale(Client_data[,3:4]),centers = 4, nstart = 12, iter.max = 130)
plot(Client_data$Credit.Card.Spendings, Client_data$Credit.Card.Possession.Time, pch=19, col=km$cluster, xlab="Credit.Card.Spendings", ylab="Credit.Card.Possession.Time")
#червено - клиенти от кратко време, използват картата малко; ще се опитаме да ги задържим с оглед повече използване на картата в бъдеще
#синьо - клиенти от дълго време, използват картата малко; най-вероятно сме изгубили тези клиенти
#черно - използват картата средно независимо от колко време са клиенти; постоянно потребление, начини да ги накараме да увеличат потреблението си в бъдеще
#зелено - използват картата доста независимо от колко време са клиенти; outliers; тези от малко време, но използвали доста картата - можем да ги мотивираме да използват картата повече в бъдеще; другите - да запазим отношенията си с тях

km$size
km$cluster
km$centers
Client_data$cluster = km$cluster #създаваме колона в таблицата към кой клъстер принадлежи всеки
