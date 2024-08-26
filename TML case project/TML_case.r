# Final project;

####Project Part 1 (Предварителна обработка на данни)####
#Import dataset
library(readxl)
TML_first_sheet <- read_excel("uni/Masters/Предварителна обработка на данни/TML_vistors_case_study_data.xlsx", sheet = 1)
TML_second_sheet <- read_excel("uni/Masters/Предварителна обработка на данни/TML_vistors_case_study_data.xlsx", sheet = 2)
TML_third_sheet <- read_excel("uni/Masters/Предварителна обработка на данни/TML_vistors_case_study_data.xlsx", sheet = 3)
View(TML_first_sheet)
View(TML_second_sheet)
View(TML_third_sheet)

#Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(cluster)

#Remove staff rows in the third sheet
#Решение според условието
#staff = TML_third_sheet = filter(TML_third_sheet, TML_third_sheet$`Брой визити до издаване на лична гривна`==0)
#TML_third_sheet = filter(TML_third_sheet, TML_third_sheet$`Брой визити до издаване на лична гривна`!=0)

#Решение според информацията, дадена от Алексей Потебня
TML_third_sheet_filtered <- read_excel("uni/Masters/Предварителна обработка на данни/TML_vistors_case_study_data.xlsx", sheet = 4)

#Find the 'staff' members
different_usernames <- TML_third_sheet$USERNAME[!(TML_third_sheet$USERNAME %in% TML_third_sheet_filtered$USERNAME)]
print(different_usernames)

#Now let's remove the 'staff' rows in all the sheets not only in the third one
filtered_sheet1 <- TML_first_sheet[!(TML_first_sheet$USERNAME %in% different_usernames), ]
filtered_sheet2 <- TML_second_sheet[!(TML_second_sheet$USERNAME %in% different_usernames), ]

#Handle missing values in the first sheet
filtered_sheet1 <- mutate_all(filtered_sheet1, funs(replace(., is.na(.), 0))) #replaces the N/A values in all of the columns with 0
filtered_sheet1 <- filtered_sheet1 %>%  mutate(`Дата на излизане` = ifelse(`Дата на излизане` == 0, `Дата на влизане`, `Дата на излизане`))

zeros = filter(filtered_sheet1, filtered_sheet1$`Дата на излизане`==0)
rm(zeros)

#Handle missing values in the second sheet
filtered_sheet2 <- filtered_sheet2 %>%  mutate(`Край на игра` = ifelse(is.na(`Край на игра`), `Начало на игра`, `Край на игра`))
zeros = filter(filtered_sheet2, is.na(filtered_sheet2$`Край на игра`))
rm(zeros)
filtered_sheet2 = filtered_sheet2 %>% mutate(`Продължителност минути` = ifelse(is.na(`Продължителност минути`),0, `Продължителност минути`))

#From the third sheet we are going to use only column "Гривна ниво" as the other columns contain incomplete or wrongly filtered information. Column “Поредна гривна” contains data about the bracelet batch number, which doesn't serve a purpose for our project. Column “Брой визити до издаване на лична гривна” isn’t the same as column “Брой визити общо”. Also, column  “Общи точки на посетителя в момента” shows us the current points on the bracelet and not the whole points from all experiments, collected during every visit. 

#Checking if there are any missing values left
any(is.na(filtered_sheet1))
any(is.na(filtered_sheet2))

#Identify the customers who have more than 1 visit
#First we filter the dataframe by Date and Username
filtered_sheet1 <- filtered_sheet1 %>%
  distinct(USERNAME, Дата, .keep_all = TRUE)

#Filter the valid visitors (their visit is less than 4 hours and more than 1 hour long)
sheet1_valid <- filtered_sheet1 %>%
  mutate(time_per_visit = difftime(`Дата на излизане`, `Дата на влизане`, units = "hours"))

sheet1_valid <- sheet1_valid %>%
  filter(time_per_visit < 4)

sheet1_valid <- sheet1_valid %>%
  filter(time_per_visit > 1)

#Then find the number of visits for each user 
username_counts <- sheet1_valid %>%
  group_by(USERNAME) %>%
  summarise(count = n())

#Filter the users with more than 1 visit
customers <- username_counts %>%
  filter(count > 1)

# Let's find the customers that have stayed for less than an hour and have more than 1 visits
sheet1_valid <- filtered_sheet1 %>%
  mutate(time_per_visit = difftime(`Дата на излизане`, `Дата на влизане`, units = "hours"))

sheet1_valid <- sheet1_valid %>%
  filter(time_per_visit < 1)

username_counts <- filtered_sheet1 %>%
  group_by(USERNAME) %>%
  summarise(count = n())

all_customers <- username_counts %>%
  filter(count > 1)

#Interesting observation: If we filter by "time per visit" (columns "Дата на влизане" and "Дата на излизане"), we can find out that people who have their first visit for less than 1 hour, tend to come back. An example of this unusual customer behavior is the username "ivan2009". That's why we decide to work with the customers who are not filtered by time, as they may present interesting information in our analysis.

#Consequently we decide to filter the valid customers who have stayed within 4 hours and use this sample for our analysis

sheet1_valid <- filtered_sheet1 %>%
  mutate(time_per_visit = difftime(`Дата на излизане`, `Дата на влизане`, units = "hours"))

sheet1_valid <- sheet1_valid %>%
  filter(time_per_visit < 4)

username_counts <- filtered_sheet1 %>%
  group_by(USERNAME) %>%
  summarise(count = n())

all_customers <- username_counts %>%
  filter(count > 1)

#Preferred experiments for these customers and the avg time
#Filter the second sheet by all_customers (customers with more than 1 visit)
filtered_rows <- filtered_sheet2 %>%
  filter(USERNAME %in% all_customers$USERNAME)

#Find the count of each experiment in the table 'filtered_rows'
experiments_counts <- filtered_rows %>%
  count(Експонат, name = "count")

ggplot(experiments_counts, aes(x = Експонат, y = count)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Брой посещения на всеки Експонат", x = "Експонат", y = "Брой посещения")

#Average time for each experiment
#Calculate the avg time of each experiment
average_time_per_experiment <- filtered_rows %>%
  group_by(Експонат) %>%
  summarise(average_time = mean(`Продължителност минути`))

#Visualize it with graphs and plots
ggplot(average_time_per_experiment, aes(x = Експонат, y = average_time)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Средно време на всеки Експонат", x = "Експонат", y = "Средно време (в минути)")

#There are some experiments where the duration per attendee is more then 4 hours, but we assume this is irrelevant. We filter the column by experiments, whose duration does not exceed 60 minutes, as we suppose there will be change in trying out of experiments. 

#For example: We noticed that there is one customer with too much time spent on the experiment "Опити с тънка сапунена ципа" which corrupts the overall results. That's why we decide to remove it as this is some kind of anomaly or mistake.

max_time <- filtered_rows %>%
  filter(Експонат == "Опити с тънка сапунена ципа") %>%
  summarise(max_time = max(`Продължителност минути`)) %>%
  pull(max_time)

max_time <- filtered_rows %>%
  filter(Експонат == "Как се получават цветовете?") %>%
  summarise(max_time = max(`Продължителност минути`)) %>%
  pull(max_time)

filtered_rows <- filtered_rows %>%
  filter(`Продължителност минути` < 60)

#Average time for each experiment
#Calculate the avg time of each experiment
average_time_per_experiment <- filtered_rows %>%
  group_by(Експонат) %>%
  summarise(average_time = mean(`Продължителност минути`))

#Visualize it with graphs and plots
ggplot(average_time_per_experiment, aes(x = Експонат, y = average_time)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Средно време на всеки Експонат", x = "Експонат", y = "Средно време (в минути)")

#Boxplot of a specific experiment
ggplot(filtered_rows, aes(x = Експонат['Как се получават цветовете?'], y = `Продължителност минути`)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Boxplot за експеримент: 'Как се получават цветовете?'", x = "Експеримент", y = "Продължителност (в минути)")

#Calculate the correlation matrix
#Create a new column "visited" with values = 1
filtered_rows <- mutate(filtered_rows, visited = 1)

#Create a new table where experiments are columns, and rows represent visits
result_table <- filtered_rows %>%
  select(Експонат, USERNAME, visited) %>%
  distinct() %>%
  pivot_wider(names_from = "Експонат", values_from = 'visited', values_fill = 0)

correlation_matrix <- cor(result_table[, !grepl("^USERNAME$", names(result_table))])

#Plotting the results on a heatmap
# Plot the correlation matrix using heatmap
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(20),
        main = "Correlation Heatmap",
        xlab = "",
        ylab = "",
        symm = TRUE,  # Display only the lower triangle
        Rowv = NA,   # Remove row dendrogram
        Colv = NA,# Remove column dendrogram
)

#Filter only the customers with level of bracelet "Начинаещ"
beginners_only <- TML_third_sheet_filtered[TML_third_sheet_filtered$`Гривна ниво` == "Начинаещ", ]

#Create new tables only with beginners
beginners_1 <- filtered_sheet1 %>%
  filter(USERNAME %in% beginners_only$USERNAME)

beginners_2 <- filtered_sheet2 %>%
  filter(USERNAME %in% beginners_only$USERNAME)

beginners_3 <- TML_third_sheet_filtered %>%
  filter(USERNAME %in% beginners_only$USERNAME)

#Count the number of visits
visits_beginners <- table(beginners_1$USERNAME)
visits_beginners <- as.data.frame(visits_beginners)
colnames(visits_beginners)[colnames(visits_beginners) == "Var1"] <- "USERNAME"
colnames(visits_beginners)[colnames(visits_beginners) == "Freq"] <- "Брой посещения"

#Count the avg time for every beginner
avg_time <- beginners_2 %>%
  group_by(USERNAME) %>%
  summarise(`Средно време на човек` = mean(`Продължителност минути`, na.rm = TRUE))

#Count the avg points for every beginner
avg_points <- beginners_1 %>%
  group_by(USERNAME) %>%
  summarise(`Среден брой точки на човек` = mean(`Точки за посещението`, na.rm = TRUE))

#Count the experiments per every customer
experiments_per_customer <- table(beginners_2$USERNAME)
experiments_per_customer <- as.data.frame(experiments_per_customer)
colnames(experiments_per_customer)[colnames(experiments_per_customer) == "Var1"] <- "USERNAME"
colnames(experiments_per_customer)[colnames(experiments_per_customer) == "Freq"] <- "Брой направени експерименти"

#Merge everything in one table
merged_table <- merge(avg_points, avg_time, by = "USERNAME", all = TRUE)
merged_table <- merge(merge(avg_points, avg_time, by = "USERNAME", all = TRUE), visits_beginners, by = "USERNAME", all = TRUE)
merged_table <- merge(merge(merge(avg_points, avg_time, by = "USERNAME", all = TRUE), visits_beginners, by = "USERNAME", all = TRUE), experiments_per_customer, by = "USERNAME")

#Make the columns numeric
merged_table$`Среден брой точки на човек` <- as.numeric(merged_table$`Среден брой точки на човек`)
merged_table$`Средно време на човек` <- as.numeric(merged_table$`Средно време на човек`)
merged_table$`Брой посещения` <- as.numeric(merged_table$`Брой посещения`)
merged_table$`Брой направени експерименти` <- as.numeric(merged_table$`Брой направени експерименти`)

#Cluster analysis
km = kmeans(scale(merged_table[,2:3]),centers = 5, nstart = 12, iter.max = 130)
plot(merged_table$`Среден брой точки на човек`, merged_table$`Средно време на човек`, pch=19, col=km$cluster, xlab="Среден брой точки на човек", ylab="Средно време на човек (в минути)")
#We make 5 clusters. The first cluster includes people with 30 points average, who spent up to 2-3 minutes average on the experiments. The second cluster includes people with 30 to 70 points average, who also spent up to 2-3 minutes average on the experiments. The third cluster includes several people with 85 to 125 points average, who spent up to 2-3 minutes average on the experiments. The fourth cluster includes two people with 26 and 42 points average, who spent about 20 minutes average on the experiments. The fifth cluster includes two people with about 20 points average, who spent about 60 minutes average on the experiments. We can conclude that almost all people spend up to 2-3 minutes average on the experiments, and their average points don't depend on this. Although there are two people who spent about an hour on the experiments, don't have high points. The majority of people have a maximum of 70 points average.

km = kmeans(scale(merged_table[,2:4]),centers = 5, nstart = 12, iter.max = 130)
plot(merged_table$`Среден брой точки на човек`, merged_table$`Брой посещения`, pch=19, col=km$cluster, xlab="Среден брой точки на човек", ylab="Брой посещения")
#We make 5 clusters. The first cluster includes people with up to 45 points average, who have one visit. The second cluster includes people with 45 to 120 points average, who have mostly one visit. The third cluster includes people with 10 to 50 points average, who have two visits. The fourth cluster includes people with 10 to 40 points average, who have 3,4 or 5 visits. The fifth cluster includes only two people with about 20 points average, who have one visit. We can conclude that regardless of the number of visits most people have up to 60 points average. High scorers have one visit because they decide they can't improve their score. People with not very high points are divided into two groups - those who gave up after the first visit and those who have a few more visits probably to improve their score.

km = kmeans(scale(merged_table[,2:5]),centers = 4, nstart = 12, iter.max = 130)
plot(merged_table$`Среден брой точки на човек`, merged_table$`Брой направени експерименти`, pch=19, col=km$cluster, xlab="Среден брой точки на човек", ylab="Брой направени експерименти")
#We make 4 clusters. The first cluster includes people with up to 45 points average and with up to 50 made experiments. The second cluster includes people with 40 to 120 points average and with 25 to 100 made experiments. The third cluster includes people with 30 to 60 points average and with 50 to 220 made experiments. The fourth cluster includes only two people with about 20 points average and with 22 and 32 made experiments. We can conclude that more than half of the people have made not many experiments and also have low score. But we can't say that people with many experiments done have high score. There are only about 10 people who have done over 100 experiments. We can't find high dependence between "Average points per human" and "Experiments done".


####Project part 2 (Машинно обучение)####

#Load the data about the weather conditions
dd <- read_excel("uni/Masters/Предварителна обработка на данни/Финален проект/weather data 1.xlsx")


# Processing the data in the weather table
# Отпечатване на резултатаprint(temperature_celsius)

dd$`Temperature` <- (dd$`Temperature (°F)` - 32) * 5 / 9
dd$Temperature <- round(dd$`Temperature`,1)

dd$`Raining` <-  ifelse(dd$`Humidity (%)` > 60,1,0)

dd$`Wind` <-  ifelse(dd$`Wind Speed (mph)` > 9,1,0)

#Create a new table based on 'sheet1_valid' with all the unique dates when TML was open

result_date <- sheet1_valid %>% group_by(`Дата`) %>% summarise(Count = n())

# Filter the rows in 'dd' with dates that match 'result_date'
filtered_dd <- dd %>%  filter(Time %in% result_date$`Дата`)
# Select the 'wind', 'raining', 'time', 'temperature' columns from 'dd'
filtered_dd <- filtered_dd %>%  select(Time ,Wind, Raining, Temperature)

result_date <- merge(result_date, filtered_dd, by.x = "Дата", by.y = "Time", all.x = TRUE)


#Changing the data type of column Data
class(result_date$Дата)
result_date$Дата <- as.Date(result_date$Дата)

library(lubridate)

#Finding Vacation days 

result_date$Vacation <- 0

vacation_periods <- c("2018-04-01" = "2018-04-09",
                      "2018-05-21" = "2018-05-21",
                      "2018-05-23" = "2018-05-23",
                      "2018-05-24" = "2018-05-24",
                      "2018-11-02" = "2018-11-04",
                      "2018-12-22" = "2019-01-02",
                      "2018-06-16" = "2018-09-14",
                      "2019-02-05" = "2019-02-05",
                      "2019-03-30" = "2019-04-07",
                      "2019-05-21" = "2019-05-21",
                      "2019-05-23" = "2019-05-23",
                      "2019-05-24" = "2019-05-24",
                      "2019-11-01" = "2019-11-03",
                      "2019-06-16" = "2019-09-14",
                      "2019-12-21" = "2020-01-05",
                      "2020-02-05" = "2020-02-05")

# Function to check if a date is in any vacation period
is_vacation <- function(date) {
  for (start_date in names(vacation_periods)) {
    end_date <- vacation_periods[start_date]
    if (date >= ymd(start_date) & date <= ymd(end_date)) {
      return(1)
    }
  }
  return(0)
}

# Apply the function to each date in the dataframe
result_date$Vacations <- sapply(result_date$Дата, is_vacation)


# Sum the values in the 'Vacations' column to get the total number of vacation days
total_vacation_days <- sum(result_date$Vacations)

# Print the total number of vacation days
total_vacation_days


#Finding weekend days

# Create a new column 'weekend' and initialize it with 0
result_date$weekend <- 0 
# Use lubridate's wday() function to identify weekends and assign 1 to 'weekend' column
result_date$weekend[wday(result_date$Дата, label = TRUE) %in% c("Sat", "Sun")] <- 1

# Sum the values in the 'weekend' column to get the total number of weekend days
total_weekend_days <- sum(result_date$weekend)

# Print the total number of vacation days
total_weekend_days

#Finding each season

# Create new columns for each season and initialize them with 0
result_date$Spring <- 0
result_date$Summer <- 0
result_date$Autumn <- 0
result_date$Winter <- 0

# Use lubridate's functions to determine the season and assign 1 to the corresponding column
result_date$Spring[month(result_date$Дата) %in% c(3, 4, 5)] <- 1  # March, April, May
result_date$Summer[month(result_date$Дата) %in% c(6, 7, 8)] <- 1  # June, July, August
result_date$Autumn[month(result_date$Дата) %in% c(9, 10, 11)] <- 1  # September, October, November
result_date$Winter[month(result_date$Дата) %in% c(12, 1, 2)] <- 1  # December, January, February

total_summer_days <- sum(result_date$Summer)
total_spring_days <- sum(result_date$Spring)
total_winter_days <- sum(result_date$Winter)
total_autumn_days <- sum(result_date$Autumn)

#Plot the sum of the days for each season
season_days <- data.frame(
  Season = c("Spring", "Summer", "Autumn", "Winter"),
  TotalDays = c(total_spring_days, total_summer_days, total_autumn_days, total_winter_days)
)

ggplot(season_days, aes(x = Season, y = TotalDays, fill = Season)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = TotalDays), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  labs(title = "Общ брой дни по сезони", x = "Сезон", y = "Общ брой дни") +
  scale_fill_brewer(palette = "Pastel1") 

#From table "sheet1_valid" choose only the kids and adults
sheet1_valid_filtered <- sheet1_valid %>%
  filter(`Роля` %in% c("Дете", "Възрастен"))

#Count the number of trying of experiments per each date
kids_adults <- sheet1_valid_filtered %>%
  group_by(`Дата`, `Роля`) %>%
  summarize(Count = n())

#Create a new table with separate columns
spread_result <- spread(kids_adults, `Роля`, Count, fill = 0)

#Merge the results
result_date <- merge(result_date, spread_result, by = "Дата", all.x = TRUE)


#Correlation Heat Map

library(corrplot)
cor_matrix <- cor(result_date[, sapply(result_date, is.numeric)])


# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         addCoef.col = "black",
         tl.col = "black", tl.cex = 0.6, tl.srt = 45)


#Plotting

summary(result_date$Count)
ggplot(result_date, aes(y = Count)) + 
  geom_boxplot(fill = "#69b3a2", color = "#1b9e77") + 
  labs(title = "Разпределение на броя посещения",
       y = "Брой посещения",
       x = "") +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank())  

#Important observation: The day on which there are the most visitors (337) is the birthday of TML

summary(result_date$Възрастен)
boxplot(result_date$Възрастен)

summary(result_date$Дете)
boxplot(result_date$Дете)

result_date_long <- tidyr::pivot_longer(result_date, 
                                        cols = c(Възрастен, Дете), 
                                        names_to = "Група", 
                                        values_to = "Брой")

# Създаване на сравнителен boxplot
ggplot(result_date_long, aes(x = Група, y = Брой, fill = Група)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Възрастен" = "#FFA07A", "Дете" = "#20B2AA")) +
  labs(title = "Сравнение на броя между възрастни и деца",
       x = "Група",
       y = "Брой посещения") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank())

#Modelling

#Linear Regression

# Fit a linear model with 'Count' as the dependent variable
model <- lm(Count ~ Wind + Raining + Temperature + Autumn+ weekend + Spring + Summer  +Winter  + Vacations , data = result_date)
# Get a summary of the model
summary(model)
plot(model)

# Fit a linear model with 'Възрастен' as the dependent variable
model_adults <- lm(Възрастен ~ Wind + Raining + Temperature + Autumn+ Spring + Summer  +Winter, data = result_date)
# Get a summary of the model
summary(model_adults)
plot(model_adults)

# Fit a linear model with 'Дете' as the dependent variable
model_kids <- lm(Дете ~ Wind + Raining + Temperature + Autumn + Spring + Summer  +Winter, data = result_date)
# Get a summary of the model
summary(model_kids)
plot(model_kids)
#!Important observation: The adults are easier to model and predict than the kids

#Random forest
install.packages("randomForest")
library(randomForest)



#'Count' is the target variable
rf_model <- randomForest(Count ~ . -Дата -Дете -Възрастен, data = result_date, importance = TRUE, ntree = 500)

# Check variable importance
importance(rf_model)

varImpPlot(rf_model)

plot(predict(rf_model), result_date$Count)

# Predict on the same dataset (for demonstration; ideally, use a test set)
predictions <- predict(rf_model, result_date)

# Calculate the mean squared error (MSE) 
mse <- mean((predictions - result_date$Count)^2)
print(mse)


#'Възрастен' is the target variable
rf_model_adults <- randomForest(Възрастен ~ . -Дата -Дете -Count -weekend -Vacations, data = result_date, importance = TRUE, ntree = 500)

# Check variable importance
importance(rf_model_adults)

varImpPlot(rf_model_adults)

plot(predict(rf_model_adults), result_date$Възрастен)

# Predict on the same dataset (for demonstration; ideally, use a test set)
predictions <- predict(rf_model_adults, result_date)

# Calculate the mean squared error (MSE) 
mse <- mean((predictions - result_date$Възрастен)^2)
print(mse)

#'Дете' is the target variable
rf_model_kids <- randomForest(Дете ~ . -Дата -Възрастен -Count -weekend -Vacations, data = result_date, importance = TRUE, ntree = 500)

# Check variable importance
importance(rf_model_kids)

varImpPlot(rf_model_kids)

plot(predict(rf_model_kids), result_date$Дете)

# Predict on the same dataset (for demonstration; ideally, use a test set)
predictions <- predict(rf_model_kids, result_date)

# Calculate the mean squared error (MSE) 
mse <- mean((predictions - result_date$Дете)^2)
print(mse)

#!Important observation: The kids probably visit TML with their parents in the summer and with their school/teacher/classmates during the school year (in the other seasons)

