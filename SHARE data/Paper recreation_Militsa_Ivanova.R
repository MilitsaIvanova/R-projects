#Cross-sectional differences in the level of depression for elderly people in Europe

#This script provides an implementation of the methodologies described in the paper titled "Cross-sectional differences in the level of depression for elderly people in Europe". The paper aims to construct data-driven indices reflecting the level of depression across different European countries. Utilizing the SHARE dataset, the analysis focuses on deriving latent variables through factor analysis and examining cross-national differences in depression among elderly people.

#Steps in the Implementation:

#- Data Loading and Preprocessing: Import and preprocess data from the SHARE project.
#- Factor Analysis: Perform factor analysis to derive latent variables.
#- Density Plots: Create density plots for the latent variables.
#- Cross-National Analysis: Analyze the mean and median values of the latent variables across different European countries using Google GeoCharts.
#- Visualization: Visualize the results through correlation matrices, scree plots, factor loadings, and GeoCharts.

#### First Video ####
#Loading libraries

library(foreign)
library(tibble)
library(dplyr)

#Import all data files

setwd("C:\\Users\\Militsa\\Documents\\uni\\Masters\\Data science\\SHARE\\wave 7 old")

#Perform bulk import
aux=list.files(path = "C:\\Users\\Militsa\\Documents\\uni\\Masters\\Data science\\SHARE\\wave 7 old") #create an auxiliary list of all the files that we want to import

aux[1:5] #The first 5 elements in the aux list of files

dd=lapply(aux,read.spss,to.data.frame=F,use.value.labels=T)

names(dd) #NULL

#give names to every module

aux2=gsub('sharew7_rel7-1-0_',"",aux)
aux2=gsub('.sav',"",aux2)
names(dd)=aux2

#save the list of imported data files
setwd("C:\\Users\\Militsa\\Documents\\uni\\Masters\\Data science\\SHARE")
save(dd,file="dd_old.RData")

### Data loading and Preprocessing ####

#Loading libraries
library(foreign)
library(tibble)
library(dplyr)
library(googleVis)
library(corrplot)
library(psych)
library(ggplot2)
library(gridExtra)
library(GPArotation)

#Load data (if it's not loaded)
setwd('C:\\Users\\Militsa\\Documents\\uni\\Masters\\Data science\\SHARE')
load('dd_old.RData')

#Make a list of required module names
nn=c('ep','dn','ac','mh','ph','co','gv_imputations')

#Get required data modules
dds=list() #This list contains data tables (tibbles) of selected modules
ddsc=list() #This list provides summary of information in selected modules

#Fill in the lists dds and ddsc
for (i in 1:length(nn)){
  dds[[i]]=as_tibble(dd[[nn[i]]])
  ddsc[[i]]=data.frame(var=names(dds[[i]]), varlab=attr(dds[[i]],"variable.labels"), varclass=sapply(dds[[i]], class), unqs=sapply(sapply(dds[[i]],unique),length),nas=colSums(is.na(dds[[i]])))
  dds[[i]]=as.data.frame(dd[[nn[i]]])
}

#assign names to list objects
names(dds)=nn
names(ddsc)=nn

#Look at the variables
view(ddsc$ac)

#Let's see how many obs. we have:
dim(dds$ac)

#Select variables with a lot of missing obs.
excl=c("hhid7","mergeidp7","coupleid7","language", as.character(ddsc$ac$var[grep("ac036_|ac037_|ac038_|ac740_",ddsc$ac$var)]))

#create a new df where the variables in excl are excluded
ac=dds$ac[,!names(dds$ac) %in% excl]
rm(excl)
sapply(ac, class)
#convert all factor variables in string
ac[,2:ncol(ac)]=sapply(ac[,2:ncol(ac)],as.character)
unique(ac$ac014_)

#Handle missing values

#if there are values 'don't know', 'refusal' replace it with NA (as char), otherwise keep the initial value
AC=as.data.frame(sapply(ac,function(x){ifelse(x=="Refusal"|x== "Don't know",NA,as.character(x))}),stringsAsFactors=F)

AC$ac012_=as.numeric(AC$ac012_)
rm(ac)
sapply(AC,class)

#Summarise variables' info

#create a new df with info about the count of missing values
ACs=data.frame(var=names(AC),nas=colSums(is.na(AC)))
ACs$nasp=round(ACs$nas/nrow(AC),2)*100 #missing values as a %

#Remove missing values
AC=na.omit(AC)

#Look at variables' unique values
ACs$unqs[4:nrow(ACs)]=sapply(AC[,4:ncol(AC)],unique)
ACs$nunqs=sapply(sapply(AC,unique),length)

#Code values numerically
#create a new df where the values will be numerically encoded
ACn=AC

#Selected; Not Selected

aux=ACs$var[which(ACs$nunqs==2)] #create an auxiliary list where the values are Selected: Not selected (nunqs==2)

ACn[,names(ACn) %in% aux]=sapply(ACn[,names(ACn) %in% aux],function(x){ifelse(x=="Selected",5,1)}) # we encode them to 1 and 5 because the maximum levels that we have is 5, so that the data belongs to the same scala 
rm(aux)

#Never, Rarely, Sometimes, Often encoding
aux=ACs$var[which(ACs$nunqs==4)]
ACn[,names(ACn) %in% aux]=sapply(ACn[,names(ACn) %in% aux],function(x){ifelse(x=="Never",1,ifelse(x=="Rarely",2,ifelse(x=="Sometimes",3,4)))})
rm(aux) 

#Agree strongly, Agree a little, Neither agree nor disagree, Disagree a little, Disagree strongly
aux=ACs$var[which(ACs$nunqs==5)]
ACn[,names(ACn) %in% aux]=sapply(ACn[,names(ACn) %in% aux], function(x){ifelse(x=="Disagree strongly",1,ifelse(x=="Disagree a little",2,ifelse(x=="Neither agree nor disagree",3,ifelse(x=="Agree a little",4,5))))})
rm(aux)

#Final checks
sapply(ACn,class)
colSums(is.na(ACn)) #no missing values

#Select variables for age, gender, marital status, children, education, total household income (thinc2), current job situation (cjs)
incl=c("mergeid","age","gender","single","mstat","yedu","cjs","thinc2")

D=dds$gv_imputations[,names(dds$gv_imputations) %in% incl]

sapply(D,class)
#convert the factor variables to string and 'age' column as numeric
D[,c(1:2,4,6:8)]=sapply(D[,c(1:2,4,6:8)],as.character)
D[,'age']=sapply(D[,'age'],as.character)
D[,'age']=sapply(D[,'age'],as.numeric)

DD=D %>% #then
  group_by(mergeid) %>% #then
  summarise(age=age[1],gender=gender[1],single=single[1],mstat=mstat[1],yedu=yedu[1],cjs=cjs[1],thinc2=thinc2[1])

#Remove respondents below 50
DD=DD[-which(DD$age<50),]
dim(DD)
#Select only respondents, who are employed or self-employed
DD <- DD %>% filter(cjs == "Employed or self-employed")
View(DD)
dim(DD)

#Merge data.frames
dd=merge(ACn,DD,by="mergeid")



### Correlation matrix and Scree plot ####


# Create activities_df with columns ac014_ to ac025_
View(dd)
activities_df <- dd[, 4:15]
sapply(activities_df,class)

# Generate the correlation matrix
cor_matrix <- cor(activities_df, use = "pairwise.complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, title = "Correlation Matrix", addCoef.col = 'black')

# Perform PCA to get the eigenvalues for the scree plot
pca_result <- principal(cor_matrix, nfactors = 12, rotate = "none")

# Extract eigenvalues
eigenvalues <- pca_result$values

# Create a scree plot
plot(eigenvalues, type = "b", main = "Scree Plot", 
     xlab = "Component Number", ylab = "Eigen values of components")
abline(h = 1, col = "red", lty = 2)


### Factor analysis ####

# Perform factor analysis with the optimal number of factors (m = 2) and oblique rotation (promax) to allow for correlated factors
optimal_factors <- 2
fa_result_optimal <- fa(activities_df, nfactors = optimal_factors, rotate = "promax")

# Extract the correlation between MR1 and MR2
factor_correlation <- fa_result_optimal$Phi

# Print the results
print(fa_result_optimal)
print(factor_correlation)


# Plot factor loadings with arrows between factors
fa.diagram(fa_result_optimal, simple = FALSE, errors = TRUE, cut = 0.3, digits = 1)

# Extract the factor scores
factor_scores <- factor.scores(activities_df, fa_result_optimal)
activities_df_with_scores <- cbind(activities_df, factor_scores$scores)

### Density of MR1 and MR2 ####

# Plot density of MR1
density_MR1 <- ggplot(activities_df_with_scores, aes(x = MR1)) +
  geom_density(color = "blue") +
  labs(title = "MR1", x = NULL, y = "Density") +
  theme_minimal()

# Plot density of MR2
density_MR2 <- ggplot(activities_df_with_scores, aes(x = MR2)) +
  geom_density(color = "blue") +
  labs(title = "MR2", x = NULL, y = "Density") +
  theme_minimal()

# Arrange the factor loadings diagram and density plots in a grid

grid.arrange(density_MR1, density_MR2, ncol = 1)

### Differences across countries in Europe ####

#Plot the difference across countries in Europe in terms of the mean and median values of MR1 and MR2

# Add the country column from dd to activities_df_with_scores
activities_df_with_scores$country <- dd[, 2]
View(activities_df_with_scores)


# Calculate the mean and median values of MR1 and MR2 by country
summary_stats <- activities_df_with_scores %>%
  group_by(country) %>%
  summarise(
    mean_MR1 = mean(MR1, na.rm = TRUE),
    median_MR1 = median(MR1, na.rm = TRUE),
    mean_MR2 = mean(MR2, na.rm = TRUE),
    median_MR2 = median(MR2, na.rm = TRUE)
  )

# Create GeoCharts for mean and median values of MR1 and MR2

# Mean MR1 by country
mean_MR1_chart <- gvisGeoChart(summary_stats, locationvar = "country", colorvar = "mean_MR1",
                               options = list(region = "150", displayMode = "regions", 
                                              colorAxis = "{colors:['#053061', '#2166ac', '#67a9cf', '#d1e5f0']}"),
                               chartid = "mean_MR1")

# Median MR1 by country
median_MR1_chart <- gvisGeoChart(summary_stats, locationvar = "country", colorvar = "median_MR1",
                                 options = list(region = "150", displayMode = "regions", 
                                                colorAxis = "{colors:['#053061', '#2166ac', '#67a9cf', '#d1e5f0']}"),
                                 chartid = "median_MR1")

# Mean MR2 by country
mean_MR2_chart <- gvisGeoChart(summary_stats, locationvar = "country", colorvar = "mean_MR2",
                               options = list(region = "150", displayMode = "regions", 
                                              colorAxis = "{colors:['#053061', '#2166ac', '#67a9cf', '#d1e5f0']}"),
                               chartid = "mean_MR2")

# Median MR2 by country
median_MR2_chart <- gvisGeoChart(summary_stats, locationvar = "country", colorvar = "median_MR2",
                                 options = list(region = "150", displayMode = "regions", 
                                                colorAxis = "{colors:['#053061', '#2166ac', '#67a9cf', '#d1e5f0']}"),
                                 chartid = "median_MR2")

# Plot the charts
plot(mean_MR1_chart)
plot(median_MR1_chart)
plot(mean_MR2_chart)
plot(median_MR2_chart)
