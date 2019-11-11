#Randall Taylor IST 707
#datastoryteller
#HW 2

library(readr)
setwd("C:/Users/randa/Desktop/workingdirectory")
datastoryteller <- read_csv("datastoryteller.csv")
View(datastoryteller)

#What is the structure of the data

str(datastoryteller)

# Cleaning of the Data change the data type of school from Chr to factor

datastoryteller$School = factor(datastoryteller$School)
str(datastoryteller)
#change section from Numeric to factor
datastoryteller$Section = factor(datastoryteller$Section)
str(datastoryteller)
#Each of the remaining columns are discrete counts of students per category. It is not continuous, so "very ahead," "Middling","Behind","more behind", "very behind", should all be integers

datastoryteller$`Very Ahead +5`= as.integer(datastoryteller$`Very Ahead +5`)
datastoryteller$`Middling +0` = as.integer(datastoryteller$`Middling +0`)
datastoryteller$`Behind -1-5`= as.integer(datastoryteller$`Behind -1-5`)
datastoryteller$`More Behind -6-10`= as.integer(datastoryteller$`More Behind -6-10`)
datastoryteller$`Very Behind -11` = as.integer(datastoryteller$`Very Behind -11`)
datastoryteller$Completed = as.integer(datastoryteller$Completed)
#reorder columns to a more logical order. Completed shift to being ahead of "very ahead","middling" ,....

datastorytellerTEMP = datastoryteller[,c(2,1,8,3,4,5,6,7)]
View(datastorytellerTEMP)
datastoryteller = datastorytellerTEMP
#displaying top 5 rows
head(datastoryteller)
#Missing Data
View(datastoryteller)
#check for any missing values
sum(is.na(datastoryteller))
#There are no missing values in this data set 

#Visualizations 

# Creating a bar chart to show the number of sections from each school
SchoolValues=c(length(which(datastoryteller$School=='A')), length(which(datastoryteller$School=='B')), length(which(datastoryteller$School=='C')), length(which(datastoryteller$School=='D')), length(which(datastoryteller$School=='E')))

barplot(SchoolValues, names.arg = c('A', 'B', 'C', 'D', 'E'), main='Number of sections Per School')

#plotting completed totals by section 

barplot(datastoryteller$Completed, main = 'Completed Students / section', names.arg = c(1:30))

#summary of the completed student / section 
summary(datastoryteller$Completed[datastoryteller$Section])

#Boxplots of 'section' to 'completed' to visualize the above summary 
library(ggplot2)
ggplot(datastoryteller, aes(x = Section, y = Completed)) + geom_boxplot(
  
  color='blue',
  fill='blue',
  alpha=0.4,
  
  notch = TRUE,
  notchwidth = 0.8,
  
  outlier.colour = "red",
  outlier.fill = "red",
  outlier.size = 3
)
datastoryteller$`Very Behind -11`
#outlier row 30; show totals via rowSums to help understand what is going on with School E  
totalactive = rowSums(datastoryteller[,c("Completed","Middling +0","Behind -1-5","More Behind -6-10","Very Behind -11")])
taDS = data.frame(datastoryteller$School,totalactive)
View(taDS)
  
#Lets scatter plot and see if we have any patterns that emerge  
plot(datastoryteller$Completed)
#Linear Regression Model attempt to find a significant relationship between Completed and Section 
reg1 = lm(Completed~Section, data = datastoryteller)
summary(reg1)

with(datastoryteller,plot(Completed~Section))
abline(reg1)
plot(reg1)
  

#Any linear relationship between what section you are in, versus completed status? 
ex.1 = plot(datastoryteller$Completed, datastoryteller$Section)
#Lets scatter plot against the log of the data, to see if any curve occurs
plot.1= plot(datastoryteller$Completed, log(datastoryteller$Completed))

######
library(Rcmdr)

#Scatterplot by Group: Schools y = Very Behind, x = Completed 

names(datastoryteller) <- make.names(names(datastoryteller))
scatterplot(Very.Behind..11~Completed | School, regLine=FALSE, smooth=FALSE,
            boxplots=FALSE, by.groups=TRUE, data=datastoryteller)


scatterplot(Very.Behind..11~Completed | School, regLine=TRUE, smooth=FALSE,
            boxplots=TRUE, by.groups=TRUE, data=datastoryteller)
  
#Trying to estabilsh a fit model, none to be found.  
LinearModel.1 <- lm(Very.Behind..11 ~ Completed, data=datastoryteller)
summary(LinearModel.1)

GLM.2 <- glm(School ~ Completed, family=binomial(logit), 
             data=datastoryteller)
summary(GLM.2)
exp(coef(GLM.2))  # Exponentiated coefficients ("odds ratios")
library(nnet, pos=19)
MLM.3 <- multinom(School ~ Very.Behind..11, data=datastoryteller, 
                  trace=FALSE)
summary(MLM.3, cor=FALSE, Wald=TRUE)
cor(datastoryteller[,c("Completed","Very.Behind..11")], use="complete")











     