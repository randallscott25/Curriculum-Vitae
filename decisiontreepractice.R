#Randall Taylor 



Library("RWeka")
#Sys.setenv(JAVA_HOME=”C:\Program Files\Java\jdk1.8.0_51\jre”)

tTrain = read.csv("Titanic_Training_Data.csv")
tTest = read.csv("Titanic_Testing_Data.csv")

#Preprocess DATA in R
#Tranform dat type 
# Numeric = Nominal 

NN = make_Weka_filter("weka/filters/unsupervised/attribute/NumericToNominal") #BUild a function using RWeka filter INterfact
#now apply NN function to training and test datasets

trainset = NN(data=tTrain, control=Weka_control(R="1-3"), na.action = NULL)
testset = NN(data=tTest, control = Weka_control(R="1,3"),na.action = NULL)

#Deal with the missing values
MS = make_Weka_filter("weka/filters/unsupervised/attribute/ReplaceMissingValues") #build a function to both training and test datasets

#Now Apply the filter MS function to both training and test datasets

trainset = MS(data = trainset, na.action = NULL)
testset = MS(data = testset, na.action = NULL)


#check data definition
str(trainset)

#Step 4 Apply J48 Algorithm

m=J48(Survived~., data = trainset)
m=J48(Survived~., data = trainset, control = Weka_control(U=FALSE, M=2, C=0.5))

#View the Parameters with function WOW

WOW("J48")

#Use 10 fold cross-validation to evalute the model 

e = evaluate_Weka_classifier(m, numFolds = 10, seed =1, class = TRUE)
e
#Apply the TRAIN model with TEST data (remember Train is in variable m)

pred = predict(m, newdata=testset, type=c("class"))

write.csv(pred,file = "predict.csv")
pred

#REmoving irrelevant attributes:

myVars = c("Pclass","Sex","Age","SibSp","Parch","Fare","Survived")
newtrain=trainset[myVars]
newtest=testset[myVars]

m=J48(Survived~., data = newtrain)
m=J48(Survived ~., data = newtrain, control = Weka_control(U=FALSE,M=2,C=0.5))
e=evaluate_Weka_classifier(m, seed = 1, numFolds = 10)
pred=predict(m,newdata = newtest, type =c("class"))
myids = c("PassengerId")
id_col=testset[myids]
newpred=cbind(id_col, pred)
colnames(newpred)=c("Passengerid","Survived")
View(newpred)


write.csv(newpred, file ="RWekaPrediction", row.names = FALSE)

InfoGainAttributeEval(Survived~., data = trainset)
library(partykit)
plot.party(newpred)
if(require("partykit", quietly = TRUE)) plot(m)
