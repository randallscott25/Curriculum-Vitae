#Randall     
#kMeans
#load in data 

setwd("C:/Users/randa/Desktop/workingdirectory")
zoo = read.csv("zoo.csv")
str(zoo)
#remove columns "names" and "Type"
zoo_unlabel = zoo[,c(2:17)]
str(zoo_unlabel)

#build kMeans Model using RWeka
library(RWeka)
modelrweka = SimpleKMeans (zoo_unlabel, control =Weka_control(N = 7, I = 500, S =100))
modelrweka
#This is Weka but it is not good. Use Built-in R functions instead

model_r = kmeans(zoo_unlabel, 7)
model_r

#print the centroids

model_r$centers

##get cluster assignments
cluster_assignments = data.frame(zoo, model_r$cluster)
View(cluster_assignments)
# visualize animal types and clusters by soecific features, red = milk YES blac = milk NO
plot(zoo$type ~ jitter(model_r$cluster, 1), pch=21,col=as.factor(zoo$milk))

##use PCA in visualization package 'cluster' to visualize kMeans model. PCA is principal components analysis 

#install.packages("cluster")
library(cluster)
clusplot(zoo_unlabel,model_r$cluster, color = TRUE, shade = TRUE, labels=2, lines=0)

#use R's HAC algorithm, which uses Euclidean distance and complete linkage by default. 
  
d = dist(as.matrix(zoo_unlabel))
hc=hclust(d)
plot(hc)

f = read.csv("iris.csv")

e = dist(as.matrix(f))
hc1=hclust(e)
plot(hc1)

#install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(zoo_unlabel$hair,zoo_unlabel$feathers, angle = 25, type = 'h')

demo = hclust(di)