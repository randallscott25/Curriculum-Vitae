
ED = function(a,b)sqrt(sum((a-b)^2))


MD = function(a,b){
  d<-abs(a-b)
  d<-sum(d)
  return(d)
}


CS = function (a,b)a%*%b/sqrt(a%*%a*b%*%b)

a = c(22,1,42,10)
  
b = c(20,0,36,8)

#The Euclidean Distance  between them 
ED(a,b)
#The Manhattan Distance Between them 
MD(a,b
  )

#The cosine similiarity between them 
CS(a,b)
